package io.github.nicolasfara.rstmanager.service.journalcheck

import edomata.core.DomainModel
import fs2.Stream
import io.circe.Decoder
import io.circe.parser.parse

/** A stored row: the stream it belongs to, its position (version in the stream, or outbox sequence number), and its raw payload. */
final case class StoredRow(stream: String, position: Long, payload: String)

/** A row the current release cannot read back, and why. */
final case class RowFailure(stream: String, position: Long, reason: String)

/** The outcome of checking one table. */
final case class RowReport(rows: Long, streams: Long, failures: Vector[RowFailure]):
  def ok: Boolean = failures.isEmpty

/** How the rows of one table are checked against the current release. */
trait RowCheck:
  /** Checks the rows of a table, which arrive grouped by stream and in position order. */
  def run[F[_]](rows: Stream[F, StoredRow]): Stream[F, RowReport]

object RowCheck:
  /**
   * Replays a journal the way the backend loads an aggregate: every row is decoded with the journal codec and folded, in version order, into the
   * state of its stream. A stream stops folding at its first failure, as the backend cannot load it past that point, but its later rows are still
   * decoded.
   */
  def journal[S, E, R](model: DomainModel[S, E, R], decoder: Decoder[E]): RowCheck = new RowCheck:
    def run[F[_]](rows: Stream[F, StoredRow]): Stream[F, RowReport] =
      rows.fold(Replay[S](RowReport(0, 0, Vector.empty), None, None))(step).map(_.report)

    private def step(replay: Replay[S], row: StoredRow): Replay[S] =
      val newStream = !replay.stream.contains(row.stream)
      val state = if newStream then Some(model.initial) else replay.state
      val counted = replay.count(row, newStream)
      decode(decoder, row) match
        case Left(reason) => counted.copy(state = None).fail(row, reason)
        case Right(event) =>
          state.fold(counted.copy(state = None)) { current =>
            model
              .transition(event)(current)
              .fold(
                rejections => counted.copy(state = None).fail(row, s"rejected by the aggregate: ${rejections.toNonEmptyList.toList.mkString(", ")}"),
                next => counted.copy(state = Some(next)),
              )
          }

  /** Decodes pending outbox rows, which the outbox publisher reads back but never folds. */
  def outbox[N](decoder: Decoder[N]): RowCheck = new RowCheck:
    def run[F[_]](rows: Stream[F, StoredRow]): Stream[F, RowReport] =
      rows
        .fold(Replay[Unit](RowReport(0, 0, Vector.empty), None, None)) { (replay, row) =>
          val counted = replay.count(row, !replay.stream.contains(row.stream))
          decode(decoder, row).fold(counted.fail(row, _), _ => counted)
        }
        .map(_.report)

  private def decode[A](decoder: Decoder[A], row: StoredRow): Either[String, A] =
    parse(row.payload).flatMap(decoder.decodeJson).left.map(error => s"cannot be decoded: ${error.getMessage}")

  /** The running report, plus the stream being read and its state (`None` once it can no longer be folded). */
  private final case class Replay[S](report: RowReport, stream: Option[String], state: Option[S]):
    def count(row: StoredRow, newStream: Boolean): Replay[S] =
      copy(report = report.copy(rows = report.rows + 1, streams = report.streams + (if newStream then 1 else 0)), stream = Some(row.stream))

    def fail(row: StoredRow, reason: String): Replay[S] =
      copy(report = report.copy(failures = report.failures :+ RowFailure(row.stream, row.position, reason)))
end RowCheck
