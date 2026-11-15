package io.github.nicolasfara.rstmanager.service.registry

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.{ EventMessage, EventMetadata }
import edomata.backend.eventsourcing.{ AggregateState, SnapshotStore }
import edomata.core.ModelTC
import edomata.skunk.BackendCodec
import skunk.*
import skunk.codec.all.*
import skunk.data.Arr
import skunk.implicits.*

/**
 * Batched, cached read side of one edomata journal namespace.
 *
 * edomata's `repository.get` reads one stream per round trip and never caches what it folds, so listing N entities costs N queries, each replaying
 * its whole stream. [[getAll]] loads any number of streams in one query and keeps the folded states in an LRU cache of its own, so a later read only
 * fetches the events appended since. Every read still asks the journal for newer events, so a cached state is a starting point, never an answer.
 *
 * The cache is not shared with the command handler on purpose: edomata trusts the states in its own cache without checking the journal, so a state
 * folded here and racing a command could make every retry of that command decide on a stale version.
 */
final class StreamReader[S, E, R] private (
    pool: Resource[IO, Session[IO]],
    newEvents: Query[(Arr[String], Arr[Long]), EventMessage[E]],
    cache: SnapshotStore[IO, S],
)(using model: ModelTC[S, E, R]):

  /** The current state of stream `id` (the initial state when the stream has no events). */
  def get(id: String): IO[AggregateState[S, E, R]] =
    getAll(List(id)).map(_.getOrElse(id, AggregateState.Valid(model.initial, 0L)))

  /** The current state of every stream in `ids`, read in a single query. */
  def getAll(ids: List[String]): IO[Map[String, AggregateState[S, E, R]]] =
    val streams = ids.distinct
    if streams.isEmpty then IO.pure(Map.empty)
    else
      for
        cached <- streams.traverse(id => cache.get(id).map(id -> _.getOrElse(AggregateState.Valid[S](model.initial, 0L): AggregateState.Valid[S])))
        // A state of version n folded the events 0..n-1, so the events still to fold are those of version n onwards.
        events <- pool.use(_.execute(newEvents)((Arr(streams*), Arr(cached.map(_._2.version)*))))
        byStream = events.groupMap(_.metadata.stream)(identity)
        folded = cached.map((id, last) => id -> fold(last, byStream.getOrElse(id, Nil)))
        _ <- folded.traverse_ {
          case (id, AggregateState.Valid(state, version)) => cache.put(id, AggregateState.Valid(state, version))
          case _ => IO.unit
        }
      yield folded.toMap
    end if
  end getAll

  /** The valid state of each of `ids` (addressed by `stream`), read in a single query: conflicted streams are left out. */
  def valid[K](ids: List[K])(stream: K => String): IO[Map[K, S]] =
    getAll(ids.map(stream)).map { states =>
      ids.flatMap(id => states.get(stream(id)).collect { case AggregateState.Valid(state, _) => id -> state }).toMap
    }

  /** Folds `events` (in version order) onto `last` like edomata does: the first rejected event turns the state into a conflict. */
  private def fold(last: AggregateState.Valid[S], events: List[EventMessage[E]]): AggregateState[S, E, R] =
    events.foldLeft[AggregateState[S, E, R]](last) {
      case (AggregateState.Valid(state, version), event) =>
        model.transition(event.payload)(state).fold(AggregateState.Conflicted(state, event, _), AggregateState.Valid(_, version + 1))
      case (conflicted, _) => conflicted
    }
end StreamReader

object StreamReader:
  /** Cache size for the per-entity namespaces: far above today's entity counts, and a folded aggregate weighs a few kilobytes at most. */
  val entityCacheSize: Int = 10_000

  /** A reader of the journal in schema `namespace` (the namespace given to `SkunkDriver`), caching up to `cacheSize` folded streams. */
  def apply[S, E, R](namespace: String, pool: Resource[IO, Session[IO]], cacheSize: Int)(using
      model: ModelTC[S, E, R],
      codec: BackendCodec[E],
  ): IO[StreamReader[S, E, R]] =
    SnapshotStore.inMem[IO, S](cacheSize).map(new StreamReader(pool, newEvents(namespace, codec), _))

  /** The events of each stream from a given version onwards; the `(stream, version)` index of the journal serves the join. */
  private def newEvents[E](namespace: String, codec: BackendCodec[E]): Query[(Arr[String], Arr[Long]), EventMessage[E]] =
    val metadata = (uuid *: timestamptz *: int8 *: int8 *: text).to[EventMetadata]
    // Wrapped in a 1-tuple, like edomata does, so the twiddle of an abstract `E` can be mapped to the event message.
    val payload = codec.codec.imap(_ *: EmptyTuple)(_.head)
    sql"""
      select j.id, j.time, j.seqnr, j.version, j.stream, j.payload
      from unnest(${_text}, ${_int8}) as k(stream, version)
      join "#$namespace".journal j on j.stream = k.stream and j.version >= k.version
      order by j.stream, j.version
    """.query((metadata *: payload).to[EventMessage[E]])
end StreamReader
