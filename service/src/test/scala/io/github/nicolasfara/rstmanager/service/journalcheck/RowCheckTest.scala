package io.github.nicolasfara.rstmanager.service.journalcheck

import scala.io.Source
import scala.util.Using

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.order.Order
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskAggregate, TaskService }
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent

import fs2.Stream
import io.circe.Decoder
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class RowCheckTest extends AnyFlatSpecLike:
  import RowCheckTest.*

  private def replay(rows: StoredRow*): RowReport =
    RowCheck.journal(TaskAggregate, Decoder[TaskEvent]).run(Stream.emits(rows)).toList.headOption.getOrElse(fail("No report"))

  "A journal check" should "replay every stream, whatever version its rows were written with" in:
    val report =
      replay(row("a", 1, "legacy-v1-task-created"), row("a", 2, "v2-task-updated"), row("b", 1, "v2-task-created"), row("b", 2, "v2-task-deleted"))

    report shouldEqual RowReport(rows = 4, streams = 2, failures = Vector.empty)
    report.ok shouldBe true

  it should "replay an order stream mixing rows written before and after materials existed" in:
    val report = RowCheck
      .journal(Order, Decoder[OrderEvent])
      .run(
        Stream.emits(
          List(
            orderRow("a", 1, "legacy-v1-order-created"),
            orderRow("a", 2, "v2-task-added"),
            orderRow("a", 3, "v3-manufacturing-materials-changed"),
            orderRow("a", 4, "v3-manufacturing-added-with-materials"),
            orderRow("a", 5, "v2-order-suspended"),
          ),
        ),
      )
      .toList

    report shouldEqual List(RowReport(rows = 5, streams = 1, failures = Vector.empty))

  it should "report a row that cannot be decoded, and stop folding its stream" in:
    val report = replay(
      StoredRow("a", 1, """{"TaskArchived":{}}"""),
      row("a", 2, "v2-task-updated"),
      row("b", 1, "v2-task-created"),
      StoredRow("b", 2, "not json"),
    )

    report.failures.map(failure => (failure.stream, failure.position)) shouldEqual Vector(("a", 1L), ("b", 2L))
    report.failures.map(_.reason).foreach(_ should startWith("cannot be decoded"))
    report.streams shouldEqual 2

  it should "report an event the aggregate rejects on replay" in:
    val report = replay(row("a", 1, "v2-task-created"), row("b", 1, "v2-task-updated"), row("b", 2, "v2-task-deleted"))

    report.failures.map(failure => (failure.stream, failure.position)) shouldEqual Vector(("b", 1L))
    report.failures.head.reason should startWith("rejected by the aggregate")

  it should "be empty on an empty journal" in:
    replay() shouldEqual RowReport(0, 0, Vector.empty)

  "An outbox check" should "only decode the rows" in:
    val report = RowCheck
      .outbox(Decoder[TaskService.Notification])
      .run(Stream.emits(List(StoredRow("a", 1, """{"TaskChanged":{"taskId":"00000000-0000-0000-0000-000000000301"}}"""), StoredRow("a", 2, "{}"))))
      .toList

    report.map(_.failures.map(_.position)) shouldEqual List(Vector(2L))
end RowCheckTest

object RowCheckTest:
  private def row(stream: String, version: Long, fixture: String): StoredRow = stored("task-events", stream, version, fixture)

  private def orderRow(stream: String, version: Long, fixture: String): StoredRow = stored("order-events", stream, version, fixture)

  private def stored(directory: String, stream: String, version: Long, fixture: String): StoredRow =
    StoredRow(stream, version, Using.resource(Source.fromResource(s"golden/$directory/$fixture.json"))(_.mkString))
