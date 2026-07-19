package io.github.nicolasfara.rstmanager.service.codecs

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ Manufacturing, ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{
  ScheduledManufacturing,
  ScheduledManufacturingId,
  ScheduledManufacturingInfo
}
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class WorkCodecsTest extends AnyFlatSpecLike:
  private val day: DateTime = DateTime.parse("2026-06-15").nn
  private val firstManufacturingId: ScheduledManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000701").nn
  private val secondManufacturingId: ScheduledManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000702").nn

  private def manufacturing(id: ScheduledManufacturingId): ScheduledManufacturing =
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        id,
        "MFG-TEST".refineUnsafe[ManufacturingCode],
        day,
        NonEmptyList.one(
          ScheduledTask.PendingTask(UUID.randomUUID().nn: ScheduledTaskId, UUID.randomUUID().nn: TaskId, TaskHours(8)),
        ),
        ManufacturingDependencies(),
      ),
    )

  private def orderData(): OrderData =
    OrderData(
      UUID.fromString("00000000-0000-0000-0000-000000000703").nn,
      "ORD-703".refineUnsafe[OrderNumber],
      UUID.fromString("00000000-0000-0000-0000-000000000704").nn: CustomerId,
      day,
      day.plusDays(5).nn,
      OrderPriority.Normal,
      NonEmptyList.of(manufacturing(firstManufacturingId), manufacturing(secondManufacturingId)),
      Some("with dependencies"),
      OrderDependencies.empty.addManufacturingDependencies(secondManufacturingId, Set(firstManufacturingId)),
    )

  "OrderData codec" should "round-trip the manufacturing dependency graph" in:
    val data = orderData()
    val decoded = data.asJson.as[OrderData].fold(err => fail(s"Decoding failed: $err"), identity)

    decoded.dependencies.toEdgePairs shouldEqual List((secondManufacturingId, firstManufacturingId))
    decoded.id shouldEqual data.id

  it should "decode order data persisted before the dependency graph existed" in:
    val legacyJson = orderData().asJson.mapObject(_.remove("dependencies"))
    val decoded = legacyJson.as[OrderData].fold(err => fail(s"Decoding failed: $err"), identity)

    decoded.dependencies.toEdgePairs shouldBe empty
    decoded.setOfManufacturing.map(_.info.id).toList shouldEqual List(firstManufacturingId, secondManufacturingId)

  "ScheduledManufacturingInfo codec" should "round-trip per-task preferred employees" in:
    val taskInstanceId: ScheduledTaskId = UUID.fromString("00000000-0000-0000-0000-000000000705").nn
    val employeeId = UUID.fromString("00000000-0000-0000-0000-000000000101").nn
    val info = manufacturing(firstManufacturingId).info.copy(taskPreferredEmployees = Map(taskInstanceId -> employeeId))
    val decoded = info.asJson.as[ScheduledManufacturingInfo].fold(err => fail(s"Decoding failed: $err"), identity)

    decoded.taskPreferredEmployees shouldEqual Map(taskInstanceId -> employeeId)

  it should "decode info persisted before per-task preferred employees existed" in:
    val legacyJson = manufacturing(firstManufacturingId).info.asJson.mapObject(_.remove("taskPreferredEmployees"))
    val decoded = legacyJson.as[ScheduledManufacturingInfo].fold(err => fail(s"Decoding failed: $err"), identity)

    decoded.taskPreferredEmployees shouldBe empty
    decoded.id shouldEqual firstManufacturingId

  "Manufacturing codec" should "decode catalog templates persisted before default employees existed" in:
    val taskId: TaskId = UUID.fromString("00000000-0000-0000-0000-000000000301").nn
    val employeeId = UUID.fromString("00000000-0000-0000-0000-000000000101").nn
    val template = Manufacturing
      .createManufacturing(
        UUID.fromString("00000000-0000-0000-0000-000000000706").nn,
        "MFG-TEST",
        "Serramento standard",
        None,
        List(taskId),
        ManufacturingDependencies(),
        Map(taskId -> employeeId),
      )
      .toEither
      .fold(errors => fail(s"Invalid template: $errors"), identity)

    val roundTripped = template.asJson.as[Manufacturing].fold(err => fail(s"Decoding failed: $err"), identity)
    roundTripped.defaultEmployees shouldEqual Map(taskId -> employeeId)

    val legacyJson = template.asJson.mapObject(_.remove("defaultEmployees"))
    val decoded = legacyJson.as[Manufacturing].fold(err => fail(s"Decoding failed: $err"), identity)
    decoded.defaultEmployees shouldBe empty
    decoded.taskIds.toList shouldEqual List(taskId)
end WorkCodecsTest
