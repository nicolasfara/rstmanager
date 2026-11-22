package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialRequirementsHttpApi.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing.{ CompletedManufacturing, InProgressManufacturing }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderData, OrderId, OrderNumber, OrderPriority }
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.{ CompletedTask, InProgressTask }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.{ NonEmptyList, ValidatedNec }
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class MaterialRequirementsHttpApiTest extends AnyFlatSpecLike:
  private val today: LocalDate = LocalDate.parse("2026-10-30")
  private val due: DateTime = DateTime.parse("2026-11-05T09:00:00Z")
  private val profile: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000901").nn)
  private val glass: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000902").nn)
  private val ghost: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000903").nn)
  private val first: OrderId = OrderId(UUID.fromString("00000000-0000-0000-0000-000000000801").nn)
  private val second: OrderId = OrderId(UUID.fromString("00000000-0000-0000-0000-000000000802").nn)

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  /** A material holding `stock` thousandths, bought on 2026-10-01. */
  private def material(id: MaterialId, code: String, stock: Long): MaterialRecord =
    MaterialRecord
      .created(
        valid(MaterialInfo.createMaterialInfo(id, code, s"Materiale $code", None, None, UnitOfMeasure.LinearMeters, UnitPrice.applyUnsafe(1L), Nil)),
        LocalDate.parse("2026-01-01"),
      )
      .withLedger(
        _.appended(
          MovementId(new UUID(1L, id.value.getLeastSignificantBits)),
          LocalDate.parse("2026-10-01"),
          SignedQuantity.applyUnsafe(stock),
          MovementKind.Adjustment(None, "Inventario"),
        ),
      )

  private def manufacturing(n: Long, lines: List[(MaterialId, Long)], done: Boolean): ScheduledManufacturing =
    val taskId = ScheduledTaskId(new UUID(2L, n))
    val templateId = TaskId(new UUID(3L, n))
    val info = ScheduledManufacturingInfo(
      ScheduledManufacturingId(new UUID(4L, n)),
      s"MFG-$n".refineUnsafe[ManufacturingCode],
      due.plusDays(n.toInt).nn,
      NonEmptyList.one(InProgressTask(taskId, templateId, TaskDuration(8), TaskDuration(0))),
      ManufacturingDependencies(),
      materials = lines.map((id, quantity) => ScheduledMaterial(id, Quantity.applyUnsafe(quantity), UnitPrice.applyUnsafe(1L))),
      quantity = 3,
    )
    if done then
      CompletedManufacturing(info.copy(tasks = NonEmptyList.one(CompletedTask(taskId, templateId, TaskDuration(8), TaskDuration(8), due))), due, due)
    else InProgressManufacturing(info, due)

  private def order(id: OrderId, number: String, manufacturings: ScheduledManufacturing*): Order =
    Order.InProgressOrder(
      OrderData(
        id,
        number.refineUnsafe[OrderNumber],
        CustomerId(UUID.fromString("00000000-0000-0000-0000-000000000805").nn),
        due.minusDays(20).nn,
        due.plusDays(10).nn,
        OrderPriority.Normal,
        NonEmptyList.fromListUnsafe(manufacturings.toList),
      ),
      due.plusDays(10).nn,
    )

  /** A read-only gateway over fixed orders and materials (setting a consumption is not expected). */
  private class FixedGateway(orders: Map[OrderId, Order], materials: List[MaterialRecord]) extends WarehouseConsumptionGateway:
    override def orderIds: IO[List[OrderId]] = IO.pure(orders.keys.toList.sortBy(_.value.toString))
    override def loadOrder(orderId: OrderId): IO[Option[Order]] = IO.pure(orders.get(orderId))
    override def listMaterials: IO[List[MaterialRecord]] = IO.pure(materials)
    override def setConsumption(
        materialId: MaterialId,
        command: MaterialService.Command.SetConsumption,
        commandId: String,
    ): IO[Either[MaterialError, Unit]] = IO.raiseError(new IllegalStateException("read-only"))

  private val gateway = FixedGateway(
    Map(
      first -> order(first, "2026-001", manufacturing(1L, List(profile -> 6000L, ghost -> 1000L), done = false)),
      second -> order(
        second,
        "2026-002",
        manufacturing(2L, List(profile -> 6000L), done = false),
        manufacturing(3L, List(glass -> 9000L), done = true),
      ),
    ),
    List(material(glass, "VET-1", 2000L), material(profile, "PRF-1", 10000L)),
  )

  private def run(exclude: Option[UUID]): MaterialRequirementsResponse =
    requirementsLogic(gateway, IO.pure(today))(exclude).unsafeRunSync().fold(failure => fail(s"Failed: $failure"), identity)

  "GET /warehouse/material-requirements" should "report stock, commitment, availability and the shortages of the open orders" in:
    val response = run(None)
    response.at shouldEqual "2026-10-30"
    response.materials shouldEqual List(
      MaterialRequirementResponse(profile.value, "PRF-1", "Materiale PRF-1", "linear_meters", "10.000", "12.000", "-2.000"),
      MaterialRequirementResponse(glass.value, "VET-1", "Materiale VET-1", "linear_meters", "2.000", "0.000", "2.000"),
    )
    response.shortages shouldEqual List(
      MaterialShortageResponse(second.value, "2026-002", new UUID(4L, 2L), "MFG-2", profile.value, "6.000", "2.000"),
    )
    response.unknownMaterials shouldEqual List(
      UnknownMaterialDemandResponse(first.value, "2026-001", new UUID(4L, 1L), "MFG-1", ghost.value, "1.000"),
    )
    response.asJson.as[MaterialRequirementsResponse] shouldEqual Right(response)

  it should "leave the excluded order out of the demand" in:
    val response = run(Some(second.value))
    response.materials.map(_.committed) shouldEqual List("6.000", "0.000")
    response.shortages shouldBe empty

  it should "turn an unexpected failure into a 500" in:
    val broken = new FixedGateway(Map.empty, Nil):
      override def listMaterials: IO[List[MaterialRecord]] = IO.raiseError(new RuntimeException("database down"))
    requirementsLogic(broken, IO.pure(today))(None).unsafeRunSync().isLeft shouldBe true
end MaterialRequirementsHttpApiTest
