package io.github.nicolasfara.rstmanager.warehouse.domain

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialId, MaterialRecord }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.StockFixtures.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing.{ CompletedManufacturing, InProgressManufacturing }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderData, OrderId, OrderNumber, OrderPriority }
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.{ CompletedTask, InProgressTask }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import io.github.iltotore.iron.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MaterialRequirementsTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:
  private val today: LocalDate = day(30)
  private val profile: MaterialId = materialId(1)
  private val glass: MaterialId = materialId(2)
  private val unknown: MaterialId = materialId(9)
  private val taskId: ScheduledTaskId = ScheduledTaskId(new UUID(0x7aL, 1L))
  private val templateTaskId: TaskId = TaskId(new UUID(0x7bL, 1L))
  private val customerId: CustomerId = CustomerId(new UUID(0x7cL, 1L))

  private val number: String :| OrderNumber = "ORD-1"
  private val code: String :| ManufacturingCode = "MFG-1"

  private def shortage(order: OrderId, manufacturing: ScheduledManufacturingId, material: MaterialId, required: Quantity, missing: Quantity) =
    MaterialShortage(order, number, manufacturing, code, material, required, missing)

  private def at(offset: Int): DateTime = DateTime.parse(s"${day(offset)}T09:00:00Z")

  private def q(thousandths: Long): Quantity = Quantity.applyUnsafe(thousandths)

  private def line(materialId: MaterialId, quantity: Long): ScheduledMaterial = ScheduledMaterial(materialId, q(quantity), UnitPrice.applyUnsafe(1L))

  /** A material holding `stock` thousandths (one purchase on day 0, or an outflow when negative). */
  private def stocked(id: MaterialId, code: String, stock: Long): MaterialRecord =
    val created = MaterialRecord.created(materialInfo(id, code, UnitOfMeasure.LinearMeters), base)
    if stock == 0L then created
    else
      created.withLedger(
        _.appended(movementId(id.value.getLeastSignificantBits.toInt), base, SignedQuantity.applyUnsafe(stock), purchase(1L, 1L, firstSupplier)._2),
      )

  private def info(manufacturing: Int, due: Int, lines: List[ScheduledMaterial]): ScheduledManufacturingInfo =
    ScheduledManufacturingInfo(
      manufacturingId(manufacturing),
      code,
      at(due),
      NonEmptyList.one(InProgressTask(taskId, templateTaskId, TaskDuration(8), TaskDuration(0))),
      ManufacturingDependencies(),
      materials = lines,
    )

  private def open(manufacturing: Int, due: Int, lines: List[ScheduledMaterial]): ScheduledManufacturing =
    InProgressManufacturing(info(manufacturing, due, lines), at(0))

  private def done(manufacturing: Int, due: Int, lines: List[ScheduledMaterial]): ScheduledManufacturing =
    val base = info(manufacturing, due, lines)
    CompletedManufacturing(
      base.copy(tasks = NonEmptyList.one(CompletedTask(taskId, templateTaskId, TaskDuration(8), TaskDuration(8), at(5)))),
      at(0),
      at(5),
    )

  private def data(id: OrderId, priority: OrderPriority, created: Int, manufacturings: ScheduledManufacturing*): OrderData =
    OrderData(id, number, customerId, at(created), at(40), priority, NonEmptyList.fromListUnsafe(manufacturings.toList))

  private def inProgress(id: OrderId, manufacturings: ScheduledManufacturing*): (OrderId, Order) =
    id -> Order.InProgressOrder(data(id, OrderPriority.Normal, 0, manufacturings*), at(40))

  private def compute(orders: List[(OrderId, Order)], materials: List[MaterialRecord], exclude: Option[OrderId]): Requirements =
    MaterialRequirements.compute(orders, materials, today, exclude)

  private val warehouse = List(stocked(glass, "B-GLASS", 5000L), stocked(profile, "A-PROFILE", 10000L))

  "MaterialRequirements" should "commit the open lines, list every material by code and report no shortage when the stock suffices" in:
    val result = compute(List(inProgress(orderId(1), open(1, 10, List(line(profile, 4000L))))), warehouse, None)
    result.materials shouldEqual List(
      MaterialRequirement(profile, SignedQuantity.applyUnsafe(10000L), q(4000L), SignedQuantity.applyUnsafe(6000L)),
      MaterialRequirement(glass, SignedQuantity.applyUnsafe(5000L), Quantity.zero, SignedQuantity.applyUnsafe(5000L)),
    )
    result.shortages shouldBe empty
    result.unknownMaterials shouldBe empty

  it should "allocate the stock by completion date: the later manufacturing is the short one" in:
    val orders = List(
      inProgress(orderId(1), open(1, 20, List(line(profile, 6000L)))),
      inProgress(orderId(2), open(2, 10, List(line(profile, 6000L)))),
    )
    val result = compute(orders, warehouse, None)
    result.shortages shouldEqual List(shortage(orderId(1), manufacturingId(1), profile, q(6000L), q(2000L)))
    result.materials.find(_.materialId == profile).map(_.available.value) shouldEqual Some(-2000L)

  it should "break ties on the completion date by urgency, then by order age" in:
    val normalOld = orderId(1)
    val urgentNew = orderId(2)
    val normalNew = orderId(3)
    val orders = List(
      normalOld -> Order.InProgressOrder(data(normalOld, OrderPriority.Normal, 0, open(1, 10, List(line(profile, 5000L)))), at(40)),
      urgentNew -> Order.InProgressOrder(data(urgentNew, OrderPriority.Urgent, 3, open(2, 10, List(line(profile, 5000L)))), at(40)),
      normalNew -> Order.InProgressOrder(data(normalNew, OrderPriority.Normal, 2, open(3, 10, List(line(profile, 5000L)))), at(40)),
    )
    compute(orders, warehouse, None).shortages shouldEqual List(shortage(normalNew, manufacturingId(3), profile, q(5000L), q(5000L)))

  it should "not count the completed manufacturings, already consumed by the ledger" in:
    val orders = List(inProgress(orderId(1), done(1, 5, List(line(profile, 50000L))), open(2, 10, List(line(profile, 3000L)))))
    val result = compute(orders, warehouse, None)
    result.materials.find(_.materialId == profile).map(_.committed) shouldEqual Some(q(3000L))
    result.shortages shouldBe empty

  it should "ignore completed, delivered and cancelled orders, and count suspended ones" in:
    val lines = List(line(profile, 7000L))
    val orders = List(
      orderId(1) -> Order.CompletedOrder(data(orderId(1), OrderPriority.Normal, 0, open(1, 10, lines)), at(20)),
      orderId(2) -> Order.DeliveredOrder(data(orderId(2), OrderPriority.Normal, 0, open(2, 10, lines)), at(20), at(21)),
      orderId(3) -> Order.CancelledOrder(data(orderId(3), OrderPriority.Normal, 0, open(3, 10, lines)), at(20), None),
      orderId(4) -> Order.SuspendedOrder(data(orderId(4), OrderPriority.Normal, 0, open(4, 10, lines)), at(40), at(20), None),
    )
    compute(orders, warehouse, None).materials.find(_.materialId == profile).map(_.committed) shouldEqual Some(q(7000L))

  it should "leave the excluded order out of the demand" in:
    val orders = List(
      inProgress(orderId(1), open(1, 10, List(line(profile, 8000L)))),
      inProgress(orderId(2), open(2, 20, List(line(profile, 8000L)))),
    )
    compute(orders, warehouse, None).shortages.map(_.orderId) shouldEqual List(orderId(2))
    val excluded = compute(orders, warehouse, Some(orderId(1)))
    excluded.shortages shouldBe empty
    excluded.materials.find(_.materialId == profile).map(_.committed) shouldEqual Some(q(8000L))

  it should "leave every manufacturing short of its whole demand when the stock is negative" in:
    val negative = List(stocked(profile, "A-PROFILE", -1000L))
    val orders = List(inProgress(orderId(1), open(1, 10, List(line(profile, 2000L))), open(2, 12, List(line(profile, 500L)))))
    val result = compute(orders, negative, None)
    result.shortages.map(shortage => (shortage.manufacturingId, shortage.missing)) shouldEqual
      List(manufacturingId(1) -> q(2000L), manufacturingId(2) -> q(500L))
    result.materials.map(_.available.value) shouldEqual List(-3500L)

  it should "aggregate one material across manufacturings and orders, and allocate each material on its own" in:
    val orders = List(
      inProgress(orderId(1), open(1, 10, List(line(profile, 4000L), line(glass, 2000L))), open(2, 11, List(line(profile, 4000L)))),
      inProgress(orderId(2), open(3, 12, List(line(profile, 4000L), line(glass, 4000L)))),
    )
    val result = compute(orders, warehouse, None)
    result.materials.map(requirement => (requirement.materialId, requirement.committed, requirement.available.value)) shouldEqual
      List((profile, q(12000L), -2000L), (glass, q(6000L), -1000L))
    result.shortages shouldEqual List(
      shortage(orderId(2), manufacturingId(3), profile, q(4000L), q(2000L)),
      shortage(orderId(2), manufacturingId(3), glass, q(4000L), q(1000L)),
    )

  it should "report the demand on unknown materials apart" in:
    val result = compute(List(inProgress(orderId(1), open(1, 10, List(line(unknown, 1000L), line(profile, 1000L))))), warehouse, None)
    result.unknownMaterials shouldEqual List(UnknownMaterialDemand(orderId(1), number, manufacturingId(1), code, unknown, q(1000L)))
    result.shortages shouldBe empty

  it should "use the stock at the end of today, ignoring later movements" in:
    val later = stocked(profile, "A-PROFILE", 0L)
      .withLedger(_.appended(movementId(50), day(31), SignedQuantity.applyUnsafe(9000L), purchase(1L, 1L, firstSupplier)._2))
    compute(Nil, List(later), None).materials.map(_.stock.value) shouldEqual List(0L)

  "The allocation" should "keep committed = sum of demand, available = stock - committed, and missing within the shortfall" in:
    val genOrders: Gen[List[(OrderId, Order)]] =
      Gen.chooseNum(1, 5).flatMap { count =>
        Gen.sequence[List[(OrderId, Order)], (OrderId, Order)]((1 to count).map { n =>
          for
            due <- Gen.chooseNum(1, 30)
            quantity <- Gen.chooseNum(1L, 9000L)
          yield inProgress(orderId(n), open(n, due, List(line(profile, quantity))))
        })
      }
    forAll(genOrders, Gen.chooseNum(-5000L, 20000L)) { (orders, stock) =>
      val result = compute(orders, List(stocked(profile, "A-PROFILE", stock)), None)
      val demand = orders.flatMap(_._2.orderData.toList.flatMap(_.setOfManufacturing.toList.flatMap(_.info.materials.map(_.quantity.value)))).sum
      result.materials.map(r => (r.committed.value, r.available.value)) shouldEqual List((demand, stock - demand))
      result.shortages.map(_.missing.value).sum shouldEqual (demand - stock.max(0L)).max(0L)
      all(result.shortages.map(s => s.missing.value <= s.required.value && s.missing.value > 0L)) shouldBe true
    }
end MaterialRequirementsTest
