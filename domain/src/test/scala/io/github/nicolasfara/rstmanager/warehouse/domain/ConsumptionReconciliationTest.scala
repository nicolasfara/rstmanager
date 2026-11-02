package io.github.nicolasfara.rstmanager.warehouse.domain

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ ConsumptionRef, ProductAggregate, ProductId, ProductRecord, ProductService }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.StockFixtures.*
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

class ConsumptionReconciliationTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:
  private type SetConsumption = ProductService.Command.SetConsumption

  private val today: LocalDate = day(30)
  private val order: OrderId = orderId(1)
  private val otherOrder: OrderId = orderId(2)
  private val profile: ProductId = productId(1)
  private val glass: ProductId = productId(2)
  private val screws: ProductId = productId(3)
  private val taskId: ScheduledTaskId = ScheduledTaskId(new UUID(0x7aL, 1L))
  private val templateTaskId: TaskId = TaskId(new UUID(0x7bL, 1L))
  private val customerId: CustomerId = CustomerId(new UUID(0x7cL, 1L))

  /** 10:00 in Rome on day `offset` (the business day of a completion). */
  private def at(offset: Int): DateTime = DateTime.parse(s"${day(offset)}T09:00:00Z")

  private def q(thousandths: Long): Quantity = Quantity.applyUnsafe(thousandths)

  private def material(productId: ProductId, quantity: Long): ScheduledMaterial =
    ScheduledMaterial(productId, q(quantity), UnitPrice.applyUnsafe(10000L))

  /** 1 m of profile at 3.00 €/m bought on day 0, nothing else. */
  private val products: List[ProductRecord] =
    List(profile -> UnitOfMeasure.LinearMeters, glass -> UnitOfMeasure.Pieces, screws -> UnitOfMeasure.Pieces).map { (id, unit) =>
      val created = ProductRecord.created(productInfo(id, s"P-${id.value.getLeastSignificantBits}", unit), base)
      if id == profile then
        created.withLedger(_.appended(movementId(1), base, SignedQuantity.applyUnsafe(1000L), purchase(1000L, 30000L, firstSupplier)._2))
      else created
    }

  private def info(manufacturing: Int, materials: List[ScheduledMaterial]): ScheduledManufacturingInfo =
    ScheduledManufacturingInfo(
      manufacturingId(manufacturing),
      "MFG-1".refineUnsafe[ManufacturingCode],
      at(25),
      NonEmptyList.one(InProgressTask(taskId, templateTaskId, TaskDuration(8), TaskDuration(0))),
      ManufacturingDependencies(),
      materials = materials,
    )

  private def inProgress(manufacturing: Int, materials: List[ScheduledMaterial]): ScheduledManufacturing =
    InProgressManufacturing(info(manufacturing, materials), at(0))

  private def completed(manufacturing: Int, materials: List[ScheduledMaterial], completedOn: Int): ScheduledManufacturing =
    val done = info(manufacturing, materials)
    CompletedManufacturing(
      done.copy(tasks = NonEmptyList.one(CompletedTask(taskId, templateTaskId, TaskDuration(8), TaskDuration(8), at(completedOn)))),
      at(0),
      at(completedOn),
    )

  private def data(id: OrderId, manufacturings: NonEmptyList[ScheduledManufacturing]): OrderData =
    OrderData(id, "ORD-1".refineUnsafe[OrderNumber], customerId, at(0), at(40), OrderPriority.Normal, manufacturings)

  private def open(manufacturings: ScheduledManufacturing*): Order =
    Order.InProgressOrder(data(order, NonEmptyList.fromListUnsafe(manufacturings.toList)), at(40))

  private def plan(target: Order, current: List[ProductRecord]): List[(ProductId, SetConsumption)] =
    ConsumptionReconciliation.plan(ConsumptionReconciliation.desired(order, target), ConsumptionReconciliation.applied(order, current), today)

  /** Applies every step through the product decision and fold, as the product backend would. */
  private def applyPlan(current: List[ProductRecord], steps: List[(ProductId, SetConsumption)]): List[ProductRecord] =
    steps.foldLeft(current) { case (records, (productId, command)) =>
      records.map { record =>
        if record.info.id != productId then record
        else
          ProductAggregate
            .Active(record)
            .setConsumption(command.ref, command.quantity, command.on, today)
            .toValidated
            .fold(errors => fail(s"Rejected: $errors"), identity) match
            case ProductAggregate.Active(product) => product
            case other => fail(s"Unexpected state: $other")
      }
    }

  private def reconcile(target: Order, current: List[ProductRecord]): List[ProductRecord] = applyPlan(current, plan(target, current))

  private def consumed(current: List[ProductRecord], productId: ProductId, manufacturing: Int): Long =
    current.find(_.info.id == productId).fold(0L)(_.ledger.consumedBy(ConsumptionRef(order, manufacturingId(manufacturing))).value)

  private def consumptionDays(current: List[ProductRecord], productId: ProductId): List[(LocalDate, Long)] =
    current.find(_.info.id == productId).toList.flatMap(_.ledger.movements.filter(_.consumption.isDefined).map(m => (m.on, m.delta.value: Long)))

  private val materials = List(material(profile, 2500L), material(glass, 2000L))

  // ---- Generators of the properties ----------------------------------------------------------------------------------------------------------

  private val genMaterials: Gen[List[ScheduledMaterial]] =
    for
      chosen <- Gen.someOf(List(profile, glass, screws))
      quantities <- Gen.listOfN(chosen.size, Gen.chooseNum(1L, 9000L).map(_ * 1000L))
    yield chosen.toList.zip(quantities).map(material)

  private def genManufacturing(n: Int): Gen[ScheduledManufacturing] =
    for
      lines <- genMaterials
      completedOn <- Gen.option(Gen.chooseNum(0, 20))
    yield completedOn.fold(inProgress(n, lines))(completed(n, lines, _))

  private val genOrder: Gen[Order] =
    for
      count <- Gen.chooseNum(1, 3)
      manufacturings <- Gen.sequence[List[ScheduledManufacturing], ScheduledManufacturing]((1 to count).map(genManufacturing))
      cancelled <- Gen.oneOf(true, false)
    yield
      val content = data(order, NonEmptyList.fromListUnsafe(manufacturings))
      if cancelled then Order.CancelledOrder(content, at(21), None) else Order.InProgressOrder(content, at(40))

  /** Products holding what an earlier version of the order consumed, plus a consumption of another order. */
  private val genProducts: Gen[List[ProductRecord]] =
    genOrder.map { earlier =>
      val other = ConsumptionRef(otherOrder, manufacturingId(1))
      reconcile(earlier, products).map(record => record.withLedger(_.consumptionSet(other, q(1000L), day(2))))
    }

  "ConsumptionReconciliation" should "consume the materials of a completed manufacturing from its completion day" in:
    val after = reconcile(open(completed(1, materials, 5), inProgress(2, List(material(screws, 4000L)))), products)
    consumptionDays(after, profile) shouldEqual List((day(5), -2500L))
    consumptionDays(after, glass) shouldEqual List((day(5), -2000L))
    consumed(after, screws, 2) shouldEqual 0L

  it should "reverse the consumption today when a task is reopened, and consume again on the day it is completed again" in:
    val done = completed(1, materials, 5)
    val consumedOnce = reconcile(open(done), products)
    val reopened = done.revertTaskToInProgress(taskId, at(7)).fold(error => fail(error.toString), identity)
    reopened.status shouldEqual ManufacturingStatus.InProgress
    val reversed = reconcile(open(reopened), consumedOnce)
    consumptionDays(reversed, profile) shouldEqual List((day(5), -2500L), (today, 2500L))
    consumed(reversed, profile, 1) shouldEqual 0L
    val completedAgain = reopened.completeTask(taskId, TaskDuration(8), at(9)).fold(error => fail(error.toString), identity)
    val again = reconcile(open(completedAgain), reversed)
    consumptionDays(again, profile) shouldEqual List((day(5), -2500L), (today, 2500L), (day(9), -2500L))
    plan(open(completedAgain), again) shouldEqual Nil

  it should "apply only the difference when the materials of a completed manufacturing change" in:
    val done = completed(1, materials, 5)
    val consumedOnce = reconcile(open(done), products)
    val corrected = done.withMaterials(List(material(profile, 4000L)))
    corrected.status shouldEqual ManufacturingStatus.Completed
    val after = reconcile(open(corrected), consumedOnce)
    consumptionDays(after, profile) shouldEqual List((day(5), -2500L), (day(5), -1500L))
    consumptionDays(after, glass) shouldEqual List((day(5), -2000L), (today, 2000L))

  it should "reverse the consumption of a completed manufacturing removed from the order" in:
    val withTwo = open(completed(1, materials, 5), completed(2, List(material(screws, 3000L)), 6))
    val consumedTwice = reconcile(withTwo, products)
    val removed = withTwo.orderData.map(_.removeManufacturing(manufacturingId(1))).getOrElse(fail("No data"))
    val after = reconcile(Order.InProgressOrder(removed, at(40)), consumedTwice)
    consumed(after, profile, 1) shouldEqual 0L
    consumed(after, glass, 1) shouldEqual 0L
    consumed(after, screws, 2) shouldEqual 3000L

  it should "keep the consumptions of a cancelled order" in:
    val withTwo = data(order, NonEmptyList.of(completed(1, materials, 5), inProgress(2, List(material(screws, 3000L)))))
    val consumedOnce = reconcile(Order.InProgressOrder(withTwo, at(40)), products)
    plan(Order.CancelledOrder(withTwo, at(10), None), consumedOnce) shouldEqual Nil
    ConsumptionReconciliation.desired(order, Order.NewOrder) shouldEqual Map.empty

  it should "record a consumption beyond the stock, flagged as an anomaly in the state of the day" in:
    val after = reconcile(open(completed(1, materials, 5)), products)
    val state = WarehouseReports.stateAt(after.map(ReportedProduct(_, None)), day(5))
    state.rows.map(row => (row.product.record.info.id, row.stock.quantity.value, row.stock.negative)) shouldEqual
      List((profile, -1500L, true), (glass, -2000L, true), (screws, 0L, false))
    state.totals.negativeProducts shouldEqual 2

  it should "leave the consumptions of other orders alone" in:
    val other = ConsumptionRef(otherOrder, manufacturingId(1))
    val withOther = products.map(record => if record.info.id == glass then record.withLedger(_.consumptionSet(other, q(5000L), day(3))) else record)
    val after = reconcile(open(inProgress(1, materials)), reconcile(open(completed(1, materials, 5)), withOther))
    after.find(_.info.id == glass).map(_.ledger.consumedBy(other)) shouldEqual Some(q(5000L))
    ConsumptionReconciliation.applied(order, after) shouldEqual Map.empty

  "The reconciliation plan" should "converge: once applied, the products consume exactly what the order wants" in:
    forAll(genOrder, genProducts) { (target, current) =>
      val after = reconcile(target, current)
      ConsumptionReconciliation.applied(order, after) shouldEqual ConsumptionReconciliation.desired(order, target).view.mapValues(_.quantity).toMap
      after.foreach(_.ledger.consumedBy(ConsumptionRef(otherOrder, manufacturingId(1))) shouldEqual q(1000L))
    }

  it should "be idempotent: a second plan is empty and applying a plan twice changes nothing more" in:
    forAll(genOrder, genProducts) { (target, current) =>
      val steps = plan(target, current)
      val once = applyPlan(current, steps)
      plan(target, once) shouldEqual Nil
      applyPlan(once, steps) shouldEqual once
    }

  it should "keep every product balance equal to its non-consumption movements minus what is consumed" in:
    forAll(genOrder, genProducts) { (target, current) =>
      reconcile(target, current).foreach { record =>
        val manual = record.ledger.active.filter(_.consumption.isEmpty).map(_.delta.value: Long).sum
        val consumedTotal = record.ledger.consumptions.values.map(_.value: Long).sum
        record.ledger.balanceAt(today).value shouldEqual manual - consumedTotal
      }
    }
end ConsumptionReconciliationTest
