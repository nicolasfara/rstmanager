package io.github.nicolasfara.rstmanager.warehouse.domain.product

import java.time.Instant

import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.product.ProductService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.StockFixtures.*
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent

import cats.Id
import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.{ CommandMessage, Decision, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class ConsumptionTest extends AnyFlatSpecLike:
  private val today: LocalDate = day(30)
  private val meters: ProductRecord = ProductRecord.created(productInfo(productId(1), "TES-1", UnitOfMeasure.LinearMeters), base)
  private val ref: ConsumptionRef = consumptionRef(1, 1)
  private val otherRef: ConsumptionRef = consumptionRef(1, 2)

  private type Result = Decision[ProductError, ProductEvent, ProductAggregate]

  private def q(thousandths: Long): Quantity = Quantity.applyUnsafe(thousandths)
  private def price(units: Long): UnitPrice = UnitPrice.applyUnsafe(units)

  private def accepted(result: Result): (List[ProductEvent], ProductAggregate) = result match
    case Decision.Accepted(events, state) => (events.toList, state)
    case other => fail(s"Expected an acceptance, got: $other")

  private def rejection(result: Result): List[ProductError] = result match
    case Decision.Rejected(reasons) => reasons.toChain.toList
    case other => fail(s"Expected a rejection, got: $other")

  private def recordOf(state: ProductAggregate): ProductRecord = state match
    case ProductAggregate.Active(product) => product
    case ProductAggregate.Archived(product, _) => product
    case ProductAggregate.Empty => fail("Unexpected empty product")

  private def consume(state: ProductAggregate, target: ConsumptionRef, quantity: Long, on: LocalDate): ProductAggregate =
    accepted(state.setConsumption(target, q(quantity), on, today))._2

  /** 10 m bought at 3.00 €/m on day 1, 2 m more at 5.00 €/m on day 10. */
  private val stocked: ProductAggregate =
    val first = ProductAggregate.Active(meters).recordPurchase(movementId(1), mpnCode, firstSupplier, q(10000L), price(30000L), None, day(1), today)
    accepted(accepted(first)._2.recordPurchase(movementId(2), mpnCode, firstSupplier, q(2000L), price(50000L), None, day(10), today))._2

  "The ledger" should "turn an absolute consumption into its delta, a reversal when it shrinks" in:
    val ledger = StockLedger.empty
      .consumptionSet(ref, q(2500L), day(3))
      .consumptionSet(ref, q(2500L), day(4)) // unchanged: no movement
      .consumptionSet(ref, q(1000L), day(5))
      .consumptionSet(otherRef, q(400L), day(5))
      .consumptionSet(ref, q(0L), day(6))
    ledger.movements.map(movement => (movement.on, movement.delta.value, movement.kind)) shouldEqual Vector(
      (day(3), -2500L, MovementKind.Consumption(ref)),
      (day(5), 1500L, MovementKind.Consumption(ref)),
      (day(5), -400L, MovementKind.Consumption(otherRef)),
      (day(6), 1000L, MovementKind.Consumption(ref)),
    )
    ledger.consumptions shouldEqual Map(otherRef -> q(400L))
    ledger.consumedBy(ref) shouldEqual Quantity.zero
    ledger.balanceAt(day(10)).value shouldEqual -400L

  it should "give consumption movements deterministic ids, distinct from each other" in:
    val first = StockLedger.empty.consumptionSet(ref, q(1000L), day(1)).consumptionSet(ref, q(0L), day(2))
    val second = StockLedger.empty.consumptionSet(ref, q(1000L), day(1)).consumptionSet(ref, q(0L), day(2))
    first.movements.map(_.id) shouldEqual second.movements.map(_.id)
    first.movements.map(_.id).distinct should have size 2
    first.movements.map(_.id) shouldEqual Vector(StockLedger.consumptionMovementId(ref, 0), StockLedger.consumptionMovementId(ref, 1))

  it should "value a consumption and its reversal at the highest cost of their own day, never as a price point" in:
    val ledger = recordOf(consume(consume(stocked, ref, 3000L, day(5)), ref, 1000L, day(12))).ledger
    val consumptions = ledger.movements.filter(_.consumption.isDefined)
    consumptions.map(movement => (ledger.unitCostOf(movement), ledger.valueOf(movement))) shouldEqual Vector(
      (Some(price(30000L)), Some(Amount.applyUnsafe(-900L))), // 3 m × 3.00 €
      (Some(price(50000L)), Some(Amount.applyUnsafe(1000L))), // storno of 2 m × 5.00 €
    )
    ledger.maxCostAt(day(12)) shouldEqual Some(price(50000L))
    ledger.snapshotAt(day(12)).quantity.value shouldEqual 11000L

  "setConsumption" should "decide nothing when the product already consumes the requested quantity" in:
    stocked.setConsumption(ref, Quantity.zero, day(5), today) shouldEqual Decision.pure(stocked)
    val consumed = consume(stocked, ref, 3000L, day(5))
    consumed.setConsumption(ref, q(3000L), day(9), today) shouldEqual Decision.pure(consumed)

  it should "record the absolute event, beyond the stock, leaving a negative balance" in:
    val (events, state) = accepted(stocked.setConsumption(ref, q(15000L), day(5), today))
    events shouldEqual List(ProductEvent.ConsumptionSet(ref, q(15000L), day(5)))
    val record = recordOf(state)
    record.ledger.balanceAt(day(5)).value shouldEqual -5000L
    record.ledger.snapshotAt(day(5)).negative shouldEqual true
    record.ledger.balanceAt(day(10)).value shouldEqual -3000L

  it should "be accepted on an archived product, which stays archived" in:
    val archived = ProductAggregate.Archived(recordOf(stocked), day(20))
    accepted(archived.setConsumption(ref, q(1000L), day(25), today))._2 match
      case ProductAggregate.Archived(product, archivedOn) =>
        archivedOn shouldEqual day(20)
        product.ledger.consumedBy(ref) shouldEqual q(1000L)
      case other => fail(s"Unexpected state: $other")

  it should "reject a day after today and a missing product" in:
    rejection(stocked.setConsumption(ref, q(1000L), today.plusDays(1).nn, today)) shouldEqual List(
      ProductError.MovementInFuture(today.plusDays(1).nn, today),
    )
    rejection(ProductAggregate.Empty.setConsumption(ref, q(1000L), day(1), today)) shouldEqual List(ProductError.ProductNotFound)

  it should "keep a day before the product's creation" in:
    val later = ProductAggregate.Active(ProductRecord.created(meters.info, day(10)))
    accepted(later.setConsumption(ref, q(1000L), day(2), today))._1 shouldEqual List(ProductEvent.ConsumptionSet(ref, q(1000L), day(2)))

  "Voiding a consumption" should "be rejected: the reconciliation with the order owns it" in:
    val consumed = consume(stocked, ref, 1000L, day(5))
    val movement = recordOf(consumed).ledger.movements.find(_.consumption.isDefined).map(_.id).getOrElse(fail("No consumption"))
    rejection(consumed.voidMovement(movement, reason, today)) shouldEqual List(ProductError.ConsumptionNotVoidable(movement))

  "A manual unload" should "still be rejected once a consumption took the stock below zero" in:
    val overConsumed = consume(stocked, ref, 13000L, day(12))
    rejection(overConsumed.unload(movementId(9), q(1L), reason, day(12), today)) shouldEqual
      List(ProductError.InsufficientStock(day(12), SignedQuantity.applyUnsafe(-1000L)))

  "The fold" should "replay consumptions, on an archived product too" in:
    val events = List(
      ProductEvent.ProductCreated(meters.info, base),
      ProductEvent.ConsumptionSet(ref, q(2000L), day(1)),
      ProductEvent.ProductArchived(day(2)),
      ProductEvent.ConsumptionSet(ref, q(500L), day(3)),
    )
    val folded: ValidatedNec[ProductError, ProductAggregate] =
      events.foldLeft(ProductAggregate.initial.validNec[ProductError])((state, event) => state.andThen(ProductAggregate.transition(event)))
    folded.toOption.collect { case ProductAggregate.Archived(product, _) =>
      (product.ledger.consumedBy(ref), product.ledger.balanceAt(day(3)).value)
    } shouldEqual
      Some((q(500L), -500L))

  "ProductService.SetConsumption" should "publish only when the consumption changes" in:
    val at = Instant.parse("2026-01-31T10:00:00Z").nn // day 30
    def run(state: ProductAggregate, quantity: Long) =
      ProductService[Id].execute(
        RequestContext(CommandMessage("message-1", at, productId(1).toString, Command.SetConsumption(ref, q(quantity), day(5))), state),
      )
    run(stocked, 1000L) match
      case EdomatonResult.Accepted(_, emitted, notifications) =>
        emitted.toList shouldEqual List(ProductEvent.ConsumptionSet(ref, q(1000L), day(5)))
        notifications.toList shouldEqual List(Notification.ProductChanged(productId(1)))
      case other => fail(s"Unexpected result: $other")
    run(consume(stocked, ref, 1000L, day(5)), 1000L) match
      case EdomatonResult.Indecisive(notifications) => notifications.toList shouldEqual Nil
      case other => fail(s"Unexpected result: $other")

  "The reports" should "count a reversal (storno) as a smaller outflow and flag the negative stock as an anomaly" in:
    val record = recordOf(consume(consume(stocked, ref, 13000L, day(12)), ref, 4000L, day(20)))
    val product = ReportedProduct(record, None)
    val state = WarehouseReports.stateAt(List(product), day(12))
    state.rows.map(_.stock.quantity.value) shouldEqual List(-1000L)
    state.totals.negativeProducts shouldEqual 1
    WarehouseReports.stateAt(List(product), day(20)).totals.negativeProducts shouldEqual 0
    val flows = WarehouseReports.flows(List(product), day(11), day(25), FlowGranularity.Week)
    // Consumed 13 m on day 12 (65.00 € at 5.00 €/m) and gave 9 m back on day 20 (45.00 €): 4 m and 20.00 € out in all.
    flows.totals.outValue shouldEqual Amount.applyUnsafe(2000L)
    flows.totals.inValue shouldEqual Amount.zero
    flows.totals.quantities shouldEqual List(UnitFlow(UnitOfMeasure.LinearMeters, SignedQuantity.zero, SignedQuantity.applyUnsafe(4000L)))
    flows.buckets
      .map(bucket => (bucket.label, bucket.totals.outValue.value)) shouldEqual List(("2026-W03", 6500L), ("2026-W04", -4500L), ("2026-W05", 0L))
end ConsumptionTest
