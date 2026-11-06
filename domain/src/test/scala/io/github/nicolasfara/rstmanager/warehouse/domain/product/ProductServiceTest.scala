package io.github.nicolasfara.rstmanager.warehouse.domain.product

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, SignedQuantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.ProductService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.Id
import cats.data.{ Chain, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class ProductServiceTest extends AnyFlatSpecLike:
  private val productId: ProductId = ProductId(UUID.fromString("00000000-0000-0000-0000-0000000006a1").nn)
  private val firstSupplier: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005a1").nn)
  private val secondSupplier: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005a2").nn)

  /** 23:30 UTC on 14 June is already 15 June in Rome. */
  private val lateEvening: Instant = Instant.parse("2026-06-14T23:30:00Z").nn
  private val romeDay: LocalDate = LocalDate.parse("2026-06-15")

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def mpn(code: String, supplierId: SupplierId): Mpn = valid(Mpn.createMpn(code, supplierId, None))

  private def info(price: Long, unit: UnitOfMeasure, mpns: List[Mpn]): ProductInfo =
    valid(
      ProductInfo.createProductInfo(
        productId,
        " tes-001 ",
        "Tessuto acrilico blu",
        Some("Tessuti"),
        None,
        unit,
        UnitPrice.applyUnsafe(price),
        mpns,
      ),
    )

  private val product: ProductInfo = info(37000L, UnitOfMeasure.LinearMeters, List(mpn("ac-100", firstSupplier), mpn("ac-100", secondSupplier)))
  private val record: ProductRecord = ProductRecord.created(product, LocalDate.parse("2026-06-01"))

  private type Result = EdomatonResult[ProductAggregate, ProductEvent, ProductError, Notification]

  private def run(command: Command, state: ProductAggregate): Result = run(command, state, productId, lateEvening)

  private def run(command: Command, state: ProductAggregate, address: ProductId, at: Instant): Result =
    ProductService[Id].execute(RequestContext(CommandMessage("message-1", at, address.toString, command), state))

  private def rejection(result: Result): List[ProductError] =
    result match
      case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
      case other => fail(s"Expected a rejection, got: $other")

  private def accepted(result: Result): (ProductAggregate, List[ProductEvent], List[Notification]) =
    result match
      case EdomatonResult.Accepted(state, events, notifications) => (state, events.toList, notifications.toList)
      case other => fail(s"Expected an acceptance, got: $other")

  private val movement: MovementId = MovementId(UUID.fromString("00000000-0000-0000-0000-0000000007a1").nn)
  private val code: String :| MpnCode = "AC-100"
  private val reason: String :| MovementReason = "Campionatura"
  private val ref: ConsumptionRef = StockFixtures.consumptionRef(1, 1)

  private def purchase(on: Option[LocalDate]): Command =
    Command.RecordPurchase(movement, code, secondSupplier, Quantity.applyUnsafe(2500L), UnitPrice.applyUnsafe(37000L), Some("DDT 7"), on)

  "ProductInfo" should "normalize codes and derive its distinct suppliers from the MPNs" in:
    (product.internalCode: String) shouldEqual "TES-001"
    product.mpns.map(m => m.code: String) shouldEqual List("AC-100", "AC-100")
    product.suppliers shouldEqual List(firstSupplier, secondSupplier)
    info(1L, UnitOfMeasure.Pieces, List(mpn("A", firstSupplier), mpn("B", firstSupplier))).suppliers shouldEqual List(firstSupplier)

  "ProductService" should "create a product dated on the business day of the command time" in:
    val (state, events, notifications) = accepted(run(Command.Create(product), ProductAggregate.Empty))
    events shouldEqual List(ProductEvent.ProductCreated(product, romeDay))
    state shouldEqual ProductAggregate.Active(ProductRecord(product, romeDay, List(CustomerPricePoint(romeDay, product.customerPrice))))
    notifications shouldEqual List(Notification.ProductChanged(productId))

  it should "reject the same MPN code twice for one supplier, normalized" in:
    val duplicated = info(1L, UnitOfMeasure.Pieces, List(mpn("x-1", firstSupplier), mpn(" X-1 ", firstSupplier), mpn("X-1", secondSupplier)))
    rejection(run(Command.Create(duplicated), ProductAggregate.Empty)) shouldEqual List(ProductError.DuplicateMpn(firstSupplier, "X-1"))
    rejection(run(Command.Update(duplicated), ProductAggregate.Active(record))) shouldEqual List(ProductError.DuplicateMpn(firstSupplier, "X-1"))

  it should "reject creating an existing product" in:
    rejection(run(Command.Create(product), ProductAggregate.Active(record))) shouldEqual List(ProductError.ProductAlreadyExists)
    rejection(run(Command.Create(product), ProductAggregate.Archived(record, romeDay))) shouldEqual List(ProductError.ProductAlreadyExists)

  it should "update an active product, recording the new customer price from the business day" in:
    val repriced = product.copy(customerPrice = UnitPrice.applyUnsafe(40000L), unit = UnitOfMeasure.SquareMeters)
    val (state, events, _) = accepted(run(Command.Update(repriced), ProductAggregate.Active(record)))
    events shouldEqual List(ProductEvent.ProductUpdated(repriced, romeDay))
    state match
      case ProductAggregate.Active(updated) =>
        updated.info shouldEqual repriced
        updated.createdOn shouldEqual record.createdOn
        updated.customerPriceAt(LocalDate.parse("2026-06-14")) shouldEqual Some(product.customerPrice)
        updated.customerPriceAt(romeDay) shouldEqual Some(repriced.customerPrice)
        updated.customerPriceAt(LocalDate.parse("2026-05-31")) shouldEqual Some(product.customerPrice)
      case other => fail(s"Unexpected state: $other")

  it should "reject changes to a missing or archived product" in:
    rejection(run(Command.Update(product), ProductAggregate.Empty)) shouldEqual List(ProductError.ProductNotFound)
    rejection(run(Command.Update(product), ProductAggregate.Archived(record, romeDay))) shouldEqual List(ProductError.ProductIsArchived)
    rejection(run(Command.Archive, ProductAggregate.Empty)) shouldEqual List(ProductError.ProductNotFound)
    rejection(run(Command.Archive, ProductAggregate.Archived(record, romeDay))) shouldEqual List(ProductError.ProductIsArchived)

  it should "archive a product on the business day, keeping its record, and publish its id" in:
    val (state, events, notifications) = accepted(run(Command.Archive, ProductAggregate.Active(record)))
    events shouldEqual List(ProductEvent.ProductArchived(romeDay))
    state shouldEqual ProductAggregate.Archived(record, romeDay)
    notifications shouldEqual List(Notification.ProductChanged(productId))

  it should "reject a product sent to another product's stream" in:
    val otherStream = ProductId(UUID.fromString("00000000-0000-0000-0000-0000000006a2").nn)
    rejection(run(Command.Create(product), ProductAggregate.Empty, otherStream, lateEvening)) shouldEqual List(ProductError.ProductIdMismatch)

  "ProductRecord price history" should "keep one point per day and drop unchanged prices" in:
    val day1 = LocalDate.parse("2026-06-01")
    val day2 = LocalDate.parse("2026-06-02")
    val price = (cents: Long) => product.copy(customerPrice = UnitPrice.applyUnsafe(cents))
    val history = ProductRecord
      .created(price(100L), day1)
      .updated(price(100L), day1) // same price: no new point
      .updated(price(200L), day2)
      .updated(price(300L), day2) // same day: last price wins
      .updated(price(300L), day2.plusDays(1).nn) // unchanged: no new point
      .customerPrices
    history shouldEqual List(CustomerPricePoint(day1, UnitPrice.applyUnsafe(100L)), CustomerPricePoint(day2, UnitPrice.applyUnsafe(300L)))

  "ProductAggregate fold" should "replay the whole lifecycle" in:
    val events = List(
      ProductEvent.ProductCreated(product, LocalDate.parse("2026-06-01")),
      ProductEvent.ProductUpdated(product, LocalDate.parse("2026-06-02")),
      ProductEvent.ProductArchived(LocalDate.parse("2026-06-03")),
    )
    val folded = events.foldLeft(ProductAggregate.initial.validNec[ProductError])((state, event) => state.andThen(ProductAggregate.transition(event)))
    folded.toOption shouldEqual Some(ProductAggregate.Archived(record, LocalDate.parse("2026-06-03")))
  "ProductService movements" should "date a movement without a day on the business day of the command, and publish the product" in:
    val (state, events, notifications) = accepted(run(purchase(None), ProductAggregate.Active(record)))
    events shouldEqual List(
      ProductEvent.PurchaseRecorded(
        movement,
        code,
        secondSupplier,
        Quantity.applyUnsafe(2500L),
        UnitPrice.applyUnsafe(37000L),
        Amount.applyUnsafe(925L),
        romeDay,
        Some("DDT 7"),
      ),
    )
    notifications shouldEqual List(Notification.ProductChanged(productId))
    state match
      case ProductAggregate.Active(product) =>
        product.hasMovements shouldEqual true
        product.ledger.balanceAt(romeDay) shouldEqual SignedQuantity.applyUnsafe(2500L)
      case other => fail(s"Unexpected state: $other")

  it should "keep a backdated day and reject a day after the command's business day" in:
    val (_, events, _) = accepted(run(purchase(Some(LocalDate.parse("2026-06-10"))), ProductAggregate.Active(record)))
    events.collect { case event: ProductEvent.PurchaseRecorded => event.on } shouldEqual List(LocalDate.parse("2026-06-10"))
    rejection(run(purchase(Some(LocalDate.parse("2026-06-16"))), ProductAggregate.Active(record))) shouldEqual
      List(ProductError.MovementInFuture(LocalDate.parse("2026-06-16"), romeDay))

  it should "lock the unit of measure once a movement exists, even voided" in:
    val (bought, _, _) = accepted(run(purchase(None), ProductAggregate.Active(record)))
    val (voided, _, _) = accepted(run(Command.VoidMovement(movement, reason), bought))
    val otherUnit = product.copy(unit = UnitOfMeasure.SquareMeters)
    rejection(run(Command.Update(otherUnit), voided)) shouldEqual List(ProductError.UnitChangeWithMovements)
    accepted(run(Command.Update(otherUnit), ProductAggregate.Active(record)))

  it should "reject manual unloads beyond the stock and accept them within it" in:
    val (bought, _, _) = accepted(run(purchase(None), ProductAggregate.Active(record)))
    val other = MovementId(UUID.fromString("00000000-0000-0000-0000-0000000007a2").nn)
    rejection(run(Command.Unload(other, Quantity.applyUnsafe(2501L), reason, None), bought)) shouldEqual
      List(ProductError.InsufficientStock(romeDay, SignedQuantity.applyUnsafe(2500L)))
    val (_, events, _) = accepted(run(Command.Unload(other, Quantity.applyUnsafe(2500L), reason, None), bought))
    events shouldEqual List(ProductEvent.StockUnloaded(other, Quantity.applyUnsafe(2500L), reason, romeDay))

  it should "reject movements on an archived product" in:
    rejection(run(purchase(None), ProductAggregate.Archived(record, romeDay))) shouldEqual List(ProductError.ProductIsArchived)
    rejection(run(Command.Adjust(movement, SignedQuantity.applyUnsafe(1000L), None, reason, None), ProductAggregate.Empty)) shouldEqual
      List(ProductError.ProductNotFound)

  "ProductService consumptions" should "decide nothing, and publish nothing, when the quantity is already the requested one" in:
    val (consumed, events, _) = accepted(run(Command.SetConsumption(ref, Quantity.applyUnsafe(1500L), romeDay), ProductAggregate.Active(record)))
    events shouldEqual List(ProductEvent.ConsumptionSet(ref, Quantity.applyUnsafe(1500L), romeDay))
    run(Command.SetConsumption(ref, Quantity.applyUnsafe(1500L), romeDay.minusDays(3).nn), consumed) shouldEqual EdomatonResult.Indecisive(
      Chain.empty,
    )
    run(Command.SetConsumption(ref, Quantity.zero, romeDay), ProductAggregate.Active(record)) shouldEqual EdomatonResult.Indecisive(Chain.empty)

  it should "be accepted on an archived product, beyond its stock" in:
    val (state, events, notifications) =
      accepted(run(Command.SetConsumption(ref, Quantity.applyUnsafe(2000L), romeDay), ProductAggregate.Archived(record, romeDay)))
    events shouldEqual List(ProductEvent.ConsumptionSet(ref, Quantity.applyUnsafe(2000L), romeDay))
    notifications shouldEqual List(Notification.ProductChanged(productId))
    state match
      case ProductAggregate.Archived(product, _) => product.ledger.balanceAt(romeDay) shouldEqual SignedQuantity.applyUnsafe(-2000L)
      case other => fail(s"Unexpected state: $other")

  it should "reject the manual void of a consumption" in:
    val (consumed, _, _) = accepted(run(Command.SetConsumption(ref, Quantity.applyUnsafe(1500L), romeDay), ProductAggregate.Active(record)))
    val consumption = StockLedger.consumptionMovementId(ref, 0)
    rejection(run(Command.VoidMovement(consumption, reason), consumed)) shouldEqual List(ProductError.ConsumptionNotVoidable(consumption))
end ProductServiceTest
