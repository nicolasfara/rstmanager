package io.github.nicolasfara.rstmanager.warehouse.domain.material

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, SignedQuantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.StockFixtures.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.Decision
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StockLedgerTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:
  private val today: LocalDate = day(30)
  private val meters: MaterialRecord = MaterialRecord.created(materialInfo(materialId(1), "TES-1", UnitOfMeasure.LinearMeters), base)
  private val pieces: MaterialRecord = MaterialRecord.created(materialInfo(materialId(2), "VIT-1", UnitOfMeasure.Pieces), base)

  private type Result = Decision[MaterialError, MaterialEvent, MaterialAggregate]

  private def q(thousandths: Long): Quantity = Quantity.applyUnsafe(thousandths)
  private def price(units: Long): UnitPrice = UnitPrice.applyUnsafe(units)

  private def accepted(result: Result): MaterialAggregate = result match
    case Decision.Accepted(_, state) => state
    case other => fail(s"Expected an acceptance, got: $other")

  private def rejection(result: Result): List[MaterialError] = result match
    case Decision.Rejected(reasons) => reasons.toChain.toList
    case other => fail(s"Expected a rejection, got: $other")

  private def ledger(state: MaterialAggregate): StockLedger = state match
    case MaterialAggregate.Active(material) => material.ledger
    case other => fail(s"Unexpected state: $other")

  private def buy(state: MaterialAggregate, id: Int, quantity: Long, cost: Long, on: LocalDate): MaterialAggregate =
    accepted(state.recordPurchase(movementId(id), mpnCode, firstSupplier, q(quantity), price(cost), None, on, today))

  private def unload(state: MaterialAggregate, id: Int, quantity: Long, on: LocalDate): Result =
    state.unload(movementId(id), q(quantity), reason, on, today)

  "A purchase" should "add stock, compute its total HALF_UP, and become a price point" in:
    val state = buy(MaterialAggregate.Active(meters), 1, 333L, 1000L, day(2)) // 0.333 m × 0.1000 € = 0.0333 € → 0.03 €
    val stock = ledger(state).snapshotAt(day(2))
    stock shouldEqual StockSnapshot(SignedQuantity.applyUnsafe(333L), Some(price(1000L)), Some(Amount.applyUnsafe(3L)), Amount.applyUnsafe(3L))
    ledger(state).snapshotAt(day(1)) shouldEqual StockSnapshot.empty

  it should "only use one of the material's MPNs" in:
    val otherSupplier = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005b9").nn)
    rejection(
      MaterialAggregate.Active(meters).recordPurchase(movementId(1), mpnCode, otherSupplier, q(1000L), price(1L), None, day(1), today),
    ) shouldEqual
      List(MaterialError.UnknownMpn(otherSupplier, "AC-100"))

  "A movement" should "be rejected when dated after today" in:
    rejection(unload(MaterialAggregate.Active(meters), 1, 1000L, today.plusDays(1).nn)) should contain(
      MaterialError.MovementInFuture(today.plusDays(1).nn, today),
    )

  it should "be accepted when dated before the material was created" in:
    accepted(
      MaterialAggregate.Active(meters).recordPurchase(movementId(1), mpnCode, firstSupplier, q(1000L), price(1L), None, base.minusDays(5).nn, today),
    )

  it should "use whole quantities for a material counted in pieces" in:
    rejection(
      MaterialAggregate.Active(pieces).recordPurchase(movementId(1), mpnCode, firstSupplier, q(1500L), price(1L), None, day(1), today),
    ) shouldEqual
      List(MaterialError.FractionalPieces)
    rejection(MaterialAggregate.Active(pieces).adjust(movementId(1), SignedQuantity.applyUnsafe(2001L), None, reason, day(1), today)) shouldEqual
      List(MaterialError.FractionalPieces)
    accepted(MaterialAggregate.Active(pieces).recordPurchase(movementId(1), mpnCode, firstSupplier, q(2000L), price(1L), None, day(1), today))

  it should "need a non-zero quantity and an unused id" in:
    rejection(MaterialAggregate.Active(meters).adjust(movementId(1), SignedQuantity.zero, None, reason, day(1), today)) shouldEqual
      List(MaterialError.NonPositiveQuantity)
    val bought = buy(MaterialAggregate.Active(meters), 1, 1000L, 1L, day(1))
    rejection(bought.adjust(movementId(1), SignedQuantity.applyUnsafe(1L), None, reason, day(1), today)) shouldEqual
      List(MaterialError.DuplicateMovement(movementId(1)))

  it should "be rejected on an archived material" in:
    rejection(unload(MaterialAggregate.Archived(meters, day(1)), 1, 1000L, day(1))) shouldEqual List(MaterialError.MaterialIsArchived)
    rejection(MaterialAggregate.Archived(meters, day(1)).voidMovement(movementId(1), reason, today)) shouldEqual List(
      MaterialError.MaterialIsArchived,
    )

  "An unload" should "be rejected when stock is insufficient on its day" in:
    val bought = buy(MaterialAggregate.Active(meters), 1, 5000L, 1L, day(5))
    rejection(unload(bought, 2, 1000L, day(4))) shouldEqual List(MaterialError.InsufficientStock(day(4), SignedQuantity.zero))
    rejection(unload(bought, 2, 5001L, day(5))) shouldEqual List(MaterialError.InsufficientStock(day(5), SignedQuantity.applyUnsafe(5000L)))

  it should "be rejected when backdated so that a later balance would go negative" in:
    val bought = buy(MaterialAggregate.Active(meters), 1, 10000L, 1L, day(1))
    val unloaded = accepted(unload(bought, 2, 8000L, day(5)))
    // On day 3 the balance would be 7, but from day 5 on it would be -1: at most 2 can go out from day 3.
    rejection(unload(unloaded, 3, 3000L, day(3))) shouldEqual List(MaterialError.InsufficientStock(day(3), SignedQuantity.applyUnsafe(2000L)))
    ledger(accepted(unload(unloaded, 3, 2000L, day(3)))).balanceAt(today) shouldEqual SignedQuantity.zero

  "An adjustment" should "record a cost only when it brings stock in, and then count as a price point" in:
    rejection(
      MaterialAggregate.Active(meters).adjust(movementId(1), SignedQuantity.applyUnsafe(-1000L), Some(price(1L)), reason, day(1), today),
    ) should
      contain(MaterialError.CostOnOutgoingAdjustment)
    val opening =
      accepted(MaterialAggregate.Active(meters).adjust(movementId(1), SignedQuantity.applyUnsafe(4000L), Some(price(32000L)), reason, day(1), today))
    ledger(opening).snapshotAt(day(1)) shouldEqual
      StockSnapshot(SignedQuantity.applyUnsafe(4000L), Some(price(32000L)), Some(Amount.applyUnsafe(1280L)), Amount.zero)
    rejection(opening.adjust(movementId(2), SignedQuantity.applyUnsafe(-4001L), None, reason, day(2), today)) shouldEqual
      List(MaterialError.InsufficientStock(day(2), SignedQuantity.applyUnsafe(4000L)))

  "Voiding" should "remove a purchase from every day, lowering the highest cost" in:
    val first = buy(MaterialAggregate.Active(meters), 1, 10000L, 30000L, day(1))
    val second = buy(first, 2, 5000L, 35000L, day(3))
    ledger(second).maxCostAt(day(5)) shouldEqual Some(price(35000L))
    val voided = accepted(second.voidMovement(movementId(2), reason, today))
    ledger(voided).maxCostAt(day(5)) shouldEqual Some(price(30000L))
    ledger(voided).balanceAt(day(5)) shouldEqual SignedQuantity.applyUnsafe(10000L)
    ledger(voided).find(movementId(2)).flatMap(_.voiding) shouldEqual Some(MovementVoid(reason, today))

  it should "be rejected when the stock would go negative without the purchase" in:
    val bought = buy(MaterialAggregate.Active(meters), 1, 10000L, 1L, day(1))
    val unloaded = accepted(unload(bought, 2, 4000L, day(10)))
    rejection(unloaded.voidMovement(movementId(1), reason, today)) shouldEqual List(
      MaterialError.InsufficientStock(day(1), SignedQuantity.applyUnsafe(6000L)),
    )
    accepted(unloaded.voidMovement(movementId(2), reason, today))

  it should "be rejected for an unknown or already voided movement" in:
    val bought = buy(MaterialAggregate.Active(meters), 1, 1000L, 1L, day(1))
    rejection(bought.voidMovement(movementId(9), reason, today)) shouldEqual List(MaterialError.MovementNotFound(movementId(9)))
    val voided = accepted(bought.voidMovement(movementId(1), reason, today))
    rejection(voided.voidMovement(movementId(1), reason, today)) shouldEqual List(MaterialError.MovementAlreadyVoided(movementId(1)))

  "The ledger lines" should "list every movement by day then recording order, with the running balance" in:
    val recorded = ledgerOf(
      List(
        RawMovement(5, SignedQuantity.applyUnsafe(-1000L), MovementKind.Unload(reason), voided = false),
        RawMovement(1, purchase(3000L, 10000L, firstSupplier)._1, purchase(3000L, 10000L, firstSupplier)._2, voided = false),
        RawMovement(5, SignedQuantity.applyUnsafe(500L), MovementKind.Adjustment(None, reason), voided = true),
      ),
    )
    val lines = recorded.lines
    lines.map(_.movement.id) shouldEqual List(movementId(1), movementId(0), movementId(2))
    lines.map(_.balance.value) shouldEqual List(3000L, 2000L, 2000L)
    lines.map(_.value.map(_.value)) shouldEqual List(Some(300L), Some(-100L), None)

  "StockLedger" should "have a far-future balance equal to the sum of the non-voided deltas" in:
    forAll(Gen.listOf(genMovement(60, voids = true))) { raw =>
      ledgerOf(raw).balanceAt(day(10000)).value shouldEqual raw.filterNot(_.voided).map(_.delta.value: Long).sum
    }

  it should "keep the snapshot of a day when movements after it are added" in:
    forAll(genLedger(60, voids = true), Gen.chooseNum(0, 60), Gen.listOf(genMovement(30, voids = true))) { (recorded, offset, later) =>
      val shifted = later.map(movement => movement.copy(offset = offset + 1 + movement.offset))
      ledgerOf(shifted, 10000, recorded).snapshotAt(day(offset)) shouldEqual recorded.snapshotAt(day(offset))
    }

  it should "never lower the highest cost as days go by, without voids" in:
    forAll(genLedger(60, voids = false), Gen.chooseNum(0, 60), Gen.chooseNum(0, 60)) { (recorded, a, b) =>
      recorded.maxCostAt(day(a.max(b))).map(_.value).getOrElse(-1L) should be >= recorded.maxCostAt(day(a.min(b))).map(_.value).getOrElse(-1L)
    }

  it should "have a minimum balance from a day equal to the lowest daily balance from then on" in:
    forAll(genLedger(40, voids = true), Gen.chooseNum(0, 40)) { (recorded, offset) =>
      recorded.minBalanceFrom(day(offset)).value shouldEqual (offset to 41).map(d => recorded.balanceAt(day(d)).value).min
    }

  "The fold" should "append movements and voids on an archived material too" in:
    val events = List(
      MaterialEvent.MaterialCreated(meters.info, base),
      MaterialEvent.PurchaseRecorded(movementId(1), mpnCode, firstSupplier, q(2000L), price(10000L), Amount.applyUnsafe(200L), day(1), None),
      MaterialEvent.MaterialArchived(day(2)),
      MaterialEvent.MovementVoided(movementId(1), reason, day(3)),
    )
    val folded: ValidatedNec[MaterialError, MaterialAggregate] =
      events.foldLeft(MaterialAggregate.initial.validNec[MaterialError])((state, event) => state.andThen(MaterialAggregate.transition(event)))
    folded.toOption.collect { case MaterialAggregate.Archived(material, _) => material.ledger.active.size } shouldEqual Some(0)
end StockLedgerTest
