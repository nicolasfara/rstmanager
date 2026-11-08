package io.github.nicolasfara.rstmanager.warehouse.domain.material

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.{ DeliveryNoteId, DeliveryNoteNumber }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.StockFixtures.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.Id
import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.{ CommandMessage, Decision, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class DeliveryNotePurchasesTest extends AnyFlatSpecLike:
  private val today: LocalDate = day(30)
  private val noteId: DeliveryNoteId = DeliveryNoteId(new UUID(0xdd7L, 1L))
  private val number: String :| DeliveryNoteNumber = "1234"
  private val ref: DeliveryNoteRef = DeliveryNoteRef(noteId, number)
  private val meters: MaterialRecord = MaterialRecord.created(materialInfo(materialId(1), "TES-1", UnitOfMeasure.LinearMeters), base)
  private val pieces: MaterialRecord = MaterialRecord.created(materialInfo(materialId(2), "VIT-1", UnitOfMeasure.Pieces), base)
  private val active: MaterialAggregate = MaterialAggregate.Active(meters)

  private type Result = Decision[MaterialError, MaterialEvent, MaterialAggregate]

  private def q(thousandths: Long): Quantity = Quantity.applyUnsafe(thousandths)
  private def price(units: Long): UnitPrice = UnitPrice.applyUnsafe(units)

  private def line(quantity: Long, cost: Long, on: LocalDate): DeliveryNotePurchaseLine =
    DeliveryNotePurchaseLine(mpnCode, firstSupplier, q(quantity), price(cost), on)

  private def accepted(result: Result): (List[MaterialEvent], MaterialAggregate) = result match
    case Decision.Accepted(events, state) => (events.toList, state)
    case other => fail(s"Expected an acceptance, got: $other")

  private def rejection(result: Result): List[MaterialError] = result match
    case Decision.Rejected(reasons) => reasons.toChain.toList
    case other => fail(s"Expected a rejection, got: $other")

  private def recordOf(state: MaterialAggregate): MaterialRecord = state match
    case MaterialAggregate.Active(material) => material
    case MaterialAggregate.Archived(material, _) => material
    case MaterialAggregate.Empty => fail("Unexpected empty material")

  private def set(state: MaterialAggregate, lines: List[DeliveryNotePurchaseLine]): MaterialAggregate =
    accepted(state.setDeliveryNotePurchases(ref, lines, today))._2

  /** 10 m at 3.00 €/m and 2 m at 3.50 €/m, both on day 5. */
  private val twoLines: List[DeliveryNotePurchaseLine] = List(line(10000L, 30000L, day(5)), line(2000L, 35000L, day(5)))

  "setDeliveryNotePurchases" should "record each line as a purchase of the note, with deterministic ids" in:
    val (events, state) = accepted(active.setDeliveryNotePurchases(ref, twoLines, today))
    events shouldEqual List(MaterialEvent.DeliveryNotePurchasesSet(ref, twoLines.map(_.priced), today))
    val ledger = recordOf(state).ledger
    ledger.movements.map(movement => (movement.id, movement.on, movement.delta.value, movement.deliveryNote)) shouldEqual Vector(
      (StockLedger.deliveryNoteMovementId(noteId, 0), day(5), 10000L, Some(ref)),
      (StockLedger.deliveryNoteMovementId(noteId, 1), day(5), 2000L, Some(ref)),
    )
    ledger.balanceAt(today).value shouldEqual 12000L
    ledger.purchasedTotalAt(today) shouldEqual Amount.applyUnsafe(3700L)
    ledger.deliveryNotes shouldEqual Map(noteId -> AppliedDeliveryNote(ref, twoLines.map(_.priced)))

  it should "void the purchases of a changed note and record the new ones" in:
    val changed = set(set(active, twoLines), List(line(8000L, 30000L, day(6))))
    val ledger = recordOf(changed).ledger
    ledger.movements.map(movement => (movement.id, movement.delta.value, movement.voiding)) shouldEqual Vector(
      (StockLedger.deliveryNoteMovementId(noteId, 0), 10000L, Some(MovementVoid(StockLedger.deliveryNoteChangedReason, today))),
      (StockLedger.deliveryNoteMovementId(noteId, 1), 2000L, Some(MovementVoid(StockLedger.deliveryNoteChangedReason, today))),
      (StockLedger.deliveryNoteMovementId(noteId, 2), 8000L, None),
    )
    ledger.balanceAt(day(5)).value shouldEqual 0L
    ledger.balanceAt(day(6)).value shouldEqual 8000L

  it should "remove the note from the material with an empty list" in:
    val removed = recordOf(set(set(active, twoLines), Nil)).ledger
    removed.active shouldEqual Vector.empty
    removed.deliveryNotes shouldEqual Map.empty
    removed.balanceAt(today).value shouldEqual 0L

  it should "decide nothing when the note already brings the same purchases" in:
    val stocked = set(active, twoLines)
    stocked.setDeliveryNotePurchases(ref, twoLines, today) shouldEqual Decision.pure(stocked)
    active.setDeliveryNotePurchases(ref, Nil, today) shouldEqual Decision.pure(active)

  it should "record the purchases again under a new number" in:
    val renumbered = DeliveryNoteRef(noteId, "1234/A")
    val (_, state) = accepted(set(active, twoLines).setDeliveryNotePurchases(renumbered, twoLines, today))
    recordOf(state).ledger.active.map(_.deliveryNote) shouldEqual Vector(Some(renumbered), Some(renumbered))

  it should "add an MPN the material does not have yet, once, at the cost of its first line" in:
    val newCode: String :| MpnCode = "NEW-1"
    val lines = List(
      DeliveryNotePurchaseLine(newCode, firstSupplier, q(1000L), price(40000L), day(5)),
      DeliveryNotePurchaseLine(newCode, firstSupplier, q(1000L), price(45000L), day(5)),
      line(1000L, 30000L, day(5)),
    )
    val (events, state) = accepted(active.setDeliveryNotePurchases(ref, lines, today))
    val added = Mpn(newCode, firstSupplier, None, Some(price(40000L)))
    events shouldEqual List(MaterialEvent.MpnAdded(added, today), MaterialEvent.DeliveryNotePurchasesSet(ref, lines.map(_.priced), today))
    recordOf(state).info.mpns.lastOption shouldEqual Some(added)
    recordOf(state).info.checkInvariants.isValid shouldEqual true

  it should "check the day, the quantity, and the whole pieces of every line" in:
    rejection(active.setDeliveryNotePurchases(ref, List(line(1000L, 1L, today.plusDays(1).nn)), today)) shouldEqual
      List(MaterialError.MovementInFuture(today.plusDays(1).nn, today))
    rejection(active.setDeliveryNotePurchases(ref, List(line(0L, 1L, day(5))), today)) shouldEqual List(MaterialError.NonPositiveQuantity)
    rejection(MaterialAggregate.Active(pieces).setDeliveryNotePurchases(ref, List(line(1500L, 1L, day(5))), today)) shouldEqual
      List(MaterialError.FractionalPieces)

  it should "reject a change that leaves the stock below zero, with the stock available from the earliest day" in:
    val stocked = set(active, List(line(10000L, 30000L, day(5))))
    val unloaded = accepted(stocked.unload(movementId(1), q(8000L), reason, day(10), today))._2
    rejection(unloaded.setDeliveryNotePurchases(ref, List(line(5000L, 30000L, day(5))), today)) shouldEqual
      List(MaterialError.InsufficientStock(day(5), SignedQuantity.applyUnsafe(2000L)))
    rejection(unloaded.setDeliveryNotePurchases(ref, Nil, today)) shouldEqual
      List(MaterialError.InsufficientStock(day(5), SignedQuantity.applyUnsafe(2000L)))
    accepted(unloaded.setDeliveryNotePurchases(ref, List(line(8000L, 30000L, day(5))), today))

  it should "accept a change that leaves a negative stock no lower than it was" in:
    val stocked = set(active, List(line(10000L, 30000L, day(5))))
    val overConsumed = accepted(stocked.setConsumption(consumptionRef(1, 1), q(15000L), day(10), today))._2
    val (_, state) = accepted(overConsumed.setDeliveryNotePurchases(ref, List(line(12000L, 30000L, day(5))), today))
    recordOf(state).ledger.balanceAt(today).value shouldEqual -3000L

  it should "only let the note go on an archived material" in:
    val archived = MaterialAggregate.Archived(recordOf(set(active, twoLines)), day(20))
    archived.setDeliveryNotePurchases(ref, twoLines, today) shouldEqual Decision.pure(archived)
    rejection(archived.setDeliveryNotePurchases(ref, List(line(1000L, 1L, day(5))), today)) shouldEqual List(MaterialError.MaterialIsArchived)
    rejection(MaterialAggregate.Archived(meters, day(20)).setDeliveryNotePurchases(ref, twoLines, today)) shouldEqual
      List(MaterialError.MaterialIsArchived)
    accepted(archived.setDeliveryNotePurchases(ref, Nil, today))._2 match
      case MaterialAggregate.Archived(material, archivedOn) =>
        archivedOn shouldEqual day(20)
        material.ledger.active shouldEqual Vector.empty
      case other => fail(s"Unexpected state: $other")
    rejection(MaterialAggregate.Empty.setDeliveryNotePurchases(ref, twoLines, today)) shouldEqual List(MaterialError.MaterialNotFound)

  "Voiding a purchase of a delivery note" should "be rejected: the note owns it" in:
    val stocked = set(active, twoLines)
    val movement = StockLedger.deliveryNoteMovementId(noteId, 0)
    rejection(stocked.voidMovement(movement, reason, today)) shouldEqual List(MaterialError.DeliveryNoteMovementNotVoidable(movement))

  "The fold" should "replay the delivery note events into the same movements" in:
    val added = Mpn("NEW-1", firstSupplier, None, Some(price(40000L)))
    val events = List(
      MaterialEvent.MaterialCreated(meters.info, base),
      MaterialEvent.MpnAdded(added, day(3)),
      MaterialEvent.DeliveryNotePurchasesSet(ref, twoLines.map(_.priced), day(5)),
      MaterialEvent.DeliveryNotePurchasesSet(ref, List(line(8000L, 30000L, day(5)).priced), day(6)),
    )
    val folded: ValidatedNec[MaterialError, MaterialAggregate] =
      events.foldLeft(MaterialAggregate.initial.validNec[MaterialError])((state, event) => state.andThen(MaterialAggregate.transition(event)))
    folded.toOption
      .map(recordOf)
      .map(record => (record.info.mpns.lastOption, record.ledger.active.map(_.id), record.ledger.balanceAt(today).value)) shouldEqual
      Some((Some(added), Vector(StockLedger.deliveryNoteMovementId(noteId, 2)), 8000L))

  "MaterialService.SetDeliveryNotePurchases" should "publish only when the purchases change" in:
    val at = Instant.parse("2026-01-31T10:00:00Z").nn // day 30
    def run(state: MaterialAggregate) =
      MaterialService[Id].execute(
        RequestContext(CommandMessage("message-1", at, materialId(1).toString, Command.SetDeliveryNotePurchases(ref, twoLines)), state),
      )
    run(active) match
      case EdomatonResult.Accepted(_, emitted, notifications) =>
        emitted.toList shouldEqual List(MaterialEvent.DeliveryNotePurchasesSet(ref, twoLines.map(_.priced), today))
        notifications.toList shouldEqual List(Notification.MaterialChanged(materialId(1)))
      case other => fail(s"Unexpected result: $other")
    run(set(active, twoLines)) match
      case EdomatonResult.Indecisive(notifications) => notifications.toList shouldEqual Nil
      case other => fail(s"Unexpected result: $other")

  "Mpn.createMpn" should "keep the expected unit cost" in:
    Mpn.createMpn(" new-1 ", SupplierId(new UUID(1L, 1L)), Some(" "), Some(price(12500L))).toOption shouldEqual
      Some(Mpn("NEW-1", SupplierId(new UUID(1L, 1L)), None, Some(price(12500L))))
end DeliveryNotePurchasesTest
