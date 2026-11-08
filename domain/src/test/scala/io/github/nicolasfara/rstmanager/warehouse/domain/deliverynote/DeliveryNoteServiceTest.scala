package io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.DeliveryNoteService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events.DeliveryNoteEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialId, MpnCode }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.Id
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class DeliveryNoteServiceTest extends AnyFlatSpecLike:
  private val noteId: DeliveryNoteId = DeliveryNoteId(UUID.fromString("00000000-0000-0000-0000-000000000b01").nn)
  private val supplierId: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-000000000301").nn)
  private val materialId: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000401").nn)
  private val code: String :| MpnCode = "AC-152-NAVY"

  /** 23:30 UTC on 14 June is already 15 June in Rome. */
  private val lateEvening: Instant = Instant.parse("2026-06-14T23:30:00Z").nn
  private val romeDay: LocalDate = LocalDate.parse("2026-06-15")

  private val line: DeliveryNoteLine =
    DeliveryNoteLine(materialId, code, Quantity.applyUnsafe(25500L), UnitOfMeasure.LinearMeters, UnitPrice.applyUnsafe(35000L))

  private val info: DeliveryNoteInfo =
    DeliveryNoteInfo
      .createDeliveryNoteInfo(noteId, " 1234 ", LocalDate.parse("2026-06-12"), supplierId, Some("  "), List(line, line))
      .fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private val registered: DeliveryNoteAggregate = DeliveryNoteAggregate.Registered(DeliveryNoteRecord(info, romeDay, None))

  private type Result = EdomatonResult[DeliveryNoteAggregate, DeliveryNoteEvent, DeliveryNoteError, Notification]

  private def run(command: Command, state: DeliveryNoteAggregate): Result = run(command, state, noteId)

  private def run(command: Command, state: DeliveryNoteAggregate, address: DeliveryNoteId): Result =
    DeliveryNoteService[Id].execute(RequestContext(CommandMessage("message-1", lateEvening, address.toString, command), state))

  private def rejection(result: Result): List[DeliveryNoteError] = result match
    case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
    case other => fail(s"Expected a rejection, got: $other")

  "DeliveryNoteInfo" should "trim the number, drop a blank note, and total its lines" in:
    (info.number: String) shouldEqual "1234"
    info.note shouldEqual None
    info.total shouldEqual Amount.applyUnsafe(17850L) // 2 × 25.5 m × 3.50 €
    info.ref.number shouldEqual info.number
    DeliveryNoteInfo.createDeliveryNoteInfo(noteId, " ", romeDay, supplierId, None, Nil).isInvalid shouldEqual true

  "DeliveryNoteService" should "register a note on the business day of the command and publish its id" in:
    run(Command.Register(info), DeliveryNoteAggregate.Empty) match
      case EdomatonResult.Accepted(state, events, notifications) =>
        state shouldEqual registered
        events.toList shouldEqual List(DeliveryNoteEvent.DeliveryNoteRegistered(info, romeDay))
        notifications.toList shouldEqual List(Notification.DeliveryNoteChanged(noteId))
      case other => fail(s"Unexpected result: $other")

  it should "reject a note without lines, with a zero quantity, or dated after today" in:
    rejection(run(Command.Register(info.copy(lines = Nil)), DeliveryNoteAggregate.Empty)) shouldEqual List(DeliveryNoteError.NoLines)
    rejection(run(Command.Register(info.copy(lines = List(line.copy(quantity = Quantity.zero)))), DeliveryNoteAggregate.Empty)) shouldEqual
      List(DeliveryNoteError.NonPositiveQuantity)
    val tomorrow = romeDay.plusDays(1).nn
    rejection(run(Command.Register(info.copy(date = tomorrow)), DeliveryNoteAggregate.Empty)) shouldEqual
      List(DeliveryNoteError.DeliveryNoteInFuture(tomorrow, romeDay))

  it should "reject registering an existing note and a note sent to another stream" in:
    rejection(run(Command.Register(info), registered)) shouldEqual List(DeliveryNoteError.DeliveryNoteAlreadyExists)
    val other = DeliveryNoteId(UUID.fromString("00000000-0000-0000-0000-000000000b02").nn)
    rejection(run(Command.Register(info), DeliveryNoteAggregate.Empty, other)) shouldEqual List(DeliveryNoteError.DeliveryNoteIdMismatch)
    rejection(run(Command.Change(info), registered, other)) shouldEqual List(DeliveryNoteError.DeliveryNoteIdMismatch)

  it should "change a registered note, and do nothing when the data is the same" in:
    val changed = info.copy(lines = List(line))
    run(Command.Change(changed), registered) match
      case EdomatonResult.Accepted(state, events, notifications) =>
        state shouldEqual DeliveryNoteAggregate.Registered(DeliveryNoteRecord(changed, romeDay, Some(romeDay)))
        events.toList shouldEqual List(DeliveryNoteEvent.DeliveryNoteChanged(changed, romeDay))
        notifications.toList shouldEqual List(Notification.DeliveryNoteChanged(noteId))
      case other => fail(s"Unexpected result: $other")
    run(Command.Change(info), registered) match
      case EdomatonResult.Indecisive(notifications) => notifications.toList shouldEqual Nil
      case other => fail(s"Unexpected result: $other")
    rejection(run(Command.Change(info), DeliveryNoteAggregate.Empty)) shouldEqual List(DeliveryNoteError.DeliveryNoteNotFound)

  it should "cancel a note, keeping it, and reject any later change" in:
    val reason: String :| CancellationReason = "Registrato due volte"
    run(Command.Cancel(reason), registered) match
      case EdomatonResult.Accepted(state, _, notifications) =>
        state shouldEqual DeliveryNoteAggregate.Cancelled(DeliveryNoteRecord(info, romeDay, None), romeDay, reason)
        notifications.toList shouldEqual List(Notification.DeliveryNoteChanged(noteId))
        rejection(run(Command.Cancel(reason), state)) shouldEqual List(DeliveryNoteError.DeliveryNoteIsCancelled)
        rejection(run(Command.Change(info), state)) shouldEqual List(DeliveryNoteError.DeliveryNoteIsCancelled)
      case other => fail(s"Unexpected result: $other")
    rejection(run(Command.Cancel(reason), DeliveryNoteAggregate.Empty)) shouldEqual List(DeliveryNoteError.DeliveryNoteNotFound)

  "DeliveryNoteAggregate fold" should "replay the whole lifecycle" in:
    val changed = info.copy(lines = List(line))
    val events = List(
      DeliveryNoteEvent.DeliveryNoteRegistered(info, romeDay),
      DeliveryNoteEvent.DeliveryNoteChanged(changed, romeDay.plusDays(1).nn),
      DeliveryNoteEvent.DeliveryNoteCancelled("Doppio", romeDay.plusDays(2).nn),
    )
    val folded = events.foldLeft(DeliveryNoteAggregate.initial.validNec[DeliveryNoteError])((state, event) =>
      state.andThen(DeliveryNoteAggregate.transition(event)),
    )
    folded.toOption shouldEqual
      Some(DeliveryNoteAggregate.Cancelled(DeliveryNoteRecord(changed, romeDay, Some(romeDay.plusDays(1).nn)), romeDay.plusDays(2).nn, "Doppio"))
end DeliveryNoteServiceTest
