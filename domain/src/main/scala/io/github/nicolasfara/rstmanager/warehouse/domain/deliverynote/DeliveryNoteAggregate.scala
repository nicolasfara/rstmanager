package io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote

import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.DeliveryNoteAggregate.*
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.DeliveryNoteError.*
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events.DeliveryNoteEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events.DeliveryNoteEvent.*

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.*
import edomata.syntax.all.*
import io.github.iltotore.iron.:|

/**
 * A registered delivery note: its current data and the days it was registered and last changed.
 *
 * @param changedOn
 *   Business day of the last change, absent while the note was never changed.
 */
final case class DeliveryNoteRecord(info: DeliveryNoteInfo, registeredOn: LocalDate, changedOn: Option[LocalDate]) derives CanEqual

/**
 * Event-sourced aggregate holding one delivery note (DDT), the source of truth of the purchases it brings to the materials.
 *
 * Lifecycle:
 *   - [[DeliveryNoteAggregate.Empty]] before the note is registered.
 *   - [[DeliveryNoteAggregate.Registered]] holds the current note; it may be changed any number of times.
 *   - [[DeliveryNoteAggregate.Cancelled]] keeps the last note, the day and the reason of the cancellation, for the history: a cancelled note brings
 *     nothing (the remedy for a note registered twice) and rejects further changes.
 *
 * The purchases are recorded on the materials by the warehouse reconciliation with the notes
 * ([[io.github.nicolasfara.rstmanager.warehouse.domain.DeliveryNoteReconciliation]]), never by this aggregate.
 */
enum DeliveryNoteAggregate derives CanEqual:
  case Empty
  case Registered(note: DeliveryNoteRecord)
  case Cancelled(note: DeliveryNoteRecord, cancelledOn: LocalDate, reason: String :| CancellationReason)

  /** Registers the note on business day `today`, provided it satisfies [[DeliveryNoteInfo.checkInvariants]] and is not dated after `today`. */
  def register(info: DeliveryNoteInfo, today: LocalDate): Decision[DeliveryNoteError, DeliveryNoteEvent, DeliveryNoteAggregate] = this.decide {
    case Empty => rules(info, today).toDecision *> DeliveryNoteRegistered(info, today).accept
    case Registered(_) | Cancelled(_, _, _) => Decision.reject(DeliveryNoteAlreadyExists)
  }.validate(_.mustBeRegistered)

  /** Replaces the note on business day `today` under the same rules as [[register]]; the same data again decides nothing. */
  def change(info: DeliveryNoteInfo, today: LocalDate): Decision[DeliveryNoteError, DeliveryNoteEvent, DeliveryNoteAggregate] = this.decide {
    case Registered(note) if note.info == info => Decision.unit
    case Registered(_) => rules(info, today).toDecision *> DeliveryNoteChanged(info, today).accept
    case Cancelled(_, _, _) => Decision.reject(DeliveryNoteIsCancelled)
    case Empty => Decision.reject(DeliveryNoteNotFound)
  }.validate(_.mustBeRegistered)

  /** Cancels the note on business day `today` for `reason`. */
  def cancel(reason: String :| CancellationReason, today: LocalDate): Decision[DeliveryNoteError, DeliveryNoteEvent, DeliveryNoteAggregate] =
    this.decide {
      case Registered(_) => Decision.accept(DeliveryNoteCancelled(reason, today))
      case Cancelled(_, _, _) => Decision.reject(DeliveryNoteIsCancelled)
      case Empty => Decision.reject(DeliveryNoteNotFound)
    }.validate(_.mustBeCancelled)

  /** The note, registered or cancelled. */
  def record: Option[DeliveryNoteRecord] = this match
    case Registered(note) => Some(note)
    case Cancelled(note, _, _) => Some(note)
    case Empty => None

  private def mustBeRegistered: ValidatedNec[DeliveryNoteError, Registered] = this match
    case registered: Registered => registered.validNec
    case Cancelled(_, _, _) => DeliveryNoteIsCancelled.invalidNec
    case Empty => DeliveryNoteNotFound.invalidNec

  private def mustBeCancelled: ValidatedNec[DeliveryNoteError, Cancelled] = this match
    case cancelled: Cancelled => cancelled.validNec
    case Registered(_) | Empty => DeliveryNoteNotFound.invalidNec
end DeliveryNoteAggregate

/** `DomainModel` instance for the event-sourced [[DeliveryNoteAggregate]]. */
object DeliveryNoteAggregate extends DomainModel[DeliveryNoteAggregate, DeliveryNoteEvent, DeliveryNoteError]:
  override def initial: DeliveryNoteAggregate = Empty

  /** The fold only applies the events: the decisions checked every rule beforehand. */
  override def transition: DeliveryNoteEvent => DeliveryNoteAggregate => ValidatedNec[DeliveryNoteError, DeliveryNoteAggregate] = {
    case DeliveryNoteRegistered(info, registeredOn) => _ => Registered(DeliveryNoteRecord(info, registeredOn, None)).validNec
    case DeliveryNoteChanged(info, changedOn) =>
      _.mustBeRegistered.map(registered => Registered(registered.note.copy(info = info, changedOn = Some(changedOn))))
    case DeliveryNoteCancelled(reason, cancelledOn) => _.mustBeRegistered.map(registered => Cancelled(registered.note, cancelledOn, reason))
  }

  /** The rules of a registered or changed note: its invariants, and a date not after `today`. */
  private def rules(info: DeliveryNoteInfo, today: LocalDate): ValidatedNec[DeliveryNoteError, Unit] =
    (
      info.checkInvariants,
      Validated.condNec(!info.date.isAfter(today), (), DeliveryNoteInFuture(info.date, today)),
    ).tupled.void
end DeliveryNoteAggregate
