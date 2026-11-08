package io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote

import com.github.nscala_time.time.Imports.LocalDate

/** Errors raised while deciding or applying delivery note operations. */
enum DeliveryNoteError derives CanEqual:
  /** A registration was attempted on an id that already exists (registered or cancelled). */
  case DeliveryNoteAlreadyExists

  /** An operation was attempted on an id that does not exist. */
  case DeliveryNoteNotFound

  /** A change was attempted on a cancelled delivery note, which only keeps its history. */
  case DeliveryNoteIsCancelled

  /** The delivery note carried in a command does not have the id of the stream it was sent to. */
  case DeliveryNoteIdMismatch

  /** The delivery note has no line. */
  case NoLines

  /** A line of the delivery note has a zero quantity. */
  case NonPositiveQuantity

  /** The delivery note is dated after `today`, the business day of the command. */
  case DeliveryNoteInFuture(date: LocalDate, today: LocalDate)
end DeliveryNoteError
