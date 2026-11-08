package io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events

import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.{ CancellationReason, DeliveryNoteInfo }

import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.:|

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.DeliveryNoteAggregate]], each with the business day
 * it happened on (from the command time).
 *
 * The journal is an append-only schema: new cases need no version bump, a changed payload does.
 */
enum DeliveryNoteEvent derives CanEqual:
  /** The delivery note was registered on `registeredOn`. */
  case DeliveryNoteRegistered(info: DeliveryNoteInfo, registeredOn: LocalDate)

  /** The delivery note was replaced by `info` on `changedOn`. */
  case DeliveryNoteChanged(info: DeliveryNoteInfo, changedOn: LocalDate)

  /** The delivery note was cancelled on `cancelledOn` for `reason`: it no longer brings anything, its record is kept for the history. */
  case DeliveryNoteCancelled(reason: String :| CancellationReason, cancelledOn: LocalDate)
