package io.github.nicolasfara.rstmanager.hr.domain.calendar.events

import io.github.nicolasfara.rstmanager.hr.domain.calendar.{ ClosureId, CompanyClosure }

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.hr.domain.calendar.CompanyCalendarAggregate]].
 *
 * Events are granular, one per closure change, rather than a snapshot of the whole calendar.
 */
enum CompanyCalendarEvent derives CanEqual:
  /** A closure was added to the calendar. */
  case ClosureAdded(closure: CompanyClosure)

  /** The closure with the same id was replaced with new data (its kind may change too). */
  case ClosureUpdated(closure: CompanyClosure)

  /** The closure with the given id was removed from the calendar. */
  case ClosureRemoved(id: ClosureId)
