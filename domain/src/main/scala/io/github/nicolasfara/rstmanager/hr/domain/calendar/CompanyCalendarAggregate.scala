package io.github.nicolasfara.rstmanager.hr.domain.calendar

import io.github.nicolasfara.rstmanager.hr.domain.calendar.CompanyCalendarError.*
import io.github.nicolasfara.rstmanager.hr.domain.calendar.events.CompanyCalendarEvent
import io.github.nicolasfara.rstmanager.hr.domain.calendar.events.CompanyCalendarEvent.*

import cats.data.ValidatedNec
import cats.syntax.all.*
import edomata.core.*
import edomata.syntax.all.*

/**
 * Event-sourced singleton aggregate holding the [[CompanyCalendar]]: there is one calendar for the whole company, on one stream.
 *
 * Decisions enforce [[CompanyCalendar.checkInvariants]] on the calendar they would produce. The fold accepts every persisted event as it is: an
 * update of an absent closure adds it, and the removal of an absent closure changes nothing.
 */
object CompanyCalendarAggregate extends DomainModel[CompanyCalendar, CompanyCalendarEvent, CompanyCalendarError]:
  override def initial: CompanyCalendar = CompanyCalendar.empty

  override def transition: CompanyCalendarEvent => CompanyCalendar => ValidatedNec[CompanyCalendarError, CompanyCalendar] = {
    case ClosureAdded(closure) => _.upsert(closure).validNec
    case ClosureUpdated(closure) => _.upsert(closure).validNec
    case ClosureRemoved(id) => _.without(id).validNec
  }

  extension (calendar: CompanyCalendar)
    /** Adds a closure with a new id, provided the resulting calendar satisfies [[CompanyCalendar.checkInvariants]]. */
    def add(closure: CompanyClosure): Decision[CompanyCalendarError, CompanyCalendarEvent, CompanyCalendar] = calendar.decide { current =>
      if current.contains(closure.id) then Decision.reject(ClosureAlreadyExists)
      else current.upsert(closure).checkInvariants.toDecision *> ClosureAdded(closure).accept
    }.validate(_.mustContain(closure.id))

    /** Replaces an existing closure, provided the resulting calendar satisfies [[CompanyCalendar.checkInvariants]]. */
    def update(closure: CompanyClosure): Decision[CompanyCalendarError, CompanyCalendarEvent, CompanyCalendar] = calendar.decide { current =>
      if current.contains(closure.id) then current.upsert(closure).checkInvariants.toDecision *> ClosureUpdated(closure).accept
      else Decision.reject(ClosureNotFound)
    }.validate(_.mustContain(closure.id))

    /** Removes an existing closure. */
    def remove(id: ClosureId): Decision[CompanyCalendarError, CompanyCalendarEvent, CompanyCalendar] = calendar.decide { current =>
      if current.contains(id) then ClosureRemoved(id).accept else Decision.reject(ClosureNotFound)
    }.validate(_.mustNotContain(id))

    private def mustContain(id: ClosureId): ValidatedNec[CompanyCalendarError, CompanyCalendar] =
      if calendar.contains(id) then calendar.validNec else ClosureNotFound.invalidNec

    private def mustNotContain(id: ClosureId): ValidatedNec[CompanyCalendarError, CompanyCalendar] =
      if calendar.contains(id) then ClosureAlreadyExists.invalidNec else calendar.validNec
  end extension
end CompanyCalendarAggregate
