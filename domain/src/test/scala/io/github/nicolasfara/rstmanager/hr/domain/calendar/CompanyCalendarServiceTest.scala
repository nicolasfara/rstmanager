package io.github.nicolasfara.rstmanager.hr.domain.calendar

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.calendar.CompanyCalendarService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.hr.domain.calendar.events.CompanyCalendarEvent

import cats.Id
import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class CompanyCalendarServiceTest extends AnyFlatSpecLike:
  private type Result = EdomatonResult[CompanyCalendar, CompanyCalendarEvent, CompanyCalendarError, Notification]

  private val firstId: ClosureId = ClosureId(UUID.fromString("00000000-0000-0000-0000-0000000000d1").nn)
  private val secondId: ClosureId = ClosureId(UUID.fromString("00000000-0000-0000-0000-0000000000d2").nn)
  private val day: LocalDate = LocalDate.parse("2026-08-10")

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def period(id: ClosureId, first: LocalDate, last: LocalDate): CompanyClosure =
    validOrFail(CompanyClosure.createPeriod(id, first, last, "Chiusura estiva"))

  private def run(command: Command, state: CompanyCalendar): Result =
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, "company-calendar", command)
    CompanyCalendarService[Id].execute(RequestContext(message, state))

  private def accepted(result: Result): (CompanyCalendar, List[CompanyCalendarEvent], List[Notification]) =
    result match
      case EdomatonResult.Accepted(state, events, notifications) => (state, events.toChain.toList, notifications.toList)
      case other => fail(s"Expected an acceptance, got: $other")

  private def rejection(result: Result): List[CompanyCalendarError] =
    result match
      case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
      case other => fail(s"Expected a rejection, got: $other")

  "CompanyCalendarService" should "add a closure and publish that the calendar changed" in:
    val closure = period(firstId, day, day.plusDays(4))
    val (state, events, notifications) = accepted(run(Command.Add(closure), CompanyCalendar.empty))

    state shouldEqual CompanyCalendar(List(closure))
    events shouldEqual List(CompanyCalendarEvent.ClosureAdded(closure))
    notifications shouldEqual List(Notification.CompanyCalendarChanged)

  it should "reject an addition reusing an existing id or overlapping another period" in:
    val existing = CompanyCalendar(List(period(firstId, day, day.plusDays(4))))

    rejection(run(Command.Add(period(firstId, day.plusDays(10), day.plusDays(11))), existing)) shouldEqual
      List(CompanyCalendarError.ClosureAlreadyExists)
    rejection(run(Command.Add(period(secondId, day.plusDays(4), day.plusDays(6))), existing)) shouldEqual
      List(CompanyCalendarError.OverlappingClosures)

  it should "update an existing closure, checking the invariants against the other closures" in:
    val existing = CompanyCalendar(List(period(firstId, day, day.plusDays(4)), period(secondId, day.plusDays(7), day.plusDays(8))))
    val moved = period(firstId, day.plusDays(1), day.plusDays(5))
    val (state, events, notifications) = accepted(run(Command.Update(moved), existing))

    state.find(firstId) shouldEqual Some(moved)
    events shouldEqual List(CompanyCalendarEvent.ClosureUpdated(moved))
    notifications shouldEqual List(Notification.CompanyCalendarChanged)
    rejection(run(Command.Update(period(firstId, day, day.plusDays(7))), existing)) shouldEqual List(CompanyCalendarError.OverlappingClosures)

  it should "reject the update or removal of an unknown closure" in:
    rejection(run(Command.Update(period(firstId, day, day)), CompanyCalendar.empty)) shouldEqual List(CompanyCalendarError.ClosureNotFound)
    rejection(run(Command.Remove(firstId), CompanyCalendar.empty)) shouldEqual List(CompanyCalendarError.ClosureNotFound)

  it should "remove a closure and publish that the calendar changed" in:
    val (state, events, notifications) = accepted(run(Command.Remove(firstId), CompanyCalendar(List(period(firstId, day, day)))))

    state shouldEqual CompanyCalendar.empty
    events shouldEqual List(CompanyCalendarEvent.ClosureRemoved(firstId))
    notifications shouldEqual List(Notification.CompanyCalendarChanged)

  "The company calendar fold" should "accept every persisted event, whatever the current state" in:
    val closure = period(firstId, day, day)
    val fold = CompanyCalendarAggregate.transition

    fold(CompanyCalendarEvent.ClosureUpdated(closure))(CompanyCalendar.empty).toOption shouldEqual Some(CompanyCalendar(List(closure)))
    fold(CompanyCalendarEvent.ClosureRemoved(secondId))(CompanyCalendar(List(closure))).toOption shouldEqual Some(CompanyCalendar(List(closure)))
    fold(CompanyCalendarEvent.ClosureAdded(closure))(CompanyCalendar(List(closure))).toOption shouldEqual Some(CompanyCalendar(List(closure)))
end CompanyCalendarServiceTest
