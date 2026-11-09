package io.github.nicolasfara.rstmanager.hr.domain.calendar

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate

/**
 * The company calendar (the state held by [[CompanyCalendarAggregate]]): the closures during which nobody works.
 *
 * It is the single definition of a working day: Monday to Friday, unless a closure covers the day. A closure wins over everything else, including a
 * working-day override on an employee, exactly like a weekend does.
 *
 * @param closures
 *   Closures in the order they were added.
 */
final case class CompanyCalendar(closures: List[CompanyClosure]) derives CanEqual:
  /** A closure covering `day`, if any. */
  def closureOn(day: LocalDate): Option[CompanyClosure] = closures.find(_.covers(day))

  /** Whether `day` is a working day: Monday to Friday and not covered by any closure. */
  def isWorkingDay(day: LocalDate): Boolean = day.getDayOfWeek <= 5 && closureOn(day).isEmpty

  /** The last day of the closed span around `day` (the latest one when closures abut), when a closure covers `day`. */
  def closedUntil(day: LocalDate): Option[LocalDate] =
    closures.flatMap(_.lastDayCovering(day)).reduceOption((first, second) => if first.isAfter(second) then first else second)

  /** The closure with the given id, if any. */
  def find(id: ClosureId): Option[CompanyClosure] = closures.find(_.id == id)

  /** Whether a closure with the given id is in the calendar. */
  def contains(id: ClosureId): Boolean = find(id).isDefined

  /** Replaces the closure with the same id in place, or appends it when there is none. */
  def upsert(closure: CompanyClosure): CompanyCalendar =
    if contains(closure.id) then CompanyCalendar(closures.map(existing => if existing.id == closure.id then closure else existing))
    else CompanyCalendar(closures :+ closure)

  /** Drops the closure with the given id, if any. */
  def without(id: ClosureId): CompanyCalendar = CompanyCalendar(closures.filterNot(_.id == id))

  /**
   * Rules every calendar must satisfy; the aggregate enforces them on every change. It rejects two closures with the same id, two periods sharing a
   * day, and two annual closures on the same day and month. A period containing an annual day is allowed: the day is closed either way.
   */
  def checkInvariants: ValidatedNec[CompanyCalendarError, CompanyCalendar] =
    val pairs = closures.tails.flatMap {
      case head :: rest => rest.map(head -> _)
      case Nil => Nil
    }.toList
    (
      Validated.condNec(closures.map(_.id).distinct.size == closures.size, (), CompanyCalendarError.ClosureAlreadyExists),
      Validated.condNec(!pairs.exists((first, second) => first.clashesWith(second)), (), CompanyCalendarError.OverlappingClosures),
    ).mapN((_, _) => this)
end CompanyCalendar

object CompanyCalendar:
  /** The calendar with no closures: only weekends are days off. */
  val empty: CompanyCalendar = CompanyCalendar(Nil)

  /**
   * Puts the [[CompanyCalendarAggregate]] model in the implicit scope of its state, as an aggregate enum's own companion does: the calendar is a
   * plain value object, so its model lives in a separate object.
   */
  export CompanyCalendarAggregate.given
