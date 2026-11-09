package io.github.nicolasfara.rstmanager.hr.domain.calendar

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs

/** Refined constraint for the mandatory, non-empty reason of a closure (shown in the user interface and on the planning board). */
type ClosureReason = DescribedAs[Not[Empty], "The closure reason cannot be empty"]

/** Refined constraint for a month of the year, `1` (January) to `12` (December). */
type Month = DescribedAs[GreaterEqual[1] & LessEqual[12], "The closure month must be between 1 and 12"]

/** Refined constraint for a day of the month, `1` to `31`; whether the day exists in its month is checked by [[CompanyClosure.createAnnual]]. */
type DayOfMonth = DescribedAs[GreaterEqual[1] & LessEqual[31], "The closure day must be between 1 and 31"]

/**
 * A span of calendar days on which the whole company is closed, so no employee is available whatever their own budget says.
 *
 *   - [[CompanyClosure.Period]] closes a one-off range of days, both bounds included.
 *   - [[CompanyClosure.Annual]] closes the same day and month every year (e.g. 25/12 or 15/08); 29/02 only closes leap years.
 */
enum CompanyClosure derives CanEqual:
  /** Closes every day from `firstDay` to `lastDay`, both included. */
  case Period(id: ClosureId, firstDay: LocalDate, lastDay: LocalDate, reason: String :| ClosureReason)

  /** Closes the day `dayOfMonth` of `month` in every year that has it. */
  case Annual(id: ClosureId, month: Int :| Month, dayOfMonth: Int :| DayOfMonth, reason: String :| ClosureReason)

  /** Stable identifier of the closure. */
  def id: ClosureId

  /** Why the company is closed. */
  def reason: String :| ClosureReason

  /** Whether the company is closed on `day` because of this closure. */
  def covers(day: LocalDate): Boolean = this match
    case Period(_, firstDay, lastDay, _) => !day.isBefore(firstDay) && !day.isAfter(lastDay)
    case Annual(_, month, dayOfMonth, _) => day.getMonthOfYear == month && day.getDayOfMonth == dayOfMonth

  /** The last day of the closed span this closure has around `day`, when it covers `day`: the planner jumps straight past it. */
  def lastDayCovering(day: LocalDate): Option[LocalDate] = this match
    case Period(_, _, lastDay, _) => Option.when(covers(day))(lastDay)
    case Annual(_, _, _, _) => Option.when(covers(day))(day)

  /** Whether this closure and `other` close a common day for sure: two periods sharing a day, or two annual closures on the same day and month. */
  def clashesWith(other: CompanyClosure): Boolean = (this, other) match
    case (Period(_, firstDay, lastDay, _), Period(_, otherFirst, otherLast, _)) => !otherLast.isBefore(firstDay) && !lastDay.isBefore(otherFirst)
    case (Annual(_, month, dayOfMonth, _), Annual(_, otherMonth, otherDay, _)) => month == otherMonth && dayOfMonth == otherDay
    case (Period(_, _, _, _), Annual(_, _, _, _)) | (Annual(_, _, _, _), Period(_, _, _, _)) => false
end CompanyClosure

object CompanyClosure:
  /**
   * Creates a one-off closure if the range is valid (a single day is allowed) and the reason is not empty.
   *
   * @param id
   *   Closure identifier.
   * @param firstDay
   *   First closed day.
   * @param lastDay
   *   Last closed day.
   * @param reason
   *   Why the company is closed.
   */
  def createPeriod(id: ClosureId, firstDay: LocalDate, lastDay: LocalDate, reason: String): ValidatedNec[String, CompanyClosure] =
    (
      Validated.condNec(!lastDay.isBefore(firstDay), (), "The closure end date must be on or after the start date"),
      reason.refineValidatedNec[ClosureReason],
    ).mapN((_, validReason) => Period(id, firstDay, lastDay, validReason))

  /**
   * Creates a yearly closure if the day exists in the month (29/02 is accepted and only closes leap years) and the reason is not empty.
   *
   * @param id
   *   Closure identifier.
   * @param month
   *   Month of the year, `1` to `12`.
   * @param dayOfMonth
   *   Day of the month, `1` to the length of the month.
   * @param reason
   *   Why the company is closed.
   */
  def createAnnual(id: ClosureId, month: Int, dayOfMonth: Int, reason: String): ValidatedNec[String, CompanyClosure] =
    (
      month.refineValidatedNec[Month],
      dayOfMonth.refineValidatedNec[DayOfMonth],
      reason.refineValidatedNec[ClosureReason],
    ).mapN((_, _, _)).andThen { case (validMonth, validDay, validReason) =>
      Validated.condNec(
        validDay <= maxDayOfMonth(validMonth),
        Annual(id, validMonth, validDay, validReason),
        s"Day $dayOfMonth does not exist in month $month",
      )
    }

  /** Longest length of a month over all years: February counts its leap-year 29 days. */
  private def maxDayOfMonth(month: Int): Int = month match
    case 2 => 29
    case 4 | 6 | 9 | 11 => 30
    case _ => 31
end CompanyClosure
