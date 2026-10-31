package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

/** Daily working hours constrained to the inclusive range `0` to `24`. */
type DailyHours = DailyHours.T

/** Refined constraint for a non-empty override reason. */
type OverrideReason = DescribedAs[Not[Empty], "The override reason, if provided, cannot be empty"]

/** Refined type companion for `DailyHours`. */
object DailyHours extends RefinedType[Int, GreaterEqual[0] & LessEqual[24]]

/** Marker for temporary changes applied to the default employee budget. */
sealed trait HoursOverride

/**
 * Overrides the working hours for a specific calendar day.
 *
 * @param hours
 *   Hours to apply on the given day.
 * @param reason
 *   Optional explanation for the override.
 * @param day
 *   Day affected by the override.
 */
final case class WorkingDayOverride(hours: DailyHours, reason: Option[String :| OverrideReason], day: LocalDate) extends HoursOverride:
  /** Returns a copy with updated hours. */
  def updateHours(hours: DailyHours): WorkingDayOverride = this.focus(_.hours).replace(hours)

  /** Returns a copy with an updated reason. */
  def updateReason(reason: Option[String :| OverrideReason]): WorkingDayOverride = this.focus(_.reason).replace(reason)

  /** Returns a copy with an updated day. */
  def updateDay(day: LocalDate): WorkingDayOverride = this.focus(_.day).replace(day)

object WorkingDayOverride:
  /**
   * Creates a one-day override from raw values.
   *
   * @param hours
   *   Working hours for the day.
   * @param reason
   *   Optional explanation.
   * @param day
   *   Day affected by the override.
   */
  def createWorkingDayOverride(hours: Int, reason: Option[String], day: LocalDate): ValidatedNec[String, WorkingDayOverride] =
    val validatedReason = reason match
      case Some(value) => value.refineValidatedNec[OverrideReason].map(Some(_))
      case None => Validated.validNec(Option.empty[String :| OverrideReason])

    (
      DailyHours.validatedNec(hours),
      validatedReason,
      Validated.validNec(day),
    ).mapN(WorkingDayOverride.apply)
end WorkingDayOverride

/**
 * Removes working capacity for a continuous range of calendar days, typically to represent vacation.
 *
 * Both bounds are inclusive: `lastDay` is the last day off, matching the "Da … A …" wording of the user interface.
 *
 * @param firstDay
 *   First day off.
 * @param lastDay
 *   Last day off.
 */
final case class VacationOverride(firstDay: LocalDate, lastDay: LocalDate) extends HoursOverride:
  /** Whether `day` is one of the vacation days. */
  def covers(day: LocalDate): Boolean = !day.isBefore(firstDay) && !day.isAfter(lastDay)

  /** Whether this vacation shares at least one day with `other`. */
  def overlaps(other: VacationOverride): Boolean = !other.lastDay.isBefore(firstDay) && !lastDay.isBefore(other.firstDay)

object VacationOverride:
  /**
   * Creates a vacation override if the range is valid (a single day is allowed).
   *
   * @param firstDay
   *   First day off.
   * @param lastDay
   *   Last day off.
   */
  def createVacationOverride(firstDay: LocalDate, lastDay: LocalDate): ValidatedNec[String, VacationOverride] =
    Validated.condNec(
      !lastDay.isBefore(firstDay),
      VacationOverride(firstDay, lastDay),
      "The vacation end date must be on or after the start date",
    )
