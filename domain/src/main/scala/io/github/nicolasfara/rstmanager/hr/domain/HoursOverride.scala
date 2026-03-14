package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
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

/** Overrides the working hours for a specific day.
  *
  * @param hours
  *   Hours to apply on the given day.
  * @param reason
  *   Optional explanation for the override.
  * @param day
  *   Day affected by the override.
  */
final case class WorkingDayOverride(hours: DailyHours, reason: Option[String :| OverrideReason], day: DateTime) extends HoursOverride:
  /** Returns a copy with updated hours. */
  def updateHours(hours: DailyHours): WorkingDayOverride = this.focus(_.hours).replace(hours)

  /** Returns a copy with an updated reason. */
  def updateReason(reason: Option[String :| OverrideReason]): WorkingDayOverride = this.focus(_.reason).replace(reason)

  /** Returns a copy with an updated day. */
  def updateDay(day: DateTime): WorkingDayOverride = this.focus(_.day).replace(day)

object WorkingDayOverride:
  /** Creates a one-day override from raw values.
    *
    * @param hours
    *   Working hours for the day.
    * @param reason
    *   Optional explanation.
    * @param day
    *   Day affected by the override.
    */
  def createWorkingDayOverride(hours: Int, reason: Option[String], day: DateTime): ValidatedNec[String, WorkingDayOverride] =
    val validatedReason = reason match
      case Some(value) => value.refineValidatedNec[OverrideReason].map(Some(_))
      case None => Validated.validNec(Option.empty[String :| OverrideReason])

    (
      DailyHours.validatedNec(hours),
      validatedReason,
      Validated.validNec(day),
    ).mapN(WorkingDayOverride.apply)

  def apply(hours: Int, reason: Option[String], day: DateTime): ValidatedNec[String, WorkingDayOverride] =
    createWorkingDayOverride(hours, reason, day)

/** Removes working capacity for a continuous interval, typically to represent vacation.
  *
  * @param interval
  *   Closed interval affected by the override.
  */
final case class VacationOverride(interval: Interval) extends HoursOverride:
  /** Returns a copy with an updated vacation interval. */
  def updateInterval(interval: Interval): VacationOverride = this.focus(_.interval).replace(interval)

object VacationOverride:
  /** Creates a vacation override if the interval bounds are valid.
    *
    * @param startDate
    *   Start of the vacation interval.
    * @param endDate
    *   End of the vacation interval.
    */
  def createVacationOverride(startDate: DateTime, endDate: DateTime): ValidatedNec[String, VacationOverride] =
    Validated.condNec(
      !endDate.isBefore(startDate),
      VacationOverride(startDate to endDate),
      "The vacation end date must be on or after the start date",
    )
