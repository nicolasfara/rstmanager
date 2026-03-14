package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

type DailyHours = DailyHours.T
type OverrideReason = DescribedAs[Not[Empty], "The override reason, if provided, cannot be empty"]

object DailyHours extends RefinedType[Int, GreaterEqual[0] & LessEqual[24]]

sealed trait HoursOverride

final case class WorkingDayOverride(hours: DailyHours, reason: Option[String :| OverrideReason], day: DateTime) extends HoursOverride:
  def updateHours(hours: DailyHours): WorkingDayOverride = this.focus(_.hours).replace(hours)

  def updateReason(reason: Option[String :| OverrideReason]): WorkingDayOverride = this.focus(_.reason).replace(reason)

  def updateDay(day: DateTime): WorkingDayOverride = this.focus(_.day).replace(day)

object WorkingDayOverride:
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

final case class VacationOverride(interval: Interval) extends HoursOverride:
  def updateInterval(interval: Interval): VacationOverride = this.focus(_.interval).replace(interval)

object VacationOverride:
  def createVacationOverride(startDate: DateTime, endDate: DateTime): ValidatedNec[String, VacationOverride] =
    Validated.condNec(
      !endDate.isBefore(startDate),
      VacationOverride(startDate to endDate),
      "The vacation end date must be on or after the start date",
    )
