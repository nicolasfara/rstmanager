package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.cats.*

import com.github.nscala_time.time.Imports.*

opaque type DailyHours = Int :| (GreaterEqual[0] & LessEqual[24])

object DailyHours:
  def apply(value: Int): Validated[String, DailyHours] = value.refineValidated

  extension (dh: DailyHours) def value: Int = dh

sealed trait HoursOverride

final case class DayOfWeekHoursOverride(hours: DailyHours, reason: Option[String], day: DateTime) extends HoursOverride

final case class VacationOverride(interval: Interval) extends HoursOverride
