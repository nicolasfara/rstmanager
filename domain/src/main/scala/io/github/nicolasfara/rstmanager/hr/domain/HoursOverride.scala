package io.github.nicolasfara.rstmanager.hr.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import java.time.LocalDate

type DailyHours = Int :| (GreaterEqual[0] & LessEqual[24])

sealed trait HoursOverride

final case class DayOfWeekHoursOverride(hours: DailyHours, reason: Option[String], day: LocalDate) extends HoursOverride

final case class RangeHoursOverride(
    hours: WeeklyHours,
    reason: Option[String],
    from: java.time.LocalDate,
    to: java.time.LocalDate
) extends HoursOverride
