package io.github.nicolasfara.rstmanager.hr.domain

import java.time.LocalDate
import io.github.iltotore.iron.*

final case class BudgetHours(default: WeeklyHours, overrides: List[HoursOverride]):
  given CanEqual[LocalDate, LocalDate] = CanEqual.derived

  def getWorkingHoursForDay(day: LocalDate): DailyHours =
    overrides
      .collectFirst {
        case DayOfWeekHoursOverride(hours, _, dayOfWeek) if dayOfWeek == day => hours
      }
      .getOrElse {
        // Fallback to default weekly hours divided by 5 (assuming a 5-day work week)
        val defaultDailyHours = default.value / 5
        defaultDailyHours.refineUnsafe
      }
