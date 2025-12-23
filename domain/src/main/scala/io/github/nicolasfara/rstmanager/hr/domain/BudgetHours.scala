package io.github.nicolasfara.rstmanager.hr.domain

import com.github.nscala_time.time.Imports.*

final case class BudgetHours(default: WeeklyHours, overrides: List[HoursOverride]):
  given CanEqual[DateTime, DateTime] = CanEqual.derived

  def getWorkingHoursForDay(day: DateTime): DailyHours =
    overrides
      .collectFirst {
        case DayOfWeekHoursOverride(hours, _, dayOfWeek) if dayOfWeek == day => hours
      }
      .getOrElse {
        // Fallback to default weekly hours divided by 5 (assuming a 5-day work week)
        val defaultDailyHours = default.value / 5
        DailyHours(defaultDailyHours).getOrElse(
          throw new IllegalStateException("Default weekly hours result in invalid daily hours")
        )
      }
