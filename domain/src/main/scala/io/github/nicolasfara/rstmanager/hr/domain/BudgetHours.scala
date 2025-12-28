package io.github.nicolasfara.rstmanager.hr.domain

import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*

final case class BudgetHours(default: WeeklyHours, overrides: List[HoursOverride]):
  given CanEqual[DateTime, DateTime] = CanEqual.derived

  def getWorkingHoursForDay(day: DateTime): DailyHours =
    overrides
      .collectFirst {
        case WorkingDayOverride(hours, _, dayOfWeek) if dayOfWeek == day => hours
        case VacationOverride(interval) if interval.contains(day)        => DailyHours(0): DailyHours
      }
      .getOrElse {
        // Fallback to default weekly hours divided by 5 (assuming a 5-day work week)
        DailyHours(default.value / 5).valueOr(_ => DailyHours(0): DailyHours)
      }
