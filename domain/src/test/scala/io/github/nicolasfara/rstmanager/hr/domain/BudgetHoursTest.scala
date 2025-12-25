package io.github.nicolasfara.rstmanager.hr.domain

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import com.github.nscala_time.time.Imports.*

class BudgetHoursTest extends AnyFlatSpecLike:
  private val defaultHours = 40
  "A BudgetHours" can "have default weekly hours" in:
    for
      default <- WeeklyHours(defaultHours)
      expectHours <- DailyHours(defaultHours / 5)
    do {
      val budgetHours = BudgetHours(default, Nil)
      val hours = budgetHours.getWorkingHoursForDay(DateTime.now())
      hours shouldEqual expectHours
    }
  it should "return the hours according to the daily override" in:
    val overrideDate = DateTime.now().withTimeAtStartOfDay()
    val overrideHours = 4
    for
      default <- WeeklyHours(defaultHours)
      hoursOverride <- DailyHours(overrideHours)
      dailyOverride = WorkingDayOverride(hoursOverride, Some("Special day"), overrideDate)
    do {
      val budgetHours = BudgetHours(default, List(dailyOverride))
      val hours = budgetHours.getWorkingHoursForDay(overrideDate)
      hours shouldEqual hoursOverride
    }
  it should "return 0 hours for any day within the vacation interval" in:
    val overrideFrom = DateTime.now().withTimeAtStartOfDay()
    val vacationInterval = overrideFrom to overrideFrom.plusDays(4)
    val overrideWeeklyHours = 0
    for
      default <- WeeklyHours(defaultHours)
      weeklyOverride <- WeeklyHours(overrideWeeklyHours)
      rangeOverride = VacationOverride(vacationInterval)
    do {
      val budgetHours = BudgetHours(default, List(rangeOverride))
      for dayOffset <- 0 until 4 do {
        val day = overrideFrom.plusDays(dayOffset)
        val hours = budgetHours.getWorkingHoursForDay(day)
        hours shouldEqual weeklyOverride
      }
    }
  it should "return the override hours when a day has overrides" in:
    val overrideDate = DateTime.now().withTimeAtStartOfDay()
    val overrideHours = 2
    val vacationFrom = overrideDate.minusDays(1)
    val vacationInterval = vacationFrom to vacationFrom.plusDays(3)
    for
      default <- WeeklyHours(defaultHours)
      hoursOverride <- DailyHours(overrideHours)
      dailyOverride = WorkingDayOverride(hoursOverride, Some("Special day"), overrideDate)
    do {
      val budgetHours = BudgetHours(default, List(dailyOverride))
      val hours = budgetHours.getWorkingHoursForDay(overrideDate)
      hours shouldEqual hoursOverride
    }