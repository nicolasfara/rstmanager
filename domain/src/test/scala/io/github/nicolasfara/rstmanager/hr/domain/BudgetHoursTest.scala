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
