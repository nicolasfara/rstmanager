package io.github.nicolasfara.rstmanager.hr.domain

import com.github.nscala_time.time.Imports.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class BudgetHoursTest extends AnyFlatSpecLike:
  private val defaultHours = 40

  private def validOrFail[A](value: cats.data.ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  "BudgetHours.createBudgetHours" should "use the default weekly hours when no override is present" in:
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, Nil))
    val hours = budgetHours.getWorkingHoursForDay(DateTime.now())

    hours shouldEqual DailyHours.applyUnsafe(defaultHours / 5)

  it should "return the hours according to the daily override" in:
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val dailyOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(4, Some("Special day"), overrideDate))
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, List(dailyOverride)))

    val hours = budgetHours.getWorkingHoursForDay(overrideDate)

    hours shouldEqual DailyHours.applyUnsafe(4)

  it should "return 0 hours for any day within the vacation interval" in:
    val overrideFrom = DateTime.now().withTimeAtStartOfDay().nn
    val vacationOverride = validOrFail(VacationOverride.createVacationOverride(overrideFrom, overrideFrom.plusDays(4).nn))
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, List(vacationOverride)))

    for dayOffset <- 0 until 4 do
      val day = overrideFrom.plusDays(dayOffset).nn
      budgetHours.getWorkingHoursForDay(day) shouldEqual DailyHours.applyUnsafe(0)

  it should "replace a working-day override when a new one is set for the same day" in:
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val firstOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(2, Some("Morning only"), overrideDate))
    val secondOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(6, Some("Recovered hours"), overrideDate))
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, List(firstOverride)))

    val updated = budgetHours.setOverride(secondOverride)

    updated.overrides should contain only secondOverride
    updated.getWorkingHoursForDay(overrideDate) shouldEqual DailyHours.applyUnsafe(6)
end BudgetHoursTest
