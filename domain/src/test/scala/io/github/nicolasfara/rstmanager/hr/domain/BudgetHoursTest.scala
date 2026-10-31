package io.github.nicolasfara.rstmanager.hr.domain

import com.github.nscala_time.time.Imports.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class BudgetHoursTest extends AnyFlatSpecLike:
  private val today: LocalDate = LocalDate.parse("2026-06-15")
  private val defaultHours = 40

  private def validOrFail[A](value: cats.data.ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  "BudgetHours.createBudgetHours" should "use the default weekly hours when no override is present" in:
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, Nil))
    val hours = budgetHours.getWorkingHoursForDay(today)

    hours shouldEqual DailyHours.applyUnsafe(defaultHours / 5)

  it should "return the hours according to the daily override" in:
    val overrideDate = today
    val dailyOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(4, Some("Special day"), overrideDate))
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, List(dailyOverride)))

    val hours = budgetHours.getWorkingHoursForDay(overrideDate)

    hours shouldEqual DailyHours.applyUnsafe(4)

  it should "return 0 hours on every vacation day, the last one included, and the default the day after" in:
    val vacationOverride = validOrFail(VacationOverride.createVacationOverride(today, today.plusDays(4)))
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, List(vacationOverride)))

    for dayOffset <- 0 to 4 do budgetHours.getWorkingHoursForDay(today.plusDays(dayOffset)) shouldEqual DailyHours.applyUnsafe(0)
    budgetHours.getWorkingHoursForDay(today.plusDays(5)) shouldEqual DailyHours.applyUnsafe(defaultHours / 5)

  it should "treat a single-day vacation as one day off" in:
    val vacationOverride = validOrFail(VacationOverride.createVacationOverride(today, today))
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, List(vacationOverride)))

    budgetHours.getWorkingHoursForDay(today) shouldEqual DailyHours.applyUnsafe(0)

  it should "let a working-day override win over a vacation covering the same day, whatever the list order" in:
    val vacationOverride = validOrFail(VacationOverride.createVacationOverride(today, today.plusDays(2)))
    val dayOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(3, None, today.plusDays(1)))

    for overrides <- List(List(vacationOverride, dayOverride), List(dayOverride, vacationOverride)) do
      val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, overrides))
      budgetHours.getWorkingHoursForDay(today.plusDays(1)) shouldEqual DailyHours.applyUnsafe(3)
      budgetHours.getWorkingHoursForDay(today) shouldEqual DailyHours.applyUnsafe(0)

  it should "replace a working-day override when a new one is set for the same day" in:
    val overrideDate = today
    val firstOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(2, Some("Morning only"), overrideDate))
    val secondOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(6, Some("Recovered hours"), overrideDate))
    val budgetHours = validOrFail(BudgetHours.createBudgetHours(defaultHours, List(firstOverride)))

    val updated = budgetHours.setOverride(secondOverride)

    updated.overrides should contain only secondOverride
    updated.getWorkingHoursForDay(overrideDate) shouldEqual DailyHours.applyUnsafe(6)
end BudgetHoursTest
