package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class EmployeeTest extends AnyFlatSpecLike:
  /** Fixed reference day: the domain never reads the clock, so the tests must not either. */
  private val today: LocalDate = LocalDate.parse("2026-06-15")

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def createEmployee(
      name: String,
      surname: String,
      contract: Contract,
      weeklyHours: Int,
      overrides: List[HoursOverride],
  ): ValidatedNec[String, Employee] =
    Employee.createEmployee(EmployeeId(UUID.randomUUID().nn), name, surname, contract, weeklyHours, overrides)

  "Contract.createFixedTerm" should "reject an end date before the start date" in:
    Contract.createFixedTerm(today, today.minusDays(1)).isValid shouldBe false

  it should "accept a one-day contract, since the end date is the last day of employment" in:
    Contract.createFixedTerm(today, today).isValid shouldBe true

  "Employee.createEmployee" should "accumulate nested validation errors" in:
    val contract = Contract.createFullTime(today - 1.month)
    val result = Employee.createEmployee(EmployeeId(UUID.randomUUID().nn), "", "Doe", contract, -1, Nil)

    result.isValid shouldBe false
    result.swap.foreach(_.length shouldEqual 2L)

  "An Employee" should "be inactive when the contract is terminated" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        today - 2.months,
        today - 1.day,
      ),
    )
    val employee = validOrFail(createEmployee("John", "Doe", contract, 40, Nil))

    employee.isActiveAt(today) shouldBe false

  it should "be active when the contract is ongoing" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        today - 1.month,
        today + 1.month,
      ),
    )
    val employee = validOrFail(createEmployee("Jane", "Smith", contract, 40, Nil))

    employee.isActiveAt(today) shouldBe true

  it should "be inactive before the contract start date" in:
    val contract = Contract.createFullTime(today.plusDays(1).nn)
    val employee = validOrFail(createEmployee("Future", "Starter", contract, 40, Nil))

    employee.isActiveAt(today) shouldBe false

  it should "be active through the last day of a fixed-term contract and inactive the day after" in:
    val start = today
    val end = start.plusDays(2).nn
    val contract = validOrFail(Contract.createFixedTerm(start, end))
    val employee = validOrFail(createEmployee("Fixed", "Term", contract, 40, Nil))

    employee.isActiveAt(start) shouldBe true
    employee.isActiveAt(start.plusDays(1)) shouldBe true
    employee.isActiveAt(end) shouldBe true
    employee.isActiveAt(end.plusDays(1)) shouldBe false

  it should "allow updating budget hours when active" in:
    val contract = Contract.createFullTime(today - 1.month)
    val employee = validOrFail(createEmployee("Alice", "Johnson", contract, 40, Nil))
    val newBudgetHours = validOrFail(BudgetHours.createBudgetHours(35, Nil))

    val updatedEmployee = employee.updateBudgetHours(newBudgetHours, today).getOrElse(fail("Failed to update budget hours"))

    updatedEmployee.budgetHours.default shouldEqual WeeklyHours.applyUnsafe(35)

  it should "prevent updating budget hours when inactive" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        today - 2.months,
        today - 1.day,
      ),
    )
    val employee = validOrFail(createEmployee("Bob", "Brown", contract, 40, Nil))
    val newBudgetHours = validOrFail(BudgetHours.createBudgetHours(30, Nil))

    val result = employee.updateBudgetHours(newBudgetHours, today)

    result.isLeft shouldBe true
    result.left.getOrElse("") shouldBe "Cannot update budget hours for an inactive employee."

  it should "allow setting hours override when active and no conflict" in:
    val contract = validOrFail(Contract.createPartTime(today - 1.month, 20))
    val employee = validOrFail(createEmployee("Charlie", "Davis", contract, 20, Nil))
    val overrideDate = today
    val dailyOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(4, Some("Reduced hours"), overrideDate))

    val result = employee.setHoursOverride(dailyOverride, today)

    result.isRight shouldBe true
    result.getOrElse(fail("Failed to set hours override")).budgetHours.overrides should contain(dailyOverride)

  it should "prevent setting hours override when inactive" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        today - 2.months,
        today - 1.day,
      ),
    )
    val employee = validOrFail(createEmployee("Diana", "Evans", contract, 40, Nil))
    val overrideDate = today
    val dailyOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(4, Some("Reduced hours"), overrideDate))

    val result = employee.setHoursOverride(dailyOverride, today)

    result.isLeft shouldBe true
    result.left.getOrElse("") shouldBe "Cannot set hours override for an inactive employee."

  it should "prevent setting conflicting hours overrides when a vacation is set" in:
    val contract = Contract.createFullTime(today - 1.month)
    val employee = validOrFail(createEmployee("Eve", "Foster", contract, 40, Nil))
    val overrideDate = today
    val vacationOverride = validOrFail(VacationOverride.createVacationOverride(overrideDate, overrideDate + 2.days))
    val updatedEmployee = employee.setHoursOverride(vacationOverride, today).getOrElse(fail("Failed to set first hours override"))
    val conflictingOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(6, Some("Another override"), overrideDate))

    val secondResult = updatedEmployee.setHoursOverride(conflictingOverride, today)

    secondResult.isLeft shouldBe true
    secondResult.left.getOrElse("") shouldBe "An override for the specified day or interval already exists."

  it should "prevent setting conflicting vacation overrides" in:
    val contract = Contract.createFullTime(today - 1.month)
    val employee = validOrFail(createEmployee("Frank", "Green", contract, 40, Nil))
    val overrideDate = today
    val firstVacationOverride = validOrFail(VacationOverride.createVacationOverride(overrideDate, overrideDate + 3.days))
    val updatedEmployee = employee.setHoursOverride(firstVacationOverride, today).getOrElse(fail("Failed to set first vacation override"))
    val conflictingVacationOverride = validOrFail(VacationOverride.createVacationOverride(overrideDate + 2.days, overrideDate + 5.days))

    val secondResult = updatedEmployee.setHoursOverride(conflictingVacationOverride, today)

    secondResult.isLeft shouldBe true
    secondResult.left.getOrElse("") shouldBe "An override for the specified day or interval already exists."

  it should "update the nested name through optics" in:
    val contract = Contract.createFullTime(today - 1.month)
    val employee = validOrFail(createEmployee("Grace", "Hill", contract, 40, Nil))

    val updated = employee.updateName("Georgia".refineUnsafe[Name])

    updated.info.name.toString shouldEqual "Georgia"
    updated.info.surname shouldEqual employee.info.surname
    updated.contract shouldEqual employee.contract
    updated.budgetHours shouldEqual employee.budgetHours
end EmployeeTest
