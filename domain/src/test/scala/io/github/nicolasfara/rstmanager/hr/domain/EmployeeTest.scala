package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.Validated
import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

import com.github.nscala_time.time.Imports.*

import java.util.UUID

class EmployeeTest extends AnyFlatSpecLike:
  private def createEmployee(
      name: String,
      surname: String,
      contract: Contract,
      weeklyHours: Int,
      overrides: List[HoursOverride]
  ): Validated[String, Employee] =
    (Name(name), Surname(surname), WeeklyHours(weeklyHours)).mapN {
      case (employeeName, employeeSurname, weeklyHoursValue) =>
        val employeeId = EmployeeId(UUID.randomUUID().nn)
        val employeeInfo = EmployeeInfo(employeeName, employeeSurname)
        val budgetHours = BudgetHours(weeklyHoursValue, overrides)
        Employee(employeeId, employeeInfo, contract, budgetHours)
    }

  "An Employee" should "be inactive when the contract is terminated" in:
    val contract = Contract.FixedTerm(
      DateTime.now().nn - 2.months,
      DateTime.now().nn - 1.day
    )
    val employee = createEmployee("John", "Doe", contract, 40, Nil).getOrElse(fail("Failed to create employee"))
    employee.isActive shouldBe false
  it should "be active when the contract is ongoing" in:
    val contract = Contract.FixedTerm(
      DateTime.now().nn - 1.month,
      DateTime.now().nn + 1.month
    )
    val employee = createEmployee("Jane", "Smith", contract, 40, Nil).getOrElse(fail("Failed to create employee"))
    employee.isActive shouldBe true
  it should "allow updating budget hours when active" in:
    val startingWeeklyHours = 40
    val contract = Contract.FullTime(DateTime.now().nn - 1.month)
    val employee =
      createEmployee("Alice", "Johnson", contract, startingWeeklyHours, Nil).getOrElse(
        fail("Failed to create employee")
      )
    employee.budgetHours.default shouldBe WeeklyHours(startingWeeklyHours).getOrElse(
      fail("Failed to create WeeklyHours")
    )
    val newWeeklyHours = WeeklyHours(35).getOrElse(fail("Failed to create WeeklyHours"))
    val updatedEmployee = employee
      .updateBudgetHours(BudgetHours(newWeeklyHours, Nil))
      .getOrElse(
        fail("Failed to update budget hours")
      )
    updatedEmployee.budgetHours.default shouldBe newWeeklyHours
  it should "prevent updating budget hours when inactive" in:
    val contract = Contract.FixedTerm(
      DateTime.now().nn - 2.months,
      DateTime.now().nn - 1.day
    )
    val employee = createEmployee("Bob", "Brown", contract, 40, Nil).getOrElse(fail("Failed to create employee"))
    val newWeeklyHours = WeeklyHours(30).getOrElse(fail("Failed to create WeeklyHours"))
    val result = employee.updateBudgetHours(BudgetHours(newWeeklyHours, Nil))
    result.isLeft shouldBe true
    result.left.getOrElse("") shouldBe "Cannot update budget hours for an inactive employee."
  it should "allow setting hours override when active and no conflict" in:
    val contract =
      Contract.PartTime(DateTime.now().nn - 1.month, WeeklyHours(20).getOrElse(fail("Failed to create WeeklyHours")))
    val employee = createEmployee("Charlie", "Davis", contract, 20, Nil).getOrElse(fail("Failed to create employee"))
    val overrideDate = DateTime.now().withTimeAtStartOfDay()
    val dailyOverride = WorkingDayOverride(
      DailyHours(4).getOrElse(fail("Failed to create DailyHours")),
      Some("Reduced hours"),
      overrideDate
    )
    val result = employee.setHoursOverride(dailyOverride)
    result.isRight shouldBe true
    val updatedEmployee = result.getOrElse(fail("Failed to set hours override"))
    updatedEmployee.budgetHours.overrides should contain(dailyOverride)
  it should "prevent setting hours override when inactive" in:
    val contract = Contract.FixedTerm(
      DateTime.now().nn - 2.months,
      DateTime.now().nn - 1.day
    )
    val employee = createEmployee("Diana", "Evans", contract, 40, Nil).getOrElse(fail("Failed to create employee"))
    val overrideDate = DateTime.now().withTimeAtStartOfDay()
    val dailyOverride = WorkingDayOverride(
      DailyHours(4).getOrElse(fail("Failed to create DailyHours")),
      Some("Reduced hours"),
      overrideDate
    )
    val result = employee.setHoursOverride(dailyOverride)
    result.isLeft shouldBe true
    result.left.getOrElse("") shouldBe "Cannot set hours override for an inactive employee."
  it should "prevent setting conflicting hours overrides when a vacation is set" in:
    val contract =
      Contract.FullTime(DateTime.now().nn - 1.month)
    val employee = createEmployee("Eve", "Foster", contract, 40, Nil).getOrElse(fail("Failed to create employee"))
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val weeklyOverride = VacationOverride(overrideDate to overrideDate + 2.days)
    val firstResult = employee.setHoursOverride(weeklyOverride)
    firstResult.isRight shouldBe true
    val updatedEmployee = firstResult.getOrElse(fail("Failed to set first hours override"))
    val conflictingOverride = WorkingDayOverride(
      DailyHours(6).getOrElse(fail("Failed to create DailyHours")),
      Some("Another override"),
      overrideDate
    )
    val secondResult = updatedEmployee.setHoursOverride(conflictingOverride)
    secondResult.isLeft shouldBe true
    secondResult.left.getOrElse("") shouldBe "An override for the specified day or interval already exists."
  it should "prevent setting conflicting vacation overrides" in:
    val contract =
      Contract.FullTime(DateTime.now().nn - 1.month)
    val employee = createEmployee("Frank", "Green", contract, 40, Nil).getOrElse(fail("Failed to create employee"))
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val firstVacationOverride = VacationOverride(overrideDate to overrideDate + 3.days)
    val firstResult = employee.setHoursOverride(firstVacationOverride)
    firstResult.isRight shouldBe true
    val updatedEmployee = firstResult.getOrElse(fail("Failed to set first vacation override"))
    val conflictingVacationOverride = VacationOverride(overrideDate + 2.days to overrideDate + 5.days)
    val secondResult = updatedEmployee.setHoursOverride(conflictingVacationOverride)
    secondResult.isLeft shouldBe true
    secondResult.left.getOrElse("") shouldBe "An override for the specified day or interval already exists."
