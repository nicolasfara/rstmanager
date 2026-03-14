package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class EmployeeTest extends AnyFlatSpecLike:
  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def createEmployee(
      name: String,
      surname: String,
      contract: Contract,
      weeklyHours: Int,
      overrides: List[HoursOverride],
  ): ValidatedNec[String, Employee] =
    Employee.createEmployee(UUID.randomUUID().nn, name, surname, contract, weeklyHours, overrides)

  "Contract.createFixedTerm" should "reject an end date that is not after the start date" in:
    val startDate = DateTime.now().withTimeAtStartOfDay().nn
    val result = Contract.createFixedTerm(startDate, startDate)

    result.isValid shouldBe false

  "Employee.createEmployee" should "accumulate nested validation errors" in:
    val contract = Contract.createFullTime(DateTime.now().nn - 1.month)
    val result = Employee.createEmployee(UUID.randomUUID().nn, "", "Doe", contract, -1, Nil)

    result.isValid shouldBe false
    result.swap.foreach(_.length shouldEqual 2L)

  "An Employee" should "be inactive when the contract is terminated" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        DateTime.now().nn - 2.months,
        DateTime.now().nn - 1.day,
      ),
    )
    val employee = validOrFail(createEmployee("John", "Doe", contract, 40, Nil))

    employee.isActive shouldBe false

  it should "be active when the contract is ongoing" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        DateTime.now().nn - 1.month,
        DateTime.now().nn + 1.month,
      ),
    )
    val employee = validOrFail(createEmployee("Jane", "Smith", contract, 40, Nil))

    employee.isActive shouldBe true

  it should "allow updating budget hours when active" in:
    val contract = Contract.createFullTime(DateTime.now().nn - 1.month)
    val employee = validOrFail(createEmployee("Alice", "Johnson", contract, 40, Nil))
    val newBudgetHours = validOrFail(BudgetHours.createBudgetHours(35, Nil))

    val updatedEmployee = employee.updateBudgetHours(newBudgetHours).getOrElse(fail("Failed to update budget hours"))

    updatedEmployee.budgetHours.default shouldEqual WeeklyHours.applyUnsafe(35)

  it should "prevent updating budget hours when inactive" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        DateTime.now().nn - 2.months,
        DateTime.now().nn - 1.day,
      ),
    )
    val employee = validOrFail(createEmployee("Bob", "Brown", contract, 40, Nil))
    val newBudgetHours = validOrFail(BudgetHours.createBudgetHours(30, Nil))

    val result = employee.updateBudgetHours(newBudgetHours)

    result.isLeft shouldBe true
    result.left.getOrElse("") shouldBe "Cannot update budget hours for an inactive employee."

  it should "allow setting hours override when active and no conflict" in:
    val contract = validOrFail(Contract.createPartTime(DateTime.now().nn - 1.month, 20))
    val employee = validOrFail(createEmployee("Charlie", "Davis", contract, 20, Nil))
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val dailyOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(4, Some("Reduced hours"), overrideDate))

    val result = employee.setHoursOverride(dailyOverride)

    result.isRight shouldBe true
    result.getOrElse(fail("Failed to set hours override")).budgetHours.overrides should contain(dailyOverride)

  it should "prevent setting hours override when inactive" in:
    val contract = validOrFail(
      Contract.createFixedTerm(
        DateTime.now().nn - 2.months,
        DateTime.now().nn - 1.day,
      ),
    )
    val employee = validOrFail(createEmployee("Diana", "Evans", contract, 40, Nil))
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val dailyOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(4, Some("Reduced hours"), overrideDate))

    val result = employee.setHoursOverride(dailyOverride)

    result.isLeft shouldBe true
    result.left.getOrElse("") shouldBe "Cannot set hours override for an inactive employee."

  it should "prevent setting conflicting hours overrides when a vacation is set" in:
    val contract = Contract.createFullTime(DateTime.now().nn - 1.month)
    val employee = validOrFail(createEmployee("Eve", "Foster", contract, 40, Nil))
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val vacationOverride = validOrFail(VacationOverride.createVacationOverride(overrideDate, overrideDate + 2.days))
    val updatedEmployee = employee.setHoursOverride(vacationOverride).getOrElse(fail("Failed to set first hours override"))
    val conflictingOverride = validOrFail(WorkingDayOverride.createWorkingDayOverride(6, Some("Another override"), overrideDate))

    val secondResult = updatedEmployee.setHoursOverride(conflictingOverride)

    secondResult.isLeft shouldBe true
    secondResult.left.getOrElse("") shouldBe "An override for the specified day or interval already exists."

  it should "prevent setting conflicting vacation overrides" in:
    val contract = Contract.createFullTime(DateTime.now().nn - 1.month)
    val employee = validOrFail(createEmployee("Frank", "Green", contract, 40, Nil))
    val overrideDate = DateTime.now().withTimeAtStartOfDay().nn
    val firstVacationOverride = validOrFail(VacationOverride.createVacationOverride(overrideDate, overrideDate + 3.days))
    val updatedEmployee = employee.setHoursOverride(firstVacationOverride).getOrElse(fail("Failed to set first vacation override"))
    val conflictingVacationOverride = validOrFail(VacationOverride.createVacationOverride(overrideDate + 2.days, overrideDate + 5.days))

    val secondResult = updatedEmployee.setHoursOverride(conflictingVacationOverride)

    secondResult.isLeft shouldBe true
    secondResult.left.getOrElse("") shouldBe "An override for the specified day or interval already exists."

  it should "update the nested name through optics" in:
    val contract = Contract.createFullTime(DateTime.now().nn - 1.month)
    val employee = validOrFail(createEmployee("Grace", "Hill", contract, 40, Nil))

    val updated = employee.updateName("Georgia".refineUnsafe[Name])

    updated.info.name.toString shouldEqual "Georgia"
    updated.info.surname shouldEqual employee.info.surname
    updated.contract shouldEqual employee.contract
    updated.budgetHours shouldEqual employee.budgetHours
end EmployeeTest
