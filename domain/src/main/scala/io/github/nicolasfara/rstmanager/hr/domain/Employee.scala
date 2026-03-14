package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

import cats.data.{Validated, ValidatedNec}
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import monocle.syntax.all.*

final case class Employee(id: EmployeeId, info: EmployeeInfo, contract: Contract, budgetHours: BudgetHours):
  /** Checks if the employee is currently active based on their contract type and dates. */
  def isActiveAt(referenceDate: DateTime): Boolean = contract match
    case Contract.FullTime(_) => true
    case Contract.PartTime(_, _) => true
    case Contract.FixedTerm(_, endDate) => endDate.isAfter(referenceDate)

  def isActive: Boolean = isActiveAt(DateTime.now())

  def updateBudgetHours(newBudgetHours: BudgetHours): Either[String, Employee] =
    Either.cond(
      isActive,
      this.focus(_.budgetHours).replace(newBudgetHours),
      "Cannot update budget hours for an inactive employee.",
    )

  def updateInfo(newInfo: EmployeeInfo): Employee = this.focus(_.info).replace(newInfo)

  def updateContract(newContract: Contract): Employee = this.focus(_.contract).replace(newContract)

  def updateName(name: String :| Name): Employee = this.focus(_.info.name).replace(name)

  def updateSurname(surname: String :| Surname): Employee = this.focus(_.info.surname).replace(surname)

  def setHoursOverride(hoursOverride: HoursOverride): Either[String, Employee] = for
    _ <- Either.cond(isActive, (), "Cannot set hours override for an inactive employee.")
    _ <- Either.cond(
      !budgetHours.hasConflict(hoursOverride),
      (),
      "An override for the specified day or interval already exists.",
    )
  yield this.focus(_.budgetHours).modify(_.setOverride(hoursOverride))
end Employee

object Employee:
  def createEmployee(
      id: UUID,
      name: String,
      surname: String,
      contract: Contract,
      defaultWeeklyHours: Int,
      overrides: List[HoursOverride],
  ): ValidatedNec[String, Employee] =
    (
      Validated.validNec(id),
      EmployeeInfo.createEmployeeInfo(name, surname),
      Validated.validNec(contract),
      BudgetHours.createBudgetHours(defaultWeeklyHours, overrides),
    ).mapN(Employee.apply)
