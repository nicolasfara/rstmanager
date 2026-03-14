package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import monocle.syntax.all.*

/** Employee aggregate containing personal information, contract, and available budget hours.
  *
  * @param id
  *   Stable employee identifier.
  * @param info
  *   Employee name and surname.
  * @param contract
  *   Contract describing the employment relationship.
  * @param budgetHours
  *   Weekly hours budget with optional overrides.
  */
final case class Employee(id: EmployeeId, info: EmployeeInfo, contract: Contract, budgetHours: BudgetHours):
  /** Checks whether the employee is active on a given reference date. */
  def isActiveAt(referenceDate: DateTime): Boolean = contract match
    case Contract.FullTime(_) => true
    case Contract.PartTime(_, _) => true
    case Contract.FixedTerm(_, endDate) => endDate.isAfter(referenceDate)

  /** Checks whether the employee is active right now. */
  def isActive: Boolean = isActiveAt(DateTime.now())

  /** Updates the budget hours if the employee is currently active. */
  def updateBudgetHours(newBudgetHours: BudgetHours): Either[String, Employee] =
    Either.cond(
      isActive,
      this.focus(_.budgetHours).replace(newBudgetHours),
      "Cannot update budget hours for an inactive employee.",
    )

  /** Returns a copy with updated personal information. */
  def updateInfo(newInfo: EmployeeInfo): Employee = this.focus(_.info).replace(newInfo)

  /** Returns a copy with an updated contract. */
  def updateContract(newContract: Contract): Employee = this.focus(_.contract).replace(newContract)

  /** Returns a copy with an updated first name. */
  def updateName(name: String :| Name): Employee = this.focus(_.info.name).replace(name)

  /** Returns a copy with an updated surname. */
  def updateSurname(surname: String :| Surname): Employee = this.focus(_.info.surname).replace(surname)

  /** Adds an hours override when the employee is active and no conflict exists. */
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
  /** Creates an `Employee` aggregate from raw identifier and validated value objects.
    *
    * @param id
    *   Employee identifier.
    * @param name
    *   Raw first name.
    * @param surname
    *   Raw surname.
    * @param contract
    *   Employment contract.
    * @param defaultWeeklyHours
    *   Weekly hours budget before overrides.
    * @param overrides
    *   Initial hours overrides.
    */
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
