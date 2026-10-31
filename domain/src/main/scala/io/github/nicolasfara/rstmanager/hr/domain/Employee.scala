package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import monocle.syntax.all.*

/**
 * Employee master-data record (the state held by [[EmployeeAggregate]]): personal information, contract, and available budget hours.
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
  /** Checks whether the employee is employed on a given calendar day (contract start and end dates included). */
  def isActiveAt(day: LocalDate): Boolean = contract.covers(day)

  /** Updates the budget hours if the employee is active on `referenceDate`. */
  def updateBudgetHours(newBudgetHours: BudgetHours, referenceDate: LocalDate): Either[String, Employee] =
    Either.cond(
      isActiveAt(referenceDate),
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

  /** Adds an hours override when the employee is active on `referenceDate` and no conflict exists. */
  def setHoursOverride(hoursOverride: HoursOverride, referenceDate: LocalDate): Either[String, Employee] = for
    _ <- Either.cond(isActiveAt(referenceDate), (), "Cannot set hours override for an inactive employee.")
    _ <- Either.cond(
      !budgetHours.hasConflict(hoursOverride),
      (),
      "An override for the specified day or interval already exists.",
    )
  yield this.focus(_.budgetHours).modify(_.setOverride(hoursOverride))

  /** Rules every employee must satisfy however it was built; the aggregate enforces them on every create and update. */
  def checkInvariants: ValidatedNec[EmployeeError, Employee] =
    Validated.condNec(!budgetHours.hasConflictingOverrides, this, EmployeeError.ConflictingHoursOverrides)
end Employee

object Employee:
  /**
   * Creates an `Employee` from a raw identifier and validated value objects.
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
end Employee
