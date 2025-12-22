package io.github.nicolasfara.rstmanager.hr.domain

import java.time.LocalDate

final case class Employee(id: EmployeeId, info: EmployeeInfo, contract: Contract, budgetHours: BudgetHours):
  private given CanEqual[LocalDate, LocalDate] = CanEqual.derived

  /** Checks if the employee is currently active based on their contract type and dates. */
  def isActive: Boolean = contract match
    case Contract.FullTime(_)           => true
    case Contract.PartTime(_, _)        => true
    case Contract.FixedTerm(_, endDate) => endDate.isAfter(java.time.LocalDate.now())

  def updateBudgetHours(newBudgetHours: BudgetHours): Either[String, Employee] =
    Either.cond(
      isActive,
      copy(budgetHours = newBudgetHours),
      "Cannot update budget hours for an inactive employee."
    )

  def updateContract(newContract: Contract): Employee = copy(contract = newContract)

  def setHoursOverride(hoursOverride: HoursOverride): Employee =
    val updatedOverrides = budgetHours.overrides.filterNot {
      case DayOfWeekHoursOverride(_, _, dayOfWeek) =>
        hoursOverride match
          case DayOfWeekHoursOverride(_, _, newDayOfWeek) => dayOfWeek == newDayOfWeek
          case _                                          => false
      case RangeHoursOverride(_, _, from, to) =>
        hoursOverride match
          case RangeHoursOverride(_, _, newFrom, newTo) => from == newFrom && to == newTo
          case _                                        => false
    } :+ hoursOverride

    val updatedBudgetHours = budgetHours.copy(overrides = updatedOverrides)
    copy(budgetHours = updatedBudgetHours)
