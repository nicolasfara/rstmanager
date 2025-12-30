package io.github.nicolasfara.rstmanager.hr.domain

import com.github.nscala_time.time.Imports.*

final case class Employee(id: EmployeeId, info: EmployeeInfo, contract: Contract, budgetHours: BudgetHours):
  given CanEqual[DateTime, DateTime] = CanEqual.derived
  given CanEqual[Interval, Interval] = CanEqual.derived

  /** Checks if the employee is currently active based on their contract type and dates. */
  def isActive: Boolean = contract match
    case Contract.FullTime(_)           => true
    case Contract.PartTime(_, _)        => true
    case Contract.FixedTerm(_, endDate) => endDate.isAfter(DateTime.now())

  def updateBudgetHours(newBudgetHours: BudgetHours): Either[String, Employee] =
    Either.cond(
      isActive,
      copy(budgetHours = newBudgetHours),
      "Cannot update budget hours for an inactive employee."
    )

  def updateContract(newContract: Contract): Employee = copy(contract = newContract)

  def setHoursOverride(hoursOverride: HoursOverride): Either[String, Employee] = for
    _ <- Either.cond(isActive, (), "Cannot set hours override for an inactive employee.")
    _ <- alreadyContainsOverride(hoursOverride)
  yield {
    val updatedOverrides = budgetHours.overrides.filter {
      case WorkingDayOverride(_, _, day) =>
        hoursOverride match {
          case WorkingDayOverride(_, _, newDay) => day != newDay
          case _                                => true
        }
      case _ => true
    } :+ hoursOverride
    val updatedBudgetHours = budgetHours.copy(overrides = updatedOverrides)
    copy(budgetHours = updatedBudgetHours)
  }

  private def alreadyContainsOverride(hoursOverride: HoursOverride): Either[String, Unit] =
    val exists = hoursOverride match {
      case VacationOverride(interval) =>
        budgetHours.overrides.exists {
          case VacationOverride(existingInterval) => existingInterval.overlaps(interval)
          case WorkingDayOverride(_, _, day)      => interval.contains(day)
        }
      case WorkingDayOverride(hours, reason, day) =>
        budgetHours.overrides.exists {
          case VacationOverride(interval) => interval.contains(day)
          case _                          => false
        }
    }
    if exists then Left("An override for the specified day or interval already exists.")
    else Right(())
