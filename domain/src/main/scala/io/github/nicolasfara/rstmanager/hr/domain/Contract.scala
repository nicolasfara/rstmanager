package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.{ Validated, ValidatedNec }
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.cats.*

/**
 * Employment contract variants supported by the domain.
 *
 * The contract controls whether the employee is permanent, fixed-term, or part-time. Contract dates are calendar days, independent of any time zone;
 * both the start date and the fixed-term end date are inclusive (the end date is the last day of employment).
 */
enum Contract:
  case FullTime(startDate: LocalDate)
  case FixedTerm(startDate: LocalDate, endDate: LocalDate)
  case PartTime(startDate: LocalDate, weeklyHours: WeeklyHours)

  /** First day of employment. */
  def startsOn: LocalDate = this match
    case FullTime(startDate) => startDate
    case FixedTerm(startDate, _) => startDate
    case PartTime(startDate, _) => startDate

  /** Last day of employment, for contracts that have one. */
  def endsOn: Option[LocalDate] = this match
    case FixedTerm(_, endDate) => Some(endDate)
    case FullTime(_) | PartTime(_, _) => None

  /** Whether `day` falls within the contract, both bounds included. */
  def covers(day: LocalDate): Boolean = !day.isBefore(startsOn) && endsOn.forall(end => !day.isAfter(end))
end Contract

object Contract:
  /** Creates a full-time contract starting on the given date. */
  def createFullTime(startDate: LocalDate): Contract = Contract.FullTime(startDate)

  /**
   * Creates a fixed-term contract when the end date is on or after the start date (a one-day contract is valid).
   *
   * @param startDate
   *   First day of employment.
   * @param endDate
   *   Last day of employment.
   */
  def createFixedTerm(startDate: LocalDate, endDate: LocalDate): ValidatedNec[String, Contract] =
    Validated.condNec(
      !endDate.isBefore(startDate),
      Contract.FixedTerm(startDate, endDate),
      "The fixed-term contract end date must be on or after the start date",
    )

  /** Creates a part-time contract after validating the weekly hours budget. */
  def createPartTime(startDate: LocalDate, weeklyHours: Int): ValidatedNec[String, Contract] =
    WeeklyHours.validatedNec(weeklyHours).map(Contract.PartTime(startDate, _))
end Contract
