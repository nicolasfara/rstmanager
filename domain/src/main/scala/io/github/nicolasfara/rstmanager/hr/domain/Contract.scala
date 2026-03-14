package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.{ Validated, ValidatedNec }
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.cats.*

/** Employment contract variants supported by the domain.
  *
  * The contract controls whether the employee is permanent, fixed-term, or part-time.
  */
enum Contract:
  case FullTime(startDate: DateTime)
  case FixedTerm(startDate: DateTime, endDate: DateTime)
  case PartTime(startDate: DateTime, weeklyHours: WeeklyHours)

object Contract:
  /** Creates a full-time contract starting on the given date. */
  def createFullTime(startDate: DateTime): Contract = Contract.FullTime(startDate)

  /** Creates a fixed-term contract when the end date is after the start date.
    *
    * @param startDate
    *   Contract start date.
    * @param endDate
    *   Contract end date.
    */
  def createFixedTerm(startDate: DateTime, endDate: DateTime): ValidatedNec[String, Contract] =
    Validated.condNec(
      endDate.isAfter(startDate),
      Contract.FixedTerm(startDate, endDate),
      "The fixed-term contract end date must be after the start date",
    )

  /** Creates a part-time contract after validating the weekly hours budget. */
  def createPartTime(startDate: DateTime, weeklyHours: Int): ValidatedNec[String, Contract] =
    WeeklyHours.validatedNec(weeklyHours).map(Contract.PartTime(startDate, _))
