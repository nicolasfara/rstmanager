package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.{Validated, ValidatedNec}
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.cats.*

enum Contract:
  case FullTime(startDate: DateTime)
  case FixedTerm(startDate: DateTime, endDate: DateTime)
  case PartTime(startDate: DateTime, weeklyHours: WeeklyHours)

object Contract:
  def createFullTime(startDate: DateTime): Contract = Contract.FullTime(startDate)

  def createFixedTerm(startDate: DateTime, endDate: DateTime): ValidatedNec[String, Contract] =
    Validated.condNec(
      endDate.isAfter(startDate),
      Contract.FixedTerm(startDate, endDate),
      "The fixed-term contract end date must be after the start date",
    )

  def createPartTime(startDate: DateTime, weeklyHours: Int): ValidatedNec[String, Contract] =
    WeeklyHours.validatedNec(weeklyHours).map(Contract.PartTime(startDate, _))
