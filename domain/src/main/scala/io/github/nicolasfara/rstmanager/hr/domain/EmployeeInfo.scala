package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

type Name = DescribedAs[Not[Empty], "The employee name cannot be empty"]
type Surname = DescribedAs[Not[Empty], "The employee surname cannot be empty"]

final case class EmployeeInfo(name: String :| Name, surname: String :| Surname) derives CanEqual:
  def updateName(name: String :| Name): EmployeeInfo = this.focus(_.name).replace(name)

  def updateSurname(surname: String :| Surname): EmployeeInfo = this.focus(_.surname).replace(surname)

object EmployeeInfo:
  def createEmployeeInfo(name: String, surname: String): ValidatedNec[String, EmployeeInfo] =
    (
      name.refineValidatedNec[Name],
      surname.refineValidatedNec[Surname],
    ).mapN(EmployeeInfo.apply)
