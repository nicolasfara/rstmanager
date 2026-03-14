package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

/** Refined constraint for a non-empty employee first name. */
type Name = DescribedAs[Not[Empty], "The employee name cannot be empty"]

/** Refined constraint for a non-empty employee surname. */
type Surname = DescribedAs[Not[Empty], "The employee surname cannot be empty"]

/** Employee personal information stored independently from contract data.
  *
  * @param name
  *   Employee first name.
  * @param surname
  *   Employee surname.
  */
final case class EmployeeInfo(name: String :| Name, surname: String :| Surname) derives CanEqual:
  /** Returns a copy with an updated first name. */
  def updateName(name: String :| Name): EmployeeInfo = this.focus(_.name).replace(name)

  /** Returns a copy with an updated surname. */
  def updateSurname(surname: String :| Surname): EmployeeInfo = this.focus(_.surname).replace(surname)

object EmployeeInfo:
  /** Creates `EmployeeInfo` from raw name components. */
  def createEmployeeInfo(name: String, surname: String): ValidatedNec[String, EmployeeInfo] =
    (
      name.refineValidatedNec[Name],
      surname.refineValidatedNec[Surname],
    ).mapN(EmployeeInfo.apply)
