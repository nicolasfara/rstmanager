package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.Validated
import io.github.nicolasfara.rstmanager.*

opaque type Name = String :| Not[Empty]
opaque type Surname = String :| Not[Empty]

object Name:
  def apply(value: String): Validated[String, Name] = value.refineValidated

object Surname:
  def apply(value: String): Validated[String, Surname] = value.refineValidated

final case class EmployeeInfo(name: Name, surname: Surname) derives CanEqual
