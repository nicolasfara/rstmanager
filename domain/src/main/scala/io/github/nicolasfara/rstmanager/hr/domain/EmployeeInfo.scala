package io.github.nicolasfara.rstmanager.hr.domain

import io.github.nicolasfara.rstmanager.*

import cats.data.Validated

opaque type Name = String :| Not[Empty]
opaque type Surname = String :| Not[Empty]

object Name:
  def apply(value: String): Validated[String, Name] = value.refineValidated

object Surname:
  def apply(value: String): Validated[String, Surname] = value.refineValidated

final case class EmployeeInfo(name: Name, surname: Surname) derives CanEqual
