package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.Validated
import io.github.nicolasfara.rstmanager.*

opaque type Name = String :| Not[Empty]

object Name:
  def apply(value: String): Validated[String, Name] = value.refineValidated

opaque type Surname = String :| Not[Empty]

object Surname:
  def apply(surname: String): Validated[String, Surname] = surname.refineValidated

opaque type Email = String :| Match["^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$"]

object Email:
  def apply(email: String): Validated[String, Email] = email.refineValidated

opaque type PhoneNumber = String :| Match["\\+?[0-9]{8,15}"]

object PhoneNumber:
  def apply(value: String): Validated[String, PhoneNumber] = value.refineValidated

final case class ContactInfo(name: Name, surname: Surname, email: Email, phone: PhoneNumber) derives CanEqual
