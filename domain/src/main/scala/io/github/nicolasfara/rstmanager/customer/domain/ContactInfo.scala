package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

type Name = DescribedAs[Not[Empty], "The customer name cannot be empty"]
type Surname = DescribedAs[Not[Empty], "The customer surname cannot be empty"]
type Email = DescribedAs[Match["^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$"], "The customer email must be a valid email address"]
type PhoneNumber = DescribedAs[
  Match["\\+?[0-9]{8,15}"],
  "The customer phone number must contain 8 to 15 digits with an optional leading +",
]

final case class ContactInfo(
    name: String :| Name,
    surname: String :| Surname,
    email: String :| Email,
    phone: String :| PhoneNumber,
) derives CanEqual:
  def updateName(name: String :| Name): ContactInfo = this.focus(_.name).replace(name)

  def updateSurname(surname: String :| Surname): ContactInfo = this.focus(_.surname).replace(surname)

  def updateEmail(email: String :| Email): ContactInfo = this.focus(_.email).replace(email)

  def updatePhone(phone: String :| PhoneNumber): ContactInfo = this.focus(_.phone).replace(phone)

object ContactInfo:
  def createContactInfo(name: String, surname: String, email: String, phone: String): ValidatedNec[String, ContactInfo] =
    (
      name.refineValidatedNec[Name],
      surname.refineValidatedNec[Surname],
      email.refineValidatedNec[Email],
      phone.refineValidatedNec[PhoneNumber],
    ).mapN(ContactInfo.apply)
