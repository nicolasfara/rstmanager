package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

/** Refined constraint for a non-empty customer first name. */
type Name = DescribedAs[Not[Empty], "The customer name cannot be empty"]

/** Refined constraint for a non-empty customer surname. */
type Surname = DescribedAs[Not[Empty], "The customer surname cannot be empty"]

/** Refined constraint for an email address. */
type Email = DescribedAs[Match["^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$"], "The customer email must be a valid email address"]

/** Refined constraint for an international-style phone number. */
type PhoneNumber = DescribedAs[
  Match["\\+?[0-9]{8,15}"],
  "The customer phone number must contain 8 to 15 digits with an optional leading +",
]

/** Contact details for a customer.
  *
  * @param name
  *   Contact first name.
  * @param surname
  *   Contact surname.
  * @param email
  *   Contact email address.
  * @param phone
  *   Contact phone number.
  */
final case class ContactInfo(
    name: String :| Name,
    surname: String :| Surname,
    email: String :| Email,
    phone: String :| PhoneNumber,
) derives CanEqual:
  /** Returns a copy with a different first name. */
  def updateName(name: String :| Name): ContactInfo = this.focus(_.name).replace(name)

  /** Returns a copy with a different surname. */
  def updateSurname(surname: String :| Surname): ContactInfo = this.focus(_.surname).replace(surname)

  /** Returns a copy with a different email address. */
  def updateEmail(email: String :| Email): ContactInfo = this.focus(_.email).replace(email)

  /** Returns a copy with a different phone number. */
  def updatePhone(phone: String :| PhoneNumber): ContactInfo = this.focus(_.phone).replace(phone)

object ContactInfo:
  /** Builds `ContactInfo` from raw values after applying refined validation.
    *
    * @param name
    *   Raw first name.
    * @param surname
    *   Raw surname.
    * @param email
    *   Raw email address.
    * @param phone
    *   Raw phone number.
    */
  def createContactInfo(name: String, surname: String, email: String, phone: String): ValidatedNec[String, ContactInfo] =
    (
      name.refineValidatedNec[Name],
      surname.refineValidatedNec[Surname],
      email.refineValidatedNec[Email],
      phone.refineValidatedNec[PhoneNumber],
    ).mapN(ContactInfo.apply)
