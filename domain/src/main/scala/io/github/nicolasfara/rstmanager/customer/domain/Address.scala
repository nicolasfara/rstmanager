package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

/** Refined constraint for a non-empty street value. */
type Street = DescribedAs[Not[Empty], "The street cannot be empty"]

/** Refined constraint for a non-empty city value. */
type City = DescribedAs[Not[Empty], "The city cannot be empty"]

/** Refined constraint for a five-digit postal code. */
type PostalCode = DescribedAs[Match["[0-9]{5}"], "The postal code must contain exactly 5 digits"]

/** Refined constraint for a non-empty country value. */
type Country = DescribedAs[Not[Empty], "The country cannot be empty"]

/** Postal address attached to a customer record.
  *
  * @param street
  *   Street name and house number.
  * @param city
  *   City for the address.
  * @param postalCode
  *   Postal code in five-digit format.
  * @param country
  *   Country name.
  */
final case class Address(
    street: String :| Street,
    city: String :| City,
    postalCode: String :| PostalCode,
    country: String :| Country,
) derives CanEqual:
  /** Returns a copy with a different street value. */
  def updateStreet(street: String :| Street): Address = this.focus(_.street).replace(street)

  /** Returns a copy with a different city value. */
  def updateCity(city: String :| City): Address = this.focus(_.city).replace(city)

  /** Returns a copy with a different postal code. */
  def updatePostalCode(postalCode: String :| PostalCode): Address = this.focus(_.postalCode).replace(postalCode)

  /** Returns a copy with a different country value. */
  def updateCountry(country: String :| Country): Address = this.focus(_.country).replace(country)

object Address:
  /** Builds an `Address` from raw strings, validating each field first.
    *
    * @param street
    *   Raw street value.
    * @param city
    *   Raw city value.
    * @param postalCode
    *   Raw postal code value.
    * @param country
    *   Raw country value.
    */
  def createAddress(street: String, city: String, postalCode: String, country: String): ValidatedNec[String, Address] =
    (
      street.refineValidatedNec[Street],
      city.refineValidatedNec[City],
      postalCode.refineValidatedNec[PostalCode],
      country.refineValidatedNec[Country],
    ).mapN(Address.apply)
