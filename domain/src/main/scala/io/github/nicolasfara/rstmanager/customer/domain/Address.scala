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

/**
 * Postal address attached to a customer record. Every field is optional, so a partially known address can be stored.
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
    street: Option[String :| Street],
    city: Option[String :| City],
    postalCode: Option[String :| PostalCode],
    country: Option[String :| Country],
) derives CanEqual:
  /** Returns a copy with a different street value. */
  def updateStreet(street: Option[String :| Street]): Address = this.focus(_.street).replace(street)

  /** Returns a copy with a different city value. */
  def updateCity(city: Option[String :| City]): Address = this.focus(_.city).replace(city)

  /** Returns a copy with a different postal code. */
  def updatePostalCode(postalCode: Option[String :| PostalCode]): Address = this.focus(_.postalCode).replace(postalCode)

  /** Returns a copy with a different country value. */
  def updateCountry(country: Option[String :| Country]): Address = this.focus(_.country).replace(country)

object Address:
  /** Address with every field unset. */
  val empty: Address = Address(None, None, None, None)

  /**
   * Builds an `Address` from raw optional strings: blank values become absent, present ones are validated.
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
  def createAddress(
      street: Option[String],
      city: Option[String],
      postalCode: Option[String],
      country: Option[String],
  ): ValidatedNec[String, Address] =
    (
      normalized(street).traverse(_.refineValidatedNec[Street]),
      normalized(city).traverse(_.refineValidatedNec[City]),
      normalized(postalCode).traverse(_.refineValidatedNec[PostalCode]),
      normalized(country).traverse(_.refineValidatedNec[Country]),
    ).mapN(Address.apply)

  private def normalized(value: Option[String]): Option[String] = value.map(_.trim.nn).filter(_.nonEmpty)
end Address
