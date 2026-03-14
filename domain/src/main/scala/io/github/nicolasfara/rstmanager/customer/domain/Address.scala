package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

type Street = DescribedAs[Not[Empty], "The street cannot be empty"]
type City = DescribedAs[Not[Empty], "The city cannot be empty"]
type PostalCode = DescribedAs[Match["[0-9]{5}"], "The postal code must contain exactly 5 digits"]
type Country = DescribedAs[Not[Empty], "The country cannot be empty"]

final case class Address(
    street: String :| Street,
    city: String :| City,
    postalCode: String :| PostalCode,
    country: String :| Country,
) derives CanEqual:
  def updateStreet(street: String :| Street): Address = this.focus(_.street).replace(street)

  def updateCity(city: String :| City): Address = this.focus(_.city).replace(city)

  def updatePostalCode(postalCode: String :| PostalCode): Address = this.focus(_.postalCode).replace(postalCode)

  def updateCountry(country: String :| Country): Address = this.focus(_.country).replace(country)

object Address:
  def createAddress(street: String, city: String, postalCode: String, country: String): ValidatedNec[String, Address] =
    (
      street.refineValidatedNec[Street],
      city.refineValidatedNec[City],
      postalCode.refineValidatedNec[PostalCode],
      country.refineValidatedNec[Country],
    ).mapN(Address.apply)
