package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.Validated
import io.github.nicolasfara.rstmanager.*

opaque type Street = String :| Not[Empty]
opaque type City = String :| Not[Empty]
opaque type PostalCode = String :| Match["[0-9]{5}"]
opaque type Country = String :| Not[Empty]

object Street:
  def apply(value: String): Validated[String, Street] = value.refineValidated

object City:
  def apply(value: String): Validated[String, City] = value.refineValidated

object PostalCode:
  def apply(value: String): Validated[String, PostalCode] = value.refineValidated

object Country:
  def apply(value: String): Validated[String, Country] = value.refineValidated

final case class Address(street: Street, city: City, cap: PostalCode, nation: Country) derives CanEqual
