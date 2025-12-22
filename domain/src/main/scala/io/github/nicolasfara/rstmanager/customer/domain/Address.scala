package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Street = String :| Not[Empty]
type City = String :| Not[Empty]
type CAP = String :| Match["[0-9]{5}"]
type Nation = String :| Not[Empty]

final case class Address(street: Street, city: City, cap: CAP, nation: Nation)

object Street:
  def apply(value: String): Either[String, Street] = refineEither(value)

object City:
  def apply(value: String): Either[String, City] = refineEither(value)

object CAP:
  def apply(value: String): Either[String, CAP] = refineEither(value)

object Nation:
  def apply(value: String): Either[String, Nation] = refineEither(value)
