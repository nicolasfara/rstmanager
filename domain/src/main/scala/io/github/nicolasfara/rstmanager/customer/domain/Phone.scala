package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.string.*

type PhoneNumber = String :| Match["\\+?[0-9]{8,15}"]

object PhoneNumber:
  def apply(value: String): Either[String, PhoneNumber] = refineEither(value)
