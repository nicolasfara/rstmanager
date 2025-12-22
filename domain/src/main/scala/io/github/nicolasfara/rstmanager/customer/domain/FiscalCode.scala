package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.string.*

type FiscalCode =
  String :| Match["[A-Z]{6}[0-9]{2}[A-Z][0-9]{2}[A-Z][0-9]{3}[A-Z]"]

object FiscalCode:
  def apply(value: String): Either[String, FiscalCode] = refineEither(value.toUpperCase)
