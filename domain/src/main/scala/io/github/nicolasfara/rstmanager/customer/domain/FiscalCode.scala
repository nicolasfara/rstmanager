package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.ValidatedNec
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs

type FiscalCodeRule =
  DescribedAs[Match["[A-Z]{6}[0-9]{2}[A-Z][0-9]{2}[A-Z][0-9]{3}[A-Z]"], "The fiscal code must match the expected Italian format"]
type FiscalCode = String :| FiscalCodeRule

object FiscalCode:
  def createFiscalCode(value: String): ValidatedNec[String, FiscalCode] =
    value.toUpperCase.nn.refineValidatedNec[FiscalCodeRule]
