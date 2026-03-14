package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.ValidatedNec
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs

/** Validation rule for Italian fiscal codes. */
type FiscalCodeRule =
  DescribedAs[Match["[A-Z]{6}[0-9]{2}[A-Z][0-9]{2}[A-Z][0-9]{3}[A-Z]"], "The fiscal code must match the expected Italian format"]

/** Fiscal code normalized as an uppercase refined string. */
type FiscalCode = String :| FiscalCodeRule

object FiscalCode:
  /** Validates and normalizes a raw fiscal code string.
    *
    * The input is converted to uppercase before refinement.
    *
    * @param value
    *   Raw fiscal code provided by the caller.
    */
  def createFiscalCode(value: String): ValidatedNec[String, FiscalCode] =
    value.toUpperCase.nn.refineValidatedNec[FiscalCodeRule]
