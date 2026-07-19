package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.{ Validated, ValidatedNec }
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs

/** Validation rule for Italian fiscal codes. */
type FiscalCodeRule =
  DescribedAs[Match["[A-Z]{6}[0-9]{2}[A-Z][0-9]{2}[A-Z][0-9]{3}[A-Z]"], "The fiscal code must match the expected Italian format"]

/** Fiscal code normalized as an uppercase refined string. */
type FiscalCode = String :| FiscalCodeRule

/** Validation rule for Italian VAT numbers (11 digits; the check digit is verified by the smart constructor). */
type VatNumberRule = DescribedAs[Match["[0-9]{11}"], "The VAT number must contain exactly 11 digits"]

/** Italian VAT number as a refined string. */
type VatNumber = String :| VatNumberRule

/** Validation rule for the fiscal identifier stored on a customer: fiscal code or VAT number. */
type TaxCodeRule = DescribedAs[
  Match["[A-Z]{6}[0-9]{2}[A-Z][0-9]{2}[A-Z][0-9]{3}[A-Z]|[0-9]{11}"],
  "The tax code must be an Italian fiscal code or an 11-digit VAT number",
]

/** Fiscal identifier of a customer: fiscal code for individuals, VAT number for companies. */
type TaxCode = String :| TaxCodeRule

object FiscalCode:
  /**
   * Validates and normalizes a raw fiscal code string.
   *
   * The input is converted to uppercase before refinement.
   *
   * @param value
   *   Raw fiscal code provided by the caller.
   */
  def createFiscalCode(value: String): ValidatedNec[String, FiscalCode] =
    value.toUpperCase.nn.refineValidatedNec[FiscalCodeRule]

object VatNumber:
  /**
   * Validates a raw Italian VAT number: 11 digits with a valid check digit.
   *
   * @param value
   *   Raw VAT number provided by the caller.
   */
  def createVatNumber(value: String): ValidatedNec[String, VatNumber] =
    value.trim.nn.refineValidatedNec[VatNumberRule].andThen { vat =>
      if hasValidCheckDigit(vat) then Validated.validNec(vat)
      else Validated.invalidNec("The VAT number check digit is invalid")
    }

  private def hasValidCheckDigit(vat: String): Boolean =
    val digits = vat.map(_.asDigit)
    val total = digits
      .take(10)
      .zipWithIndex
      .map { (digit, index) =>
        if index % 2 == 0 then digit
        else
          val doubled = digit * 2
          if doubled > 9 then doubled - 9 else doubled
      }
      .sum
    (10 - total % 10) % 10 == digits(10)
end VatNumber
