package io.github.nicolasfara.rstmanager.warehouse.domain.supplier

import java.util.Locale

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

/** Refined constraint for a non-empty supplier name. */
type SupplierName = DescribedAs[Not[Empty], "Il nome del fornitore non può essere vuoto"]

/** Refined constraint for a supplier email address. */
type SupplierEmail = DescribedAs[Match["^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$"], "L'email del fornitore deve essere un indirizzo valido"]

/**
 * Supplier master-data record (the state held by [[SupplierAggregate]]). Suppliers may be foreign, so the VAT number, phone, and address are free
 * text.
 *
 * @param id
 *   Stable supplier identifier.
 * @param name
 *   Business name of the supplier.
 * @param vatNumber
 *   VAT number, without blanks and in upper case.
 * @param email
 *   Contact email address.
 * @param phone
 *   Contact phone number.
 * @param address
 *   Postal address, on one line.
 * @param notes
 *   Free-form notes about the supplier.
 */
final case class Supplier(
    id: SupplierId,
    name: String :| SupplierName,
    vatNumber: Option[String],
    email: Option[String :| SupplierEmail],
    phone: Option[String],
    address: Option[String],
    notes: Option[String],
) derives CanEqual

object Supplier:
  /**
   * Creates a `Supplier` from raw input values: blanks around every value are dropped, blank optional values become absent, and the VAT number loses
   * its inner blanks and is upper-cased.
   */
  def createSupplier(
      id: SupplierId,
      name: String,
      vatNumber: Option[String],
      email: Option[String],
      phone: Option[String],
      address: Option[String],
      notes: Option[String],
  ): ValidatedNec[String, Supplier] =
    (
      Validated.validNec(id),
      name.trim.nn.refineValidatedNec[SupplierName],
      Validated.validNec(normalized(vatNumber).map(_.filterNot(_.isWhitespace).toUpperCase(Locale.ROOT).nn)),
      normalized(email).traverse(_.refineValidatedNec[SupplierEmail]),
      Validated.validNec(normalized(phone)),
      Validated.validNec(normalized(address)),
      Validated.validNec(normalized(notes)),
    ).mapN(Supplier.apply)

  private def normalized(value: Option[String]): Option[String] = value.map(_.trim.nn).filter(_.nonEmpty)
end Supplier
