package io.github.nicolasfara.rstmanager.warehouse.domain.product

import java.util.Locale

import io.github.nicolasfara.rstmanager.warehouse.domain.{ UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

/** Refined constraint for a non-empty manufacturer part number (MPN). */
type MpnCode = DescribedAs[Not[Empty], "Il codice MPN non può essere vuoto"]

/** Refined constraint for a non-empty internal product code. */
type ProductCode = DescribedAs[Not[Empty], "Il codice interno del prodotto non può essere vuoto"]

/** Refined constraint for a non-empty product description. */
type ProductDescription = DescribedAs[Not[Empty], "La descrizione del prodotto non può essere vuota"]

/**
 * A manufacturer part number under which a supplier sells the product.
 *
 * @param code
 *   Supplier's part code, trimmed and upper-cased so that equal codes compare equal.
 * @param supplierId
 *   Supplier selling the product under this code.
 * @param description
 *   Optional supplier-side description.
 */
final case class Mpn(code: String :| MpnCode, supplierId: SupplierId, description: Option[String]) derives CanEqual:
  /** The identity of the MPN: the same code from another supplier is a different MPN. */
  def key: (SupplierId, String) = (supplierId, code)

object Mpn:
  /** Builds an MPN from raw values, normalizing the code (trimmed, upper case) and dropping a blank description. */
  def createMpn(code: String, supplierId: SupplierId, description: Option[String]): ValidatedNec[String, Mpn] =
    (
      ProductInfo.normalizedCode(code).refineValidatedNec[MpnCode],
      Validated.validNec(supplierId),
      Validated.validNec(ProductInfo.normalized(description)),
    ).mapN(Mpn.apply)

/**
 * Master data of a warehouse product.
 *
 * @param id
 *   Stable product identifier.
 * @param internalCode
 *   Company code of the product, trimmed and upper-cased.
 * @param description
 *   What the product is.
 * @param productType
 *   Optional free-form category ("tipologia").
 * @param notes
 *   Optional free-form notes.
 * @param unit
 *   Unit in which the product is stocked and priced.
 * @param customerPrice
 *   Current price per unit charged to customers.
 * @param mpns
 *   Part numbers under which suppliers sell the product; at most one per (supplier, code), checked by [[ProductInfo.checkInvariants]].
 */
final case class ProductInfo(
    id: ProductId,
    internalCode: String :| ProductCode,
    description: String :| ProductDescription,
    productType: Option[String],
    notes: Option[String],
    unit: UnitOfMeasure,
    customerPrice: UnitPrice,
    mpns: List[Mpn],
) derives CanEqual:
  /** Suppliers of the product, derived from its MPNs, in first-appearance order. */
  def suppliers: List[SupplierId] = mpns.map(_.supplierId).distinct

  /** Rules every product must satisfy however it was built: no (supplier, MPN code) pair appears twice. */
  def checkInvariants: ValidatedNec[ProductError, ProductInfo] =
    val keys = mpns.map(_.key)
    keys.diff(keys.distinct).distinct.traverse_((supplierId, code) => ProductError.DuplicateMpn(supplierId, code).invalidNec[Unit]).as(this)
end ProductInfo

object ProductInfo:
  /** Creates a `ProductInfo` from raw values, normalizing the internal code (trimmed, upper case) and dropping blank optional values. */
  def createProductInfo(
      id: ProductId,
      internalCode: String,
      description: String,
      productType: Option[String],
      notes: Option[String],
      unit: UnitOfMeasure,
      customerPrice: UnitPrice,
      mpns: List[Mpn],
  ): ValidatedNec[String, ProductInfo] =
    (
      Validated.validNec(id),
      normalizedCode(internalCode).refineValidatedNec[ProductCode],
      description.trim.nn.refineValidatedNec[ProductDescription],
      Validated.validNec(normalized(productType)),
      Validated.validNec(normalized(notes)),
      Validated.validNec(unit),
      Validated.validNec(customerPrice),
      Validated.validNec(mpns),
    ).mapN(ProductInfo.apply)

  /** A code as stored: no surrounding blanks, upper case. */
  def normalizedCode(raw: String): String = raw.trim.nn.toUpperCase(Locale.ROOT).nn

  private[product] def normalized(value: Option[String]): Option[String] = value.map(_.trim.nn).filter(_.nonEmpty)
end ProductInfo
