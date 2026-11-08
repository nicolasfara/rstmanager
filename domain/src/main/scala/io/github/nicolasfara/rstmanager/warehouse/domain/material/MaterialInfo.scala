package io.github.nicolasfara.rstmanager.warehouse.domain.material

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

/** Refined constraint for a non-empty internal material code. */
type MaterialCode = DescribedAs[Not[Empty], "Il codice interno del materiale non può essere vuoto"]

/** Refined constraint for a non-empty material description. */
type MaterialDescription = DescribedAs[Not[Empty], "La descrizione del materiale non può essere vuota"]

/**
 * A manufacturer part number under which a supplier sells the material.
 *
 * @param code
 *   Supplier's part code, trimmed and upper-cased so that equal codes compare equal.
 * @param supplierId
 *   Supplier selling the material under this code.
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
      MaterialInfo.normalizedCode(code).refineValidatedNec[MpnCode],
      Validated.validNec(supplierId),
      Validated.validNec(MaterialInfo.normalized(description)),
    ).mapN(Mpn.apply)

/**
 * Master data of a warehouse material.
 *
 * @param id
 *   Stable material identifier.
 * @param internalCode
 *   Company code of the material, trimmed and upper-cased.
 * @param description
 *   What the material is.
 * @param materialType
 *   Optional free-form category ("tipologia").
 * @param notes
 *   Optional free-form notes.
 * @param unit
 *   Unit in which the material is stocked and priced.
 * @param customerPrice
 *   Current price per unit charged to customers.
 * @param mpns
 *   Part numbers under which suppliers sell the material; at most one per (supplier, code), checked by [[MaterialInfo.checkInvariants]].
 */
final case class MaterialInfo(
    id: MaterialId,
    internalCode: String :| MaterialCode,
    description: String :| MaterialDescription,
    materialType: Option[String],
    notes: Option[String],
    unit: UnitOfMeasure,
    customerPrice: UnitPrice,
    mpns: List[Mpn],
) derives CanEqual:
  /** Suppliers of the material, derived from its MPNs, in first-appearance order. */
  def suppliers: List[SupplierId] = mpns.map(_.supplierId).distinct

  /** Rules every material must satisfy however it was built: no (supplier, MPN code) pair appears twice. */
  def checkInvariants: ValidatedNec[MaterialError, MaterialInfo] =
    val keys = mpns.map(_.key)
    keys.diff(keys.distinct).distinct.traverse_((supplierId, code) => MaterialError.DuplicateMpn(supplierId, code).invalidNec[Unit]).as(this)
end MaterialInfo

object MaterialInfo:
  /** Creates a `MaterialInfo` from raw values, normalizing the internal code (trimmed, upper case) and dropping blank optional values. */
  def createMaterialInfo(
      id: MaterialId,
      internalCode: String,
      description: String,
      materialType: Option[String],
      notes: Option[String],
      unit: UnitOfMeasure,
      customerPrice: UnitPrice,
      mpns: List[Mpn],
  ): ValidatedNec[String, MaterialInfo] =
    (
      Validated.validNec(id),
      normalizedCode(internalCode).refineValidatedNec[MaterialCode],
      description.trim.nn.refineValidatedNec[MaterialDescription],
      Validated.validNec(normalized(materialType)),
      Validated.validNec(normalized(notes)),
      Validated.validNec(unit),
      Validated.validNec(customerPrice),
      Validated.validNec(mpns),
    ).mapN(MaterialInfo.apply)

  /** A code as stored: no surrounding blanks, upper case. */
  def normalizedCode(raw: String): String = raw.trim.nn.toUpperCase(Locale.ROOT).nn

  private[material] def normalized(value: Option[String]): Option[String] = value.map(_.trim.nn).filter(_.nonEmpty)
end MaterialInfo
