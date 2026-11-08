package io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, UnitOfMeasure, UnitPrice, Valuation }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ DeliveryNoteRef, MaterialId, MpnCode }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

/** Refined constraint for a non-empty delivery note number. */
type DeliveryNoteNumber = DescribedAs[Not[Empty], "Il numero del DDT non può essere vuoto"]

/** Refined constraint for the non-empty reason a delivery note is cancelled. */
type CancellationReason = DescribedAs[Not[Empty], "Il motivo dell'annullamento non può essere vuoto"]

/**
 * One line of a delivery note: `quantity` of material `materialId`, received under the MPN `mpnCode` of the note's supplier at `unitCost` per unit.
 *
 * @param unit
 *   Unit of the material when the line was written, kept so that the note reads the same after the material changes.
 */
final case class DeliveryNoteLine(
    materialId: MaterialId,
    mpnCode: String :| MpnCode,
    quantity: Quantity,
    unit: UnitOfMeasure,
    unitCost: UnitPrice,
) derives CanEqual:
  /** Quantity × unit cost, rounded HALF_UP to the cent, as the material records it. */
  def total: Amount = Valuation.amount(quantity, unitCost)

/**
 * A delivery note (DDT, documento di trasporto): the goods a supplier delivered on a day, one line per material and MPN. The same material may appear
 * on several lines.
 *
 * @param id
 *   Stable delivery note identifier.
 * @param number
 *   Number the supplier gave the note, trimmed.
 * @param date
 *   Business day of the note: the purchases it brings are dated on it.
 * @param supplierId
 *   Supplier that delivered the goods: every line is bought under one of its MPNs.
 * @param note
 *   Optional free-form note.
 * @param lines
 *   What was delivered; at least one line, each with a positive quantity, checked by [[DeliveryNoteInfo.checkInvariants]].
 */
final case class DeliveryNoteInfo(
    id: DeliveryNoteId,
    number: String :| DeliveryNoteNumber,
    date: LocalDate,
    supplierId: SupplierId,
    note: Option[String],
    lines: List[DeliveryNoteLine],
) derives CanEqual:
  /** How the purchases of the materials name the note. */
  def ref: DeliveryNoteRef = DeliveryNoteRef(id, number)

  /** The value of the note: the sum of its line totals. */
  def total: Amount = Amount.sum(lines.map(_.total))

  /** Rules every delivery note must satisfy however it was built: at least one line, and no line with a zero quantity. */
  def checkInvariants: ValidatedNec[DeliveryNoteError, DeliveryNoteInfo] =
    (
      Validated.condNec(lines.nonEmpty, (), DeliveryNoteError.NoLines),
      Validated.condNec(lines.forall(_.quantity.value > 0L), (), DeliveryNoteError.NonPositiveQuantity),
    ).tupled.as(this)
end DeliveryNoteInfo

object DeliveryNoteInfo:
  /** Creates a `DeliveryNoteInfo` from raw values: the number is trimmed and a blank note becomes absent. */
  def createDeliveryNoteInfo(
      id: DeliveryNoteId,
      number: String,
      date: LocalDate,
      supplierId: SupplierId,
      note: Option[String],
      lines: List[DeliveryNoteLine],
  ): ValidatedNec[String, DeliveryNoteInfo] =
    (
      Validated.validNec(id),
      number.trim.nn.refineValidatedNec[DeliveryNoteNumber],
      Validated.validNec(date),
      Validated.validNec(supplierId),
      Validated.validNec(note.map(_.trim.nn).filter(_.nonEmpty)),
      Validated.validNec(lines),
    ).mapN(DeliveryNoteInfo.apply)
end DeliveryNoteInfo
