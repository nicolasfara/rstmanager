package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ MaterialRequest, MaterialResponse, MpnDto }
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit

/**
 * Pure (DOM-free) state of the material create/edit form: request building, validation messages, and the MPN rows editor.
 *
 * The page keeps one [[State]] in a `Var` and only applies the transformations below, so every rule is unit-testable without Laminar.
 */
object MaterialForm:

  /** Decimals of a customer price on the API (ten-thousandths of a euro). */
  val priceScale: Int = 4

  /**
   * One row of the MPN editor. `key` is stable across edits so Laminar keeps the row's inputs (and focus) while typing; `expectedUnitCost` is the
   * typed expected purchase price (blank = unknown), prefilled as the unit cost of the delivery note lines bought under this MPN.
   */
  final case class MpnRow(key: Int, supplierId: String, code: String, description: String, expectedUnitCost: String) derives CanEqual:
    /** A row the user added but never filled in: ignored instead of being reported. */
    def isBlank: Boolean = supplierId.trim.nn.isEmpty && code.trim.nn.isEmpty && description.trim.nn.isEmpty && expectedUnitCost.trim.nn.isEmpty

    /** The expected price as the API expects it (4 decimals): `Right(None)` when blank, `Left(())` when unreadable. */
    def parsedExpectedCost: Either[Unit, Option[String]] =
      if expectedUnitCost.trim.nn.isEmpty then Right(None) else WarehouseFormats.parseDecimal(expectedUnitCost, priceScale).map(Some(_)).toRight(())

  final case class State(
      editingId: Option[UUID],
      internalCode: String,
      description: String,
      materialType: String,
      notes: String,
      unit: String,
      customerPrice: String,
      mpns: List[MpnRow],
      nextKey: Int,
  ):
    /** True until the user types into a required field or adds an MPN row: validation messages stay hidden on a fresh form. */
    def pristine: Boolean =
      editingId.isEmpty && internalCode.trim.nn.isEmpty && description.trim.nn.isEmpty && customerPrice.trim.nn.isEmpty && mpns.isEmpty

    def parsedPrice: Option[String] = WarehouseFormats.parseDecimal(customerPrice, priceScale)

    /** The MPN rows that count, with their 1-based position in the editor (blank rows dropped). */
    private def filledRows: List[(MpnRow, Int)] = mpns.zipWithIndex.collect { case (row, index) if !row.isBlank => row -> (index + 1) }

    def errors: List[String] =
      val fieldErrors = List(
        Option.when(internalCode.trim.nn.isEmpty)("Codice obbligatorio"),
        Option.when(description.trim.nn.isEmpty)("Descrizione obbligatoria"),
        Option.when(MeasureUnit.fromWire(unit).isEmpty)("Unità di misura non valida"),
        if customerPrice.trim.nn.isEmpty then Some("Prezzo cliente obbligatorio")
        else parsedPrice.fold(Some("Prezzo non valido (es. 18,50, al massimo 4 decimali)"))(_ => None),
      ).flatten
      val rowErrors = filledRows.flatMap { (row, n) =>
        List(
          Option.when(parseUuid(row.supplierId).isEmpty)(s"Fornitore mancante nella riga MPN $n"),
          Option.when(row.code.trim.nn.isEmpty)(s"Codice mancante nella riga MPN $n"),
          Option.when(row.parsedExpectedCost.isLeft)(s"Prezzo acquisto previsto non valido nella riga MPN $n (es. 3,50, al massimo 4 decimali)"),
        ).flatten
      }
      val duplicateErrors = filledRows
        .filter((row, _) => row.code.trim.nn.nonEmpty && parseUuid(row.supplierId).isDefined)
        .groupBy((row, _) => (row.supplierId.trim.nn, normalizedCode(row.code)))
        .values
        .filter(_.size > 1)
        .map(group => group.map(_._2).sorted)
        .toList
        .sortBy(_.headOption)
        .map(positions => s"MPN ripetuto per lo stesso fornitore nelle righe ${positions.mkString(", ")}")
      fieldErrors ++ rowErrors ++ duplicateErrors
    end errors

    /** The payload to send, or `None` while [[errors]] is not empty. */
    def request: Option[MaterialRequest] =
      for
        price <- parsedPrice
        if errors.isEmpty
      yield MaterialRequest(
        internalCode.trim.nn,
        description.trim.nn,
        SupplierForm.opt(materialType),
        SupplierForm.opt(notes),
        unit,
        price,
        filledRows.flatMap((row, _) =>
          parseUuid(row.supplierId).map(id =>
            MpnDto(row.code.trim.nn, id, SupplierForm.opt(row.description), row.parsedExpectedCost.toOption.flatten),
          ),
        ),
      )

    def addRow: State = copy(mpns = mpns :+ MpnRow(nextKey, "", "", "", ""), nextKey = nextKey + 1)

    def removeRow(key: Int): State = copy(mpns = mpns.filterNot(_.key == key))

    def updateRow(key: Int)(patch: MpnRow => MpnRow): State = copy(mpns = mpns.map(row => if row.key == key then patch(row) else row))
  end State

  val empty: State = State(None, "", "", "", "", MeasureUnit.Pieces.wireName, "", Nil, 0)

  /**
   * The form prefilled with `material` for editing; the customer price and the expected purchase prices are shown the Italian way with at least the
   * cents (`"18.5000"` → `"18,50"`).
   */
  def fromResponse(material: MaterialResponse): State =
    State(
      Some(material.id),
      material.internalCode,
      material.description,
      material.materialType.getOrElse(""),
      material.notes.getOrElse(""),
      material.unit,
      WarehouseFormats.decimalInputText(material.customerPrice, 2, priceScale),
      material.mpns.zipWithIndex.map((mpn, index) =>
        MpnRow(
          index,
          mpn.supplierId.toString,
          mpn.code,
          mpn.description.getOrElse(""),
          mpn.expectedUnitCost.fold("")(WarehouseFormats.decimalInputText(_, 2, priceScale)),
        ),
      ),
      material.mpns.size,
    )

  /** The distinct material types already in use, sorted case-insensitively: suggestions for the "Tipologia" input. */
  def materialTypes(materials: List[MaterialResponse]): List[String] =
    materials
      .flatMap(_.materialType)
      .map(_.trim.nn)
      .filter(_.nonEmpty)
      .groupBy(_.toLowerCase.nn)
      .values
      .flatMap(_.headOption)
      .toList
      .sortBy(_.toLowerCase.nn)

  /** Codes are compared as the server stores them: trimmed and upper case. */
  private def normalizedCode(code: String): String = code.trim.nn.toUpperCase.nn

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value.trim).nn).toOption
end MaterialForm
