package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ MaterialLineDto, MaterialLineResponse, MaterialResponse }
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MovementForm

/**
 * Pure (DOM-free) state of the material lines of an order manufacturing ("Materiali"): draft rows ↔ request DTOs, Italian decimal parsing, validation
 * messages, and the cost preview.
 *
 * Price freezing mirrors the server: a line without a typed price keeps the price already frozen for its material in the manufacturing, or else (a
 * new material) is sent without `unitPrice`, so the server freezes the material's current customer price, which the preview shows meanwhile. A typed
 * price is sent as is and becomes the frozen one. Line costs are rounded HALF_UP to the cent and summed, like the server's `materialsCost`.
 */
object MaterialDrafts:

  /** Decimals of a quantity on the API (thousandths of a unit). */
  val quantityScale: Int = MovementForm.quantityScale

  /** Decimals of a unit price on the API (ten-thousandths of a euro). */
  val priceScale: Int = MovementForm.costScale

  /**
   * One editable material line: `key` keeps the row identity across edits, `materialId` the selected material (`""` = none yet), `quantity` and
   * `unitPrice` the typed text (a blank price = not overridden).
   */
  final case class Row(key: Int, materialId: String, quantity: String, unitPrice: String) derives CanEqual

  object Row:
    def empty(key: Int): Row = Row(key, "", "", "")

  /**
   * A material a line may name: an active warehouse material (with its current `customerPrice`), or one only known from the lines already in the
   * manufacturing (archived or no longer in the warehouse: no current price, it keeps the frozen one).
   */
  final case class Material(id: UUID, internalCode: String, description: String, unit: String, customerPrice: Option[String], archived: Boolean)
      derives CanEqual:
    /** Label in the material select: `CODE — Description`, with `(archiviato)` for an archived material. */
    def label: String =
      val base = if description.isEmpty then internalCode else s"$internalCode — $description"
      if archived then s"$base (archiviato)" else base

  /** What the lines are checked against: the selectable materials and the unit prices already frozen in the manufacturing, by material. */
  final case class Context(materials: List[Material], frozen: Map[UUID, String]):
    private val byId: Map[UUID, Material] = materials.map(material => material.id -> material).toMap

    def material(id: UUID): Option[Material] = byId.get(id)

    /** `(id, label)` pairs for the material select, in display order. */
    def options: List[(String, String)] = materials.map(material => material.id.toString -> material.label)

  object Context:
    /** Nothing to pick from (materials not loaded yet). */
    val empty: Context = Context(Nil, Map.empty)

    /**
     * The context of a manufacturing whose lines are `current` (none for a new manufacturing): the active `materials` sorted by internal code, then
     * the materials of the current lines that are not among them, and the current frozen prices.
     */
    def of(materials: List[MaterialResponse], current: List[MaterialLineResponse]): Context =
      val active = materials
        .filterNot(_.archived)
        .sortBy(_.internalCode)
        .map(p => Material(p.id, p.internalCode, p.description, p.unit, Some(p.customerPrice), false))
      val activeIds = active.map(_.id).toSet
      val kept = current.filterNot(line => activeIds.contains(line.materialId)).map { line =>
        Material(
          line.materialId,
          line.internalCode.getOrElse(Formats.shortId(line.materialId)),
          line.description.getOrElse("Materiale non più in magazzino"),
          line.unit.getOrElse(""),
          None,
          line.archived,
        )
      }
      Context(active ++ kept, current.map(line => line.materialId -> line.unitPrice).toMap)
  end Context

  /** The editable rows of the lines already in a manufacturing: quantities as Italian input text, no price override (the frozen one is kept). */
  def rowsOf(current: List[MaterialLineResponse], newKey: () => Int): List[Row] =
    current.map(line => Row(newKey(), line.materialId.toString, WarehouseFormats.decimalInputText(line.quantity, 0, quantityScale), ""))

  /** The material selected in `row`, when the context knows it. */
  def materialOf(row: Row, context: Context): Option[Material] = parseUuid(row.materialId).flatMap(context.material)

  /** A row nothing was entered in: it is ignored (neither validated nor sent). */
  def isBlank(row: Row): Boolean = row.materialId.trim.nn.isEmpty && row.quantity.trim.nn.isEmpty && row.unitPrice.trim.nn.isEmpty

  // ---- Prices ------------------------------------------------------------------------------------

  /** Where the unit price of a line comes from. */
  enum PriceSource derives CanEqual:
    /** Typed in the row: sent to the server. */
    case Manual

    /** Already frozen in the manufacturing for the material: kept (not sent). */
    case Frozen

    /** The material's current customer price: frozen by the server on save (not sent). */
    case Catalog

  /** The unit price a line will have once saved (canonical, 4 decimals) and where it comes from. */
  final case class PricePreview(unitPrice: String, source: PriceSource) derives CanEqual

  /**
   * The price of `row` once saved: the typed one, else the frozen one of its material, else the material's customer price; `None` when unreadable.
   */
  def pricePreview(row: Row, context: Context): Option[PricePreview] =
    if row.unitPrice.trim.nn.nonEmpty then WarehouseFormats.parseDecimal(row.unitPrice, priceScale).map(PricePreview(_, PriceSource.Manual))
    else
      parseUuid(row.materialId).flatMap { id =>
        context.frozen
          .get(id)
          .map(PricePreview(_, PriceSource.Frozen))
          .orElse(context.material(id).flatMap(_.customerPrice).map(PricePreview(_, PriceSource.Catalog)))
      }

  /** Italian note about the price of a line, shown under its price input. */
  def priceNote(preview: PricePreview): String = preview.source match
    case PriceSource.Manual => "Prezzo inserito"
    case PriceSource.Frozen => s"Prezzo congelato: ${WarehouseFormats.formatUnitPrice(preview.unitPrice)}"
    case PriceSource.Catalog => s"Prezzo di listino: ${WarehouseFormats.formatUnitPrice(preview.unitPrice)} (congelato al salvataggio)"

  // ---- Costs -------------------------------------------------------------------------------------

  private def cents(value: BigDecimal): String = value.setScale(2, RoundingMode.HALF_UP).bigDecimal.toPlainString.nn

  /** The cost of a line, `quantity × unit price` rounded HALF_UP to the cent (2 decimals), when both are readable. */
  def lineCost(row: Row, context: Context): Option[String] =
    for
      quantity <- WarehouseFormats.parseDecimal(row.quantity, quantityScale)
      price <- pricePreview(row, context)
    yield cents(BigDecimal(quantity) * BigDecimal(price.unitPrice))

  /** The materials cost of the readable lines: the sum of their rounded line costs (2 decimals), like the server. */
  def totalCost(rows: List[Row], context: Context): String =
    cents(rows.flatMap(lineCost(_, context)).map(BigDecimal(_)).sum)

  // ---- Validation and request --------------------------------------------------------------------

  private def parseUuid(value: String): Option[UUID] =
    Try {
      val id: UUID = UUID.fromString(value.trim).nn
      id
    }.toOption

  private def materialKeyOf(row: Row): String = row.materialId.trim.nn

  /** True for a unit counted in whole pieces. */
  def isPieces(unit: String): Boolean = MeasureUnit.fromWire(unit).exists(!_.allowsFractions)

  private def quantityOf(text: String, material: Option[Material]): Either[String, String] =
    if text.trim.nn.isEmpty then Left("indica la quantità")
    else
      WarehouseFormats.parseDecimal(text, quantityScale) match
        case None => Left("quantità non valida (es. 2,5, al massimo 3 decimali)")
        case Some(quantity) if BigDecimal(quantity).signum <= 0 => Left("la quantità deve essere maggiore di zero")
        case Some(quantity) if material.exists(p => isPieces(p.unit)) && !MovementForm.isIntegral(quantity) =>
          Left(s"${material.fold("il materiale")(_.internalCode)} è gestito a unità intere: la quantità deve essere un numero intero")
        case Some(quantity) => Right(quantity)

  private def priceOf(text: String): Either[String, Option[String]] =
    if text.trim.nn.isEmpty then Right(None)
    else WarehouseFormats.parseDecimal(text, priceScale).map(Some(_)).toRight("prezzo unitario non valido (es. 3,50, al massimo 4 decimali)")

  /** One non-blank row as a request line, or its messages (without the row prefix). */
  private def resolve(row: Row, context: Context): Either[List[String], MaterialLineDto] =
    val material: Either[String, Material] =
      if row.materialId.trim.nn.isEmpty then Left("seleziona un materiale")
      else parseUuid(row.materialId).flatMap(context.material).toRight("il materiale selezionato non è più disponibile")
    val quantity = quantityOf(row.quantity, material.toOption)
    val price = priceOf(row.unitPrice)
    (material, quantity, price) match
      case (Right(p), Right(q), Right(up)) => Right(MaterialLineDto(p.id, q, up))
      case _ => Left(material.left.toOption.toList ++ quantity.left.toOption ++ price.left.toOption)

  /**
   * The request lines of `rows` (blank rows skipped, order kept), or every validation message prefixed with its 1-based row number: missing or
   * unavailable material, a material already in an earlier row, missing/invalid/zero quantity, decimals for a material counted in pieces, invalid
   * price.
   */
  def request(rows: List[Row], context: Context): Either[List[String], List[MaterialLineDto]] =
    val checked = rows.zipWithIndex.filterNot((row, _) => isBlank(row)).map { (row, index) =>
      val materialKey = materialKeyOf(row)
      val repeated = materialKey.nonEmpty && rows.take(index).exists(earlier => materialKeyOf(earlier) == materialKey)
      val duplicate = Option.when(repeated) {
        val code = parseUuid(row.materialId).flatMap(context.material).fold("il materiale")(_.internalCode)
        s"$code è già in un'altra riga: indica la quantità totale in una sola riga"
      }
      resolve(row, context) match
        case Right(line) if duplicate.isEmpty => Right(line)
        case result => Left((result.left.getOrElse(Nil) ++ duplicate).map(message => s"Riga ${index + 1}: $message"))
    }
    val errors = checked.flatMap(_.left.getOrElse(Nil))
    if errors.isEmpty then Right(checked.flatMap(_.toOption)) else Left(errors)
  end request

  /** The validation messages of `rows` (see [[request]]); empty when they can be sent. */
  def errors(rows: List[Row], context: Context): List[String] = request(rows, context).left.getOrElse(Nil)
end MaterialDrafts
