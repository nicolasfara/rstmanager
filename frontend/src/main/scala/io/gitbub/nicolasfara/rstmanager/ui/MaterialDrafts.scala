package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ MaterialDto, MaterialResponse, ProductResponse }
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MovementForm

/**
 * Pure (DOM-free) state of the material lines of an order manufacturing ("Materiali"): draft rows ↔ request DTOs, Italian decimal parsing, validation
 * messages, and the cost preview.
 *
 * Price freezing mirrors the server: a line without a typed price keeps the price already frozen for its product in the manufacturing, or else (a new
 * product) is sent without `unitPrice`, so the server freezes the product's current customer price, which the preview shows meanwhile. A typed price
 * is sent as is and becomes the frozen one. Line costs are rounded HALF_UP to the cent and summed, like the server's `materialsCost`.
 */
object MaterialDrafts:

  /** Decimals of a quantity on the API (thousandths of a unit). */
  val quantityScale: Int = MovementForm.quantityScale

  /** Decimals of a unit price on the API (ten-thousandths of a euro). */
  val priceScale: Int = MovementForm.costScale

  /**
   * One editable material line: `key` keeps the row identity across edits, `productId` the selected product (`""` = none yet), `quantity` and
   * `unitPrice` the typed text (a blank price = not overridden).
   */
  final case class Row(key: Int, productId: String, quantity: String, unitPrice: String) derives CanEqual

  object Row:
    def empty(key: Int): Row = Row(key, "", "", "")

  /**
   * A product a line may name: an active warehouse product (with its current `customerPrice`), or one only known from the lines already in the
   * manufacturing (archived or no longer in the warehouse: no current price, it keeps the frozen one).
   */
  final case class Product(id: UUID, internalCode: String, description: String, unit: String, customerPrice: Option[String], archived: Boolean)
      derives CanEqual:
    /** Label in the product select: `CODE — Description`, with `(archiviato)` for an archived product. */
    def label: String =
      val base = if description.isEmpty then internalCode else s"$internalCode — $description"
      if archived then s"$base (archiviato)" else base

  /** What the lines are checked against: the selectable products and the unit prices already frozen in the manufacturing, by product. */
  final case class Context(products: List[Product], frozen: Map[UUID, String]):
    private val byId: Map[UUID, Product] = products.map(product => product.id -> product).toMap

    def product(id: UUID): Option[Product] = byId.get(id)

    /** `(id, label)` pairs for the product select, in display order. */
    def options: List[(String, String)] = products.map(product => product.id.toString -> product.label)

  object Context:
    /** Nothing to pick from (products not loaded yet). */
    val empty: Context = Context(Nil, Map.empty)

    /**
     * The context of a manufacturing whose lines are `current` (none for a new manufacturing): the active `products` sorted by internal code, then
     * the products of the current lines that are not among them, and the current frozen prices.
     */
    def of(products: List[ProductResponse], current: List[MaterialResponse]): Context =
      val active = products
        .filterNot(_.archived)
        .sortBy(_.internalCode)
        .map(p => Product(p.id, p.internalCode, p.description, p.unit, Some(p.customerPrice), false))
      val activeIds = active.map(_.id).toSet
      val kept = current.filterNot(line => activeIds.contains(line.productId)).map { line =>
        Product(
          line.productId,
          line.internalCode.getOrElse(Formats.shortId(line.productId)),
          line.description.getOrElse("Prodotto non più in magazzino"),
          line.unit.getOrElse(""),
          None,
          line.archived,
        )
      }
      Context(active ++ kept, current.map(line => line.productId -> line.unitPrice).toMap)
  end Context

  /** The editable rows of the lines already in a manufacturing: quantities as Italian input text, no price override (the frozen one is kept). */
  def rowsOf(current: List[MaterialResponse], newKey: () => Int): List[Row] =
    current.map(line => Row(newKey(), line.productId.toString, WarehouseFormats.decimalInputText(line.quantity, 0, quantityScale), ""))

  /** The product selected in `row`, when the context knows it. */
  def productOf(row: Row, context: Context): Option[Product] = parseUuid(row.productId).flatMap(context.product)

  /** A row nothing was entered in: it is ignored (neither validated nor sent). */
  def isBlank(row: Row): Boolean = row.productId.trim.nn.isEmpty && row.quantity.trim.nn.isEmpty && row.unitPrice.trim.nn.isEmpty

  // ---- Prices ------------------------------------------------------------------------------------

  /** Where the unit price of a line comes from. */
  enum PriceSource derives CanEqual:
    /** Typed in the row: sent to the server. */
    case Manual

    /** Already frozen in the manufacturing for the product: kept (not sent). */
    case Frozen

    /** The product's current customer price: frozen by the server on save (not sent). */
    case Catalog

  /** The unit price a line will have once saved (canonical, 4 decimals) and where it comes from. */
  final case class PricePreview(unitPrice: String, source: PriceSource) derives CanEqual

  /** The price of `row` once saved: the typed one, else the frozen one of its product, else the product's customer price; `None` when unreadable. */
  def pricePreview(row: Row, context: Context): Option[PricePreview] =
    if row.unitPrice.trim.nn.nonEmpty then WarehouseFormats.parseDecimal(row.unitPrice, priceScale).map(PricePreview(_, PriceSource.Manual))
    else
      parseUuid(row.productId).flatMap { id =>
        context.frozen
          .get(id)
          .map(PricePreview(_, PriceSource.Frozen))
          .orElse(context.product(id).flatMap(_.customerPrice).map(PricePreview(_, PriceSource.Catalog)))
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

  private def productKeyOf(row: Row): String = row.productId.trim.nn

  /** True for a unit counted in whole pieces. */
  def isPieces(unit: String): Boolean = MeasureUnit.fromWire(unit).exists(!_.allowsFractions)

  private def quantityOf(text: String, product: Option[Product]): Either[String, String] =
    if text.trim.nn.isEmpty then Left("indica la quantità")
    else
      WarehouseFormats.parseDecimal(text, quantityScale) match
        case None => Left("quantità non valida (es. 2,5, al massimo 3 decimali)")
        case Some(quantity) if BigDecimal(quantity).signum <= 0 => Left("la quantità deve essere maggiore di zero")
        case Some(quantity) if product.exists(p => isPieces(p.unit)) && !MovementForm.isIntegral(quantity) =>
          Left(s"${product.fold("il prodotto")(_.internalCode)} è gestito a pezzi: la quantità deve essere un numero intero")
        case Some(quantity) => Right(quantity)

  private def priceOf(text: String): Either[String, Option[String]] =
    if text.trim.nn.isEmpty then Right(None)
    else WarehouseFormats.parseDecimal(text, priceScale).map(Some(_)).toRight("prezzo unitario non valido (es. 3,50, al massimo 4 decimali)")

  /** One non-blank row as a request line, or its messages (without the row prefix). */
  private def resolve(row: Row, context: Context): Either[List[String], MaterialDto] =
    val product: Either[String, Product] =
      if row.productId.trim.nn.isEmpty then Left("seleziona un prodotto")
      else parseUuid(row.productId).flatMap(context.product).toRight("il prodotto selezionato non è più disponibile")
    val quantity = quantityOf(row.quantity, product.toOption)
    val price = priceOf(row.unitPrice)
    (product, quantity, price) match
      case (Right(p), Right(q), Right(up)) => Right(MaterialDto(p.id, q, up))
      case _ => Left(product.left.toOption.toList ++ quantity.left.toOption ++ price.left.toOption)

  /**
   * The request lines of `rows` (blank rows skipped, order kept), or every validation message prefixed with its 1-based row number: missing or
   * unavailable product, a product already in an earlier row, missing/invalid/zero quantity, decimals for a product counted in pieces, invalid price.
   */
  def request(rows: List[Row], context: Context): Either[List[String], List[MaterialDto]] =
    val checked = rows.zipWithIndex.filterNot((row, _) => isBlank(row)).map { (row, index) =>
      val productKey = productKeyOf(row)
      val repeated = productKey.nonEmpty && rows.take(index).exists(earlier => productKeyOf(earlier) == productKey)
      val duplicate = Option.when(repeated) {
        val code = parseUuid(row.productId).flatMap(context.product).fold("il prodotto")(_.internalCode)
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
