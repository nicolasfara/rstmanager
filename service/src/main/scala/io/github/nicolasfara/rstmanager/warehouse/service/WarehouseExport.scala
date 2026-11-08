package io.github.nicolasfara.rstmanager.warehouse.service

import java.io.ByteArrayOutputStream
import java.time.Instant

import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MovementKind, StockMovement, StockSnapshot }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier
import io.github.nicolasfara.rstmanager.warehouse.service.WarehouseHttpSupport.OrderLabel
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

import cats.effect.{ IO, Resource }
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import org.dhatim.fastexcel.{ Workbook as XlsxWorkbook, Worksheet }

/**
 * The Excel export of the warehouse at the end of a day (`GET /warehouse/export.xlsx`).
 *
 * The workbook is first built as a pure model — sheets of typed cells, from the [[WarehouseReports.stateAt]] state and the movements up to the day —
 * then written by [[write]]. Numbers are real numeric cells with a number format (the spreadsheet application shows them with the separators of its
 * locale), days are date cells; texts are in Italian. Sheets:
 *   - `Magazzino`: one row per material of the state, plus a totals row;
 *   - `Movimenti`: every movement not voided dated on or before the day, in chronological order (automatic consumptions name their order);
 *   - `Info`: the day, when the file was generated, the totals, and how stock is valued.
 */
object WarehouseExport:
  import BusinessCalendar.given
  import WarehouseHttpSupport.{ displayDay, formatDay, unitShortLabel }

  /** Media type of an Excel workbook (`.xlsx`). */
  val mediaType: String = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

  /** Excel number format of a numeric cell; the separators shown follow the locale of the spreadsheet application. */
  enum NumberFormat(val code: String) derives CanEqual:
    /** Whole pieces. */
    case Pieces extends NumberFormat("0")

    /** Quantities in a fractional unit (metres, square metres, kilograms): 3 decimals. */
    case Quantity extends NumberFormat("0.000")

    /** Euro amounts: 2 decimals, thousands grouped. */
    case Euro extends NumberFormat("#,##0.00 \"€\"")

    /** Euro prices per unit: 4 decimals, thousands grouped. */
    case UnitCost extends NumberFormat("#,##0.0000 \"€\"")

    /** Counts. */
    case Count extends NumberFormat("0")
  end NumberFormat

  /** Format of the date cells. */
  val dayFormat: String = "dd/mm/yyyy"

  /** A cell of the model. */
  enum Cell derives CanEqual:
    case Text(value: String)
    case Number(value: BigDecimal, format: NumberFormat)
    case Day(value: LocalDate)
    case Blank

  /** A column: its header and its width, in characters. */
  final case class Column(header: String, width: Double) derives CanEqual

  /**
   * A sheet: a bold, frozen header row naming `columns`, then `rows`, then (when present) a bold `totals` row. Every row has one cell per column.
   */
  final case class Sheet(name: String, columns: List[Column], rows: List[List[Cell]], totals: Option[List[Cell]]) derives CanEqual

  final case class Workbook(sheets: List[Sheet]) derives CanEqual

  /**
   * A movement of the export, with the unit cost and the signed value it is shown with (as in the movement list: incoming movements at the cost they
   * record, outgoing ones and consumption reversals at the highest cost of their day).
   */
  final case class ExportedMovement(material: ReportedMaterial, movement: StockMovement, unitCost: Option[UnitPrice], value: Option[Amount])
      derives CanEqual:
    /** The supplier of a purchase. */
    def supplierId: Option[SupplierId] = movement.kind match
      case MovementKind.Purchase(_, supplierId, _, _, _) => Some(supplierId)
      case MovementKind.Unload(_) | MovementKind.Adjustment(_, _) | MovementKind.Consumption(_) => None

  /** The name of the downloaded file: `magazzino-YYYY-MM-DD.xlsx`. */
  def fileName(day: LocalDate): String = s"magazzino-${formatDay(day)}.xlsx"

  /** Every movement not voided dated on or before `day`, of every material, by day, then material code, then recording order. */
  def movementsUntil(materials: List[ReportedMaterial], day: LocalDate): List[ExportedMovement] =
    materials.flatMap { material =>
      val ledger = material.record.ledger
      ledger.active.toList
        .filterNot(_.on.isAfter(day))
        .map { movement =>
          ExportedMovement(material, movement, ledger.unitCostOf(movement), ledger.valueOf(movement))
        }
    }
      .sortBy(exported => (exported.movement.on, exported.material.record.info.internalCode: String, exported.movement.sequence))

  /**
   * The whole workbook of `state`, with `movements` (see [[movementsUntil]]), naming suppliers from `suppliers` and the orders of consumptions from
   * `orders`, generated at `generatedAt`.
   */
  def workbook(
      state: WarehouseState,
      movements: List[ExportedMovement],
      suppliers: Map[SupplierId, StoredSupplier],
      orders: Map[OrderId, OrderLabel],
      generatedAt: Instant,
  ): Workbook =
    Workbook(List(stockSheet(state, suppliers), movementsSheet(movements, suppliers, orders), infoSheet(state, movements.size, generatedAt)))

  // ---- Magazzino ---------------------------------------------------------------------------------

  val stockColumns: List[Column] = List(
    Column("Codice", 16),
    Column("Descrizione", 40),
    Column("Tipologia", 16),
    Column("UdM", 6),
    Column("Quantità", 12),
    Column("Costo unitario max", 18),
    Column("Valore", 16),
    Column("Costo totale acquisti", 20),
    Column("Prezzo cliente", 16),
    Column("Fornitori", 30),
    Column("MPN", 34),
    Column("Note", 30),
    Column("Anomalia", 20),
    Column("Stato", 12),
  )

  def stockSheet(state: WarehouseState, suppliers: Map[SupplierId, StoredSupplier]): Sheet =
    Sheet("Magazzino", stockColumns, rows(state, suppliers), Some(totalsRow(state.totals)))

  /** One row per material of `state`, in the order of the state (by internal code). */
  def rows(state: WarehouseState, suppliers: Map[SupplierId, StoredSupplier]): List[List[Cell]] =
    state.rows.map(stockRow(_, suppliers))

  def stockRow(row: StateRow, suppliers: Map[SupplierId, StoredSupplier]): List[Cell] =
    val info = row.material.record.info
    List(
      Cell.Text(info.internalCode),
      Cell.Text(info.description),
      text(info.materialType.getOrElse("")),
      Cell.Text(unitShortLabel(info.unit)),
      quantityCell(row.stock.quantity, info.unit),
      row.stock.maxUnitCost.fold(Cell.Blank)(unitCostCell),
      row.stock.value.fold(Cell.Blank)(euroCell),
      euroCell(row.stock.purchasedTotal),
      row.customerPrice.fold(Cell.Blank)(unitCostCell),
      text(info.suppliers.map(supplierName(_, suppliers)).mkString("; ")),
      text(info.mpns.map(mpn => s"${mpn.code} (${supplierName(mpn.supplierId, suppliers)})").mkString("; ")),
      text(info.notes.getOrElse("")),
      text(anomalies(row.stock).mkString("; ")),
      Cell.Text(if row.archived then "Archiviato" else "Attivo"),
    )
  end stockRow

  /** The anomalies of a stock: a negative balance, stock without any cost to value it. */
  def anomalies(stock: StockSnapshot): List[String] =
    List(
      Option.when(stock.negative)("Giacenza negativa"),
      Option.when(stock.value.isEmpty && stock.quantity.value != 0L)("Non valorizzato"),
    ).flatten

  def totalsRow(totals: StateTotals): List[Cell] =
    val flagged = List(
      Option.when(totals.negativeMaterials > 0)(s"${totals.negativeMaterials} con giacenza negativa"),
      Option.when(totals.unvaluedMaterials > 0)(s"${totals.unvaluedMaterials} non valorizzati"),
    ).flatten
    List(
      Cell.Text("Totale"),
      Cell.Text(if totals.materials == 1 then "1 materiale" else s"${totals.materials} materiali"),
      Cell.Blank,
      Cell.Blank,
      Cell.Blank,
      Cell.Blank,
      euroCell(totals.value),
      euroCell(totals.purchasedTotal),
      Cell.Blank,
      Cell.Blank,
      Cell.Blank,
      Cell.Blank,
      text(flagged.mkString("; ")),
      Cell.Blank,
    )
  end totalsRow

  // ---- Movimenti ---------------------------------------------------------------------------------

  val movementColumns: List[Column] = List(
    Column("Data", 12),
    Column("Codice", 16),
    Column("Descrizione", 40),
    Column("UdM", 6),
    Column("Tipo", 20),
    Column("Fornitore", 26),
    Column("MPN", 20),
    Column("Quantità", 12),
    Column("Costo unitario", 16),
    Column("Valore", 16),
    Column("Causale/Nota", 40),
  )

  def movementsSheet(movements: List[ExportedMovement], suppliers: Map[SupplierId, StoredSupplier], orders: Map[OrderId, OrderLabel]): Sheet =
    Sheet("Movimenti", movementColumns, movements.map(movementRow(_, suppliers, orders)), None)

  def movementRow(exported: ExportedMovement, suppliers: Map[SupplierId, StoredSupplier], orders: Map[OrderId, OrderLabel]): List[Cell] =
    val info = exported.material.record.info
    val movement = exported.movement
    List(
      Cell.Day(movement.on),
      Cell.Text(info.internalCode),
      Cell.Text(info.description),
      Cell.Text(unitShortLabel(info.unit)),
      Cell.Text(kindLabel(movement)),
      exported.supplierId.fold(Cell.Blank)(id => Cell.Text(supplierName(id, suppliers))),
      mpnCell(movement.kind),
      quantityCell(movement.delta, info.unit),
      exported.unitCost.fold(Cell.Blank)(unitCostCell),
      exported.value.fold(Cell.Blank)(euroCell),
      text(causeOf(movement.kind, orders)),
    )
  end movementRow

  /** How the export names a movement: by its kind, telling an automatic consumption from its reversal. */
  def kindLabel(movement: StockMovement): String = movement.kind match
    case MovementKind.Purchase(_, _, _, _, _) => "Carico"
    case MovementKind.Unload(_) => "Scarico"
    case MovementKind.Adjustment(_, _) => "Rettifica"
    case MovementKind.Consumption(_) => if movement.incoming then "Storno lavorazione" else "Scarico lavorazione"

  private def mpnCell(kind: MovementKind): Cell = kind match
    case MovementKind.Purchase(code, _, _, _, _) => Cell.Text(code)
    case MovementKind.Unload(_) | MovementKind.Adjustment(_, _) | MovementKind.Consumption(_) => Cell.Blank

  /**
   * The note of a purchase, the reason of a manual movement (empty for a purchase without note), or the order and manufacturing of a consumption
   * (`Ordine 2026-014 – lavorazione TND-01`; the order id when the order cannot be loaded).
   */
  private def causeOf(kind: MovementKind, orders: Map[OrderId, OrderLabel]): String = kind match
    case MovementKind.Purchase(_, _, _, _, note) => note.getOrElse("")
    case MovementKind.Unload(reason) => reason
    case MovementKind.Adjustment(_, reason) => reason
    case MovementKind.Consumption(ref) =>
      orders.get(ref.orderId) match
        case Some(label) =>
          s"Ordine ${label.number}" + label.manufacturingCodes.get(ref.manufacturingId).fold("")(code => s" – lavorazione $code")
        case None => s"Ordine ${ref.orderId.value}"

  // ---- Info --------------------------------------------------------------------------------------

  def infoSheet(state: WarehouseState, movements: Int, generatedAt: Instant): Sheet =
    val generated = new DateTime(generatedAt.toEpochMilli, BusinessCalendar.zone).toString("dd/MM/yyyy HH:mm")
    Sheet(
      "Info",
      List(Column("Voce", 34), Column("Valore", 70)),
      List(
        List(Cell.Text("Stato magazzino al"), Cell.Day(state.at)),
        List(Cell.Text("Generato il"), Cell.Text(s"$generated (ora italiana)")),
        List(Cell.Text("Materiali"), countCell(state.totals.materials)),
        List(Cell.Text("Valore magazzino"), euroCell(state.totals.value)),
        List(Cell.Text("Costo totale acquisti"), euroCell(state.totals.purchasedTotal)),
        List(Cell.Text("Materiali con giacenza negativa"), countCell(state.totals.negativeMaterials)),
        List(Cell.Text("Materiali non valorizzati"), countCell(state.totals.unvaluedMaterials)),
        List(Cell.Text("Movimenti fino al giorno"), countCell(movements)),
        List(
          Cell.Text("Valorizzazione"),
          Cell.Text(s"Quantità × costo unitario d'acquisto più alto registrato fino al ${displayDay(state.at)}"),
        ),
        List(Cell.Text("Movimenti esclusi"), Cell.Text("I movimenti annullati non sono conteggiati")),
      ),
      None,
    )
  end infoSheet

  // ---- Cells -------------------------------------------------------------------------------------

  /** A text cell, or a blank one for an empty text. */
  def text(value: String): Cell = if value.isEmpty then Cell.Blank else Cell.Text(value)

  def quantityCell(quantity: SignedQuantity, unit: UnitOfMeasure): Cell =
    Cell.Number(BigDecimal(BigInt(quantity.value), SignedQuantity.scale), if unit.allowsFractions then NumberFormat.Quantity else NumberFormat.Pieces)

  def unitCostCell(price: UnitPrice): Cell = Cell.Number(BigDecimal(BigInt(price.value), UnitPrice.scale), NumberFormat.UnitCost)

  def euroCell(amount: Amount): Cell = Cell.Number(BigDecimal(BigInt(amount.value), Amount.scale), NumberFormat.Euro)

  def countCell(count: Int): Cell = Cell.Number(BigDecimal(count), NumberFormat.Count)

  /** The name of supplier `id`, marked when deleted; a supplier missing from `suppliers` is shown by id. */
  private def supplierName(id: SupplierId, suppliers: Map[SupplierId, StoredSupplier]): String =
    suppliers.get(id).fold(id.value.toString)(stored => if stored.deleted then s"${stored.supplier.name} (eliminato)" else stored.supplier.name)

  // ---- Writer ------------------------------------------------------------------------------------

  /** Writes `book` as an `.xlsx` file. The workbook is assembled in memory; closing it writes the file. */
  def write(book: Workbook): IO[Array[Byte]] =
    IO(new ByteArrayOutputStream()).flatMap { out =>
      Resource
        .fromAutoCloseable(IO(new XlsxWorkbook(out, "rstmanager", "1.0")))
        .use(workbook => IO.blocking(book.sheets.foreach(writeSheet(workbook, _))))
        *> IO(out.toByteArray)
    }

  private def writeSheet(workbook: XlsxWorkbook, sheet: Sheet): Unit =
    val worksheet = workbook.newWorksheet(sheet.name)
    sheet.columns.zipWithIndex.foreach((column, index) => worksheet.width(index, column.width))
    writeRow(worksheet, 0, sheet.columns.map(column => Cell.Text(column.header)), bold = true)
    worksheet.freezePane(0, 1)
    sheet.rows.zipWithIndex.foreach((cells, index) => writeRow(worksheet, index + 1, cells, bold = false))
    sheet.totals.foreach(cells => writeRow(worksheet, sheet.rows.size + 1, cells, bold = true))

  /** Writes `cells` on row `row`, setting each cell's style once (number format and weight together). */
  private def writeRow(worksheet: Worksheet, row: Int, cells: List[Cell], bold: Boolean): Unit =
    cells.zipWithIndex.foreach { (cell, column) =>
      val format = cell match
        case Cell.Text(value) =>
          worksheet.value(row, column, value)
          None
        case Cell.Number(value, numberFormat) =>
          worksheet.value(row, column, value.bigDecimal)
          Some(numberFormat.code)
        case Cell.Day(value) =>
          worksheet.value(row, column, java.time.LocalDate.of(value.getYear, value.getMonthOfYear, value.getDayOfMonth))
          Some(dayFormat)
        case Cell.Blank => None
      if bold || format.isDefined then
        val style = worksheet.style(row, column)
        val formatted = format.fold(style)(style.format)
        (if bold then formatted.bold() else formatted).set()
    }
end WarehouseExport
