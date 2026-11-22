package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.WarehouseViews.MaterialSummary

/**
 * The shared rendering of a material in the materials ("Materiali") and history ("Storico") tabs, from a [[MaterialSummary]]: the stacked card of the
 * narrow layouts and the cells of the compact table (from `xl`), so the two tabs show a material the same way.
 */
object MaterialCards:

  private val archivedBadgeCls = "bg-slate-100 text-slate-500 border-slate-200"

  private def anomalyBadge: HtmlElement =
    span(title := "Giacenza negativa: verificare i movimenti", badge("Anomalia", "bg-rose-50 text-rose-700 border-rose-200"))

  /** The stock quantity with its unit, red when negative. */
  def stockText(m: MaterialSummary): HtmlElement =
    span(
      cls := "whitespace-nowrap tabular-nums",
      cls := (if m.stock.negative then "font-medium text-rose-700" else ""),
      WarehouseFormats.formatQty(m.stock.quantity, m.unit),
    )

  /** The stock value (its tooltip tells the highest unit cost), or a grey dash while no purchase cost is known. */
  def valueText(m: MaterialSummary): HtmlElement =
    m.stock.value.fold(span(cls := "text-slate-400", title := "Nessun costo d'acquisto registrato", "—")) { value =>
      span(
        cls := "whitespace-nowrap tabular-nums",
        m.stock.maxUnitCost.map(cost => title := s"Costo unitario max: ${WarehouseFormats.formatUnitPrice(cost)}"),
        WarehouseFormats.formatEuro(value),
      )
    }

  /** The highest purchase unit cost so far, or a dash. */
  def maxCostText(m: MaterialSummary): HtmlElement =
    span(cls := "whitespace-nowrap tabular-nums", m.stock.maxUnitCost.fold("—")(WarehouseFormats.formatUnitPrice))

  /** The customer price, or a dash. */
  def priceText(m: MaterialSummary): HtmlElement =
    span(cls := "whitespace-nowrap tabular-nums", m.customerPrice.fold("—")(WarehouseFormats.formatUnitPrice))

  /** The supplier names, a deleted supplier struck through. */
  private def supplierNames(m: MaterialSummary): HtmlElement =
    if m.suppliers.isEmpty then div(cls := "text-slate-400", "—")
    else
      div(m.suppliers.map { s =>
        if s.active then div(cls := "wrap-anywhere", s.name)
        else div(cls := "wrap-anywhere text-slate-400 line-through", title := "Fornitore eliminato", s.name)
      })

  private def mpnLine(m: MaterialSummary, lineCls: String): Modifier[HtmlElement] =
    Option.when(m.mpnCodes.nonEmpty) {
      val codes = m.mpnCodes.mkString(", ")
      div(cls := s"text-xs text-slate-400 $lineCls", title := codes, codes)
    }

  private def typeAndUnitLine(m: MaterialSummary): HtmlElement =
    div(
      cls := "wrap-anywhere text-xs text-slate-400",
      title := WarehouseFormats.unitLabel(m.unit),
      WarehouseViews.typeAndUnit(m.materialType, m.unit),
    )

  // ---- Compact table cells (xl+) -----------------------------------------------------------------

  /** "Materiale": code, description, type · unit, notes, and the archived badge. */
  def materialCell(m: MaterialSummary): HtmlElement =
    div(
      cls := "min-w-0",
      div(cls := "wrap-anywhere font-medium", cls := (if m.archived then "text-slate-500" else "text-slate-800"), m.internalCode),
      div(cls := "wrap-anywhere", cls := (if m.archived then "" else "text-slate-700"), m.description),
      typeAndUnitLine(m),
      m.notes.map(n => div(cls := "wrap-anywhere text-xs text-slate-400", n)),
      m.archivedLabel.map(text => div(cls := "mt-1", badge(text, archivedBadgeCls))),
    )

  /** "Giacenza": the quantity, with the anomaly badge when negative. */
  def stockCell(m: MaterialSummary): HtmlElement =
    div(stockText(m), Option.when(m.stock.negative)(div(cls := "mt-1", anomalyBadge)))

  /** "Valore / Prezzo": the stock value over the customer price. */
  def valuePriceCell(m: MaterialSummary): HtmlElement =
    div(div(valueText(m)), div(cls := "text-xs text-slate-500", title := "Prezzo cliente", priceText(m)))

  /** "Fornitori": the supplier names over the MPN codes. */
  def suppliersCell(m: MaterialSummary): HtmlElement =
    div(cls := "min-w-0", supplierNames(m), mpnLine(m, "wrap-anywhere"))

  // ---- Card (below xl) ---------------------------------------------------------------------------

  /**
   * The card of a material in the narrow layouts: code, description, and type · unit with the archived/anomaly badges on the right, then stock,
   * value, customer price, any `extraFields` (e.g. the highest cost in the history), and the suppliers with their MPNs on one truncated line; the
   * `actions`, when given, sit in a bar at the bottom like the order cards.
   */
  def listCard(m: MaterialSummary, extraFields: List[HtmlElement])(actions: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "space-y-3 p-4",
      cls := (if m.archived then "bg-slate-50" else ""),
      div(
        cls := "flex items-start justify-between gap-3",
        div(
          cls := "min-w-0",
          div(cls := "wrap-anywhere font-medium", cls := (if m.archived then "text-slate-500" else "text-slate-800"), m.internalCode),
          div(cls := "mt-0.5 wrap-anywhere text-sm text-slate-600", m.description),
          typeAndUnitLine(m),
          m.notes.map(n => div(cls := "mt-0.5 wrap-anywhere text-xs text-slate-400", n)),
        ),
        Option.when(m.archived || m.stock.negative)(
          div(
            cls := "flex shrink-0 flex-col items-end gap-1",
            m.archivedLabel.map(text => badge(text, archivedBadgeCls)),
            Option.when(m.stock.negative)(anomalyBadge),
          ),
        ),
      ),
      div(
        cls := "grid grid-cols-2 gap-3",
        readField("Giacenza", stockText(m)),
        readField("Valore", valueText(m)),
        readField("Prezzo cliente", priceText(m)),
        extraFields,
        div(cls := "col-span-2 min-w-0", readField("Fornitori", div(supplierNames(m), mpnLine(m, "truncate")))),
      ),
      Option.when(actions.nonEmpty)(div(cls := cardActionsCls, actions)),
    )
end MaterialCards
