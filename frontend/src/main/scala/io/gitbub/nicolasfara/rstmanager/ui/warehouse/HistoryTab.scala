package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/**
 * Day-by-day history of the warehouse ("Storico"): the state at the end of a chosen day — every product existing then, with its stock, highest
 * purchase cost, value, and the customer price in effect — plus the totals of that day. Days are browsed with a date picker and previous/next arrows
 * (never past today); a client-side search narrows the rows. "Esporta Excel" downloads the workbook of the day on screen (`GET
 * /warehouse/export.xlsx?at=`).
 */
object HistoryTab:

  private val navBtn = s"$btnGhost px-2.5 disabled:opacity-40"

  def apply(): HtmlElement =
    val today = WarehousePage.today()
    val selected = Var(today)
    val search = Var("")
    val dayCheck = selected.signal.map(WarehouseViews.stateDay(_, today)).distinct
    // The last valid day: an invalid selection shows a hint and keeps the state of the previous day on screen.
    val applied = Var(today)
    val state = loadable(AppBus.warehouseTicks.combineWith(applied.signal.distinct))(() => ApiClient.warehouseState(applied.now()))
    val exporting = Var(false)
    val exportError = Var(Option.empty[ApiError])

    // Exports the day whose state is on screen (`applied`), never an invalid selection.
    def exportDay(): Unit =
      if !exporting.now() then
        exporting.set(true)
        exportError.set(None)
        ApiClient.exportWarehouse(applied.now()).foreach { result =>
          exporting.set(false)
          result.left.foreach(showError(exportError, "Esportazione Excel")(_))
        }

    def exportButton: HtmlElement =
      button(
        tpe := "button",
        cls := s"$btnGhost disabled:opacity-40",
        title <-- applied.signal.map(day => s"Scarica il magazzino del ${IsoDay.display(day)} in formato Excel (.xlsx)"),
        child.text <-- exporting.signal.map(busy => if busy then "Esportazione…" else "Esporta Excel"),
        disabled <-- exporting.signal,
        onClick --> (_ => exportDay()),
      )

    def toolbar: HtmlElement =
      card(
        cls := "flex flex-wrap items-end gap-3 p-4",
        div(
          span(cls := "mb-1 block text-xs font-medium text-slate-500", "Giorno"),
          div(
            cls := "flex items-center gap-1.5",
            button(
              tpe := "button",
              cls := navBtn,
              title := "Giorno precedente",
              "◀",
              onClick --> (_ => selected.update(WarehouseViews.previousDay)),
            ),
            div(cls := "w-40", textInput(selected.signal, selected.writer, "", "date").amend(maxAttr := today)),
            button(
              tpe := "button",
              cls := navBtn,
              title := "Giorno successivo",
              "▶",
              disabled <-- selected.signal.map(WarehouseViews.nextDay(_, today).isEmpty),
              onClick --> (_ => WarehouseViews.nextDay(selected.now(), today).foreach(selected.set)),
            ),
            button(
              tpe := "button",
              cls := s"$btnGhost disabled:opacity-40",
              "Oggi",
              disabled <-- selected.signal.map(_ == today),
              onClick --> (_ => selected.set(today)),
            ),
          ),
        ),
        div(cls := "min-w-[12rem] flex-1", field("Cerca", textInput(search, "Codice, descrizione o tipologia"))),
        div(cls := "ml-auto flex gap-2", roleGated(Role.Viewer)(exportButton)),
      )

    def totalCard(label: String, value: String, valueCls: String, hintText: Option[String]): HtmlElement =
      card(
        cls := "p-4",
        hintText.map(text => title := text),
        div(cls := "text-xs font-medium text-slate-500", label),
        div(cls := s"mt-1 text-lg font-semibold tabular-nums $valueCls", value),
      )

    def totals(t: StateTotalsResponse): HtmlElement =
      div(
        cls := "grid grid-cols-2 gap-3 sm:grid-cols-3 lg:grid-cols-5",
        totalCard("N° prodotti", t.products.toString, "text-slate-800", None),
        totalCard("Valore magazzino", WarehouseFormats.formatEuro(t.value), "text-slate-800", Some("Giacenza × costo unitario d'acquisto più alto")),
        totalCard(
          "Costo totale acquisti",
          WarehouseFormats.formatEuro(t.purchasedTotal),
          "text-slate-800",
          Some("Speso in acquisti fino a questo giorno"),
        ),
        totalCard(
          "Prodotti in negativo",
          t.negativeProducts.toString,
          if t.negativeProducts > 0 then "text-rose-700" else "text-slate-800",
          Some("Prodotti con giacenza negativa: anomalia da verificare"),
        ),
        totalCard(
          "Non valorizzati",
          t.unvaluedProducts.toString,
          if t.unvaluedProducts > 0 then "text-amber-700" else "text-slate-800",
          Some("Prodotti senza alcun costo d'acquisto registrato: il loro valore non è conteggiato"),
        ),
      )

    def suppliersCell(row: StateRowResponse): HtmlElement =
      if row.suppliers.isEmpty then div(cls := "text-xs text-slate-400", "—")
      else
        div(row.suppliers.map { s =>
          if s.active then div(s.name) else div(cls := "text-slate-400 line-through", title := "Fornitore eliminato", s.name)
        })

    def renderRow(row: StateRowResponse): HtmlElement =
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(
          cls := "px-4 py-2 font-medium text-slate-800",
          row.internalCode,
          Option.when(row.archived)(div(cls := "mt-1", badge("Archiviato", "bg-slate-100 text-slate-500 border-slate-200"))),
        ),
        td(cls := "px-4 py-2 text-slate-700", row.description),
        td(cls := "px-4 py-2 text-slate-500", row.productType.getOrElse("—")),
        td(cls := "px-4 py-2 text-slate-500", title := WarehouseFormats.unitLabel(row.unit), WarehouseFormats.unitShortLabel(row.unit)),
        td(
          cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums",
          cls := (if row.stock.negative then "font-medium text-rose-700" else ""),
          WarehouseFormats.formatDecimal(row.stock.quantity, 0, 3),
          Option.when(row.stock.negative)(div(cls := "mt-1", badge("Anomalia", "bg-rose-50 text-rose-700 border-rose-200"))),
        ),
        td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums", row.stock.maxUnitCost.fold("—")(WarehouseFormats.formatUnitPrice)),
        td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums", row.stock.value.fold("—")(WarehouseFormats.formatEuro)),
        td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums", row.customerPrice.fold("—")(WarehouseFormats.formatUnitPrice)),
        td(cls := "px-4 py-2 text-slate-600", suppliersCell(row)),
      )

    def rowsTable(report: WarehouseStateResponse): HtmlElement =
      card(
        cls := "overflow-hidden",
        div(
          cls := "flex items-center justify-between border-b border-slate-100 px-4 py-2 text-xs text-slate-500",
          span(s"Situazione a fine giornata del ${IsoDay.display(report.at)}"),
          child.text <-- search.signal.map { query =>
            val shown = WarehouseViews.filterRows(report.rows, query).size
            if shown == report.rows.size then s"${report.rows.size} prodotti" else s"$shown di ${report.rows.size} prodotti"
          },
        ),
        child <-- search.signal.map(WarehouseViews.filterRows(report.rows, _)).map { rows =>
          if report.rows.isEmpty then div(cls := "p-4", emptyState("Nessun prodotto esistente in questa data."))
          else if rows.isEmpty then div(cls := "p-4", emptyState("Nessun prodotto corrisponde alla ricerca."))
          else
            div(
              cls := "overflow-x-auto",
              table(
                cls := "w-full text-sm",
                thead(
                  cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                  tr(
                    th(cls := "px-4 py-2", "Codice"),
                    th(cls := "px-4 py-2", "Descrizione"),
                    th(cls := "px-4 py-2", "Tipologia"),
                    th(cls := "px-4 py-2", "UdM"),
                    th(cls := "px-4 py-2 text-right", "Quantità"),
                    th(cls := "px-4 py-2 text-right", "Costo unitario max"),
                    th(cls := "px-4 py-2 text-right", "Valore"),
                    th(cls := "px-4 py-2 text-right", "Prezzo cliente"),
                    th(cls := "px-4 py-2", "Fornitori"),
                  ),
                ),
                tbody(rows.map(renderRow)),
              ),
            )
        },
      )

    div(
      cls := "space-y-4",
      dayCheck --> {
        case Right(day) => applied.set(day)
        case Left(_) => ()
      },
      toolbar,
      child.maybe <-- dayCheck.map(_.left.toOption.map(message => div(cls := "text-xs text-rose-600", message))),
      child.maybe <-- exportError.signal.map(_.map(errorBanner)),
      renderResult(state)(report => div(cls := "space-y-4", totals(report.totals), rowsTable(report))),
    )
  end apply
end HistoryTab
