package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, Pagination, Paging, TableSort, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/**
 * Day-by-day history of the warehouse ("Storico"): the state at the end of a chosen day — every material existing then, with its stock, highest
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
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[StateRowResponse](pager)(
      "code" -> Column.text(_.internalCode),
      "description" -> Column.text(_.description),
      "type" -> Column.optionalText(_.materialType),
      "unit" -> Column.text(row => WarehouseFormats.unitShortLabel(row.unit)),
      "quantity" -> Column.decimal(_.stock.quantity),
      "maxCost" -> Column.optionalDecimal(_.stock.maxUnitCost),
      "value" -> Column.optionalDecimal(_.stock.value),
      "price" -> Column.optionalDecimal(_.customerPrice),
      "suppliers" -> Column.optionalText(_.suppliers.headOption.map(_.name)),
    )
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
          cls := "w-full sm:w-auto",
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
            // Mobile: the date takes what the arrows and "Oggi" leave, so the row fits 360px.
            div(cls := "min-w-0 flex-1 sm:w-40 sm:flex-none", textInput(selected.signal, selected.writer, "", "date").amend(maxAttr := today)),
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
        div(cls := "min-w-[12rem] flex-1", field("Cerca", searchBox(search, "Codice, descrizione o tipologia", pager))),
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
        totalCard("N° materiali", t.materials.toString, "text-slate-800", None),
        totalCard("Valore magazzino", WarehouseFormats.formatEuro(t.value), "text-slate-800", Some("Giacenza × costo unitario d'acquisto più alto")),
        totalCard(
          "Costo totale acquisti",
          WarehouseFormats.formatEuro(t.purchasedTotal),
          "text-slate-800",
          Some("Speso in acquisti fino a questo giorno"),
        ),
        totalCard(
          "Materiali in negativo",
          t.negativeMaterials.toString,
          if t.negativeMaterials > 0 then "text-rose-700" else "text-slate-800",
          Some("Materiali con giacenza negativa: anomalia da verificare"),
        ),
        totalCard(
          "Non valorizzati",
          t.unvaluedMaterials.toString,
          if t.unvaluedMaterials > 0 then "text-amber-700" else "text-slate-800",
          Some("Materiali senza alcun costo d'acquisto registrato: il loro valore non è conteggiato"),
        ),
      )

    def renderCard(row: StateRowResponse): HtmlElement =
      val summary = WarehouseViews.materialSummary(row)
      MaterialCards.listCard(summary, List(readField("Costo unitario max", MaterialCards.maxCostText(summary))))()

    def renderRow(row: StateRowResponse): HtmlElement =
      val summary = WarehouseViews.materialSummary(row)
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(cls := "px-3 py-2", MaterialCards.materialCell(summary)),
        td(cls := "px-3 py-2 text-right", MaterialCards.stockCell(summary)),
        td(cls := "px-3 py-2 text-right", MaterialCards.maxCostText(summary)),
        td(cls := "px-3 py-2 text-right", MaterialCards.valuePriceCell(summary)),
        td(cls := "px-3 py-2 text-slate-600", MaterialCards.suppliersCell(summary)),
      )

    def rowsTable(report: WarehouseStateResponse): HtmlElement =
      card(
        cls := "overflow-hidden",
        div(
          cls := "flex flex-wrap items-center justify-between gap-x-3 gap-y-1 border-b border-slate-100 px-4 py-2 text-xs text-slate-500",
          span(s"Situazione a fine giornata del ${IsoDay.display(report.at)}"),
          child.text <-- search.signal.map { query =>
            val shown = WarehouseViews.filterRows(report.rows, query).size
            if shown == report.rows.size then s"${report.rows.size} materiali" else s"$shown di ${report.rows.size} materiali"
          },
        ),
        Option.when(report.rows.nonEmpty)(
          div(
            cls := "border-b border-slate-100 px-4 py-2 xl:hidden",
            sort.picker(
              List(
                "code" -> "Codice",
                "description" -> "Descrizione",
                "type" -> "Tipologia",
                "unit" -> "UdM",
                "quantity" -> "Giacenza",
                "maxCost" -> "Costo unitario max",
                "value" -> "Valore",
                "price" -> "Prezzo cliente",
                "suppliers" -> "Fornitori",
              ),
            ),
          ),
        ),
        pager.view(sort.sorted(search.signal.map(WarehouseViews.filterRows(report.rows, _)))) { rows =>
          if report.rows.isEmpty then div(cls := "p-4", emptyState("Nessun materiale esistente in questa data."))
          else if rows.isEmpty then div(cls := "p-4", emptyState("Nessun materiale corrisponde alla ricerca."))
          else
            div(
              // Below xl: stacked cards. From xl: the compact table, laid out like the materials tab.
              div(cls := "divide-y divide-slate-100 xl:hidden", rows.map(renderCard)),
              div(
                cls := "hidden overflow-x-auto xl:block",
                table(
                  cls := "w-full text-sm",
                  thead(
                    cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                    tr(
                      sort.header("px-3 py-2", "code" -> "Codice", "description" -> "Descrizione", "type" -> "Tipo", "unit" -> "UdM"),
                      sort.header("quantity", "Giacenza", "px-3 py-2 text-right"),
                      sort.header("maxCost", "Costo unit. max", "px-3 py-2 text-right"),
                      sort.header("px-3 py-2 text-right", "value" -> "Valore", "price" -> "Prezzo"),
                      sort.header("suppliers", "Fornitori", "px-3 py-2"),
                    ),
                  ),
                  tbody(rows.map(renderRow)),
                ),
              ),
            )
        },
      )

    div(
      cls := "space-y-4",
      dayCheck --> {
        case Right(day) => applied.set(day); pager.reset()
        case Left(_) => ()
      },
      toolbar,
      child.maybe <-- dayCheck.map(_.left.toOption.map(message => div(cls := "text-xs text-rose-600", message))),
      child.maybe <-- exportError.signal.map(_.map(errorBanner)),
      renderResult(state)(report => div(cls := "space-y-4", totals(report.totals), rowsTable(report))),
    )
  end apply
end HistoryTab
