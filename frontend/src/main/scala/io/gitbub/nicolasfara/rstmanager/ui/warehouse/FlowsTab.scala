package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.WarehouseViews.Granularity

/**
 * Incoming and outgoing stock flows ("Entrate/Uscite") per week or month over a period: summary cards, a bar chart (Tailwind divs, two bars per
 * bucket), the table of buckets with the quantities per unit of measure, and the spend per supplier of the same period with a per-MPN breakdown.
 */
object FlowsTab:

  private val toggleBase = "rounded-full px-3 py-1 text-xs font-medium border transition-colors"

  private def toggleCls(active: Boolean): String =
    if active then s"$toggleBase bg-slate-900 text-white border-slate-900"
    else s"$toggleBase bg-white text-slate-600 border-slate-300 hover:bg-slate-100"

  private def signCls(value: String): String =
    val sign = WarehouseViews.signOf(value)
    if sign > 0 then "text-emerald-700" else if sign < 0 then "text-rose-700" else "text-slate-800"

  def apply(): HtmlElement =
    val today = WarehousePage.today()
    val granularity = Var(Granularity.Month)
    val period = Var(WarehouseViews.defaultPeriod(Granularity.Month, today))
    val check = period.signal.map(p => WarehouseViews.validatePeriod(p.from, p.to)).distinct
    // The last valid period: an invalid one shows a hint and keeps the reports of the previous period on screen.
    val applied = Var(period.now())
    val appliedPeriod = applied.signal.distinct
    val flows = loadable(AppBus.warehouseTicks.combineWith(appliedPeriod, granularity.signal.distinct))(() =>
      ApiClient.warehouseFlows(applied.now().from, applied.now().to, granularity.now().wireName),
    )
    val spend = loadable(AppBus.warehouseTicks.combineWith(appliedPeriod))(() => ApiClient.supplierSpend(applied.now().from, applied.now().to))
    val expanded = Var(Set.empty[UUID])

    def selectGranularity(next: Granularity): Unit =
      val previous = granularity.now()
      if previous != next then
        period.update(WarehouseViews.periodOnToggle(_, previous, next, today))
        granularity.set(next)

    def toolbar: HtmlElement =
      card(
        cls := "flex flex-wrap items-end gap-3 p-4",
        div(
          span(cls := "mb-1 block text-xs font-medium text-slate-500", "Raggruppa per"),
          div(
            cls := "flex gap-2 py-1",
            Granularity.values.toList.map { g =>
              button(
                tpe := "button",
                cls <-- granularity.signal.map(active => toggleCls(active == g)),
                g.label,
                onClick --> (_ => selectGranularity(g)),
              )
            },
          ),
        ),
        div(
          cls := "w-40",
          field(
            "Dal",
            textInput(period.signal.map(_.from), Observer[String](v => period.update(_.copy(from = v))), "", "date").amend(maxAttr := today),
          ),
        ),
        div(
          cls := "w-40",
          field("Al", textInput(period.signal.map(_.to), Observer[String](v => period.update(_.copy(to = v))), "", "date").amend(maxAttr := today)),
        ),
        button(
          tpe := "button",
          cls := s"$btnGhost mb-0.5 disabled:opacity-40",
          "Periodo predefinito",
          disabled <-- period.signal.combineWith(granularity.signal).map((p, g) => p == WarehouseViews.defaultPeriod(g, today)),
          onClick --> (_ => period.set(WarehouseViews.defaultPeriod(granularity.now(), today))),
        ),
      )

    def summaryCard(label: String, value: String, valueCls: String, sub: Option[String]): HtmlElement =
      card(
        cls := "p-4",
        div(cls := "text-xs font-medium text-slate-500", label),
        div(cls := s"mt-1 text-lg font-semibold tabular-nums $valueCls", value),
        sub.map(text => div(cls := "mt-0.5 text-xs text-slate-400", text)),
      )

    def summary(report: FlowReportResponse): HtmlElement =
      div(
        cls := "grid gap-3 sm:grid-cols-3",
        summaryCard(
          "Entrate",
          WarehouseFormats.formatEuro(report.totals.inValue),
          "text-emerald-700",
          Some(s"di cui acquisti ${WarehouseFormats.formatEuro(report.totals.purchaseValue)}"),
        ),
        summaryCard("Uscite", WarehouseFormats.formatEuro(report.totals.outValue), "text-rose-700", None),
        summaryCard("Netto", WarehouseFormats.formatEuro(report.totals.net), signCls(report.totals.net), None),
      )

    def legendDot(colorCls: String, text: String): HtmlElement =
      span(cls := "inline-flex items-center gap-1.5", span(cls := s"inline-block h-2.5 w-2.5 rounded-sm $colorCls"), text)

    def bar(colorCls: String, percent: Double, tooltip: String): HtmlElement =
      div(cls := s"w-4 rounded-t $colorCls", styleAttr := f"height: $percent%.2f%%", title := tooltip)

    def chart(report: FlowReportResponse): HtmlElement =
      val bars = WarehouseViews.scaleBars(report.buckets.map(b => (b.totals.inValue, b.totals.outValue)))
      card(
        cls := "p-4",
        div(
          cls := "mb-3 flex flex-wrap items-center justify-between gap-2",
          sectionTitle("Andamento"),
          div(cls := "flex gap-4 text-xs text-slate-500", legendDot("bg-emerald-500", "Entrate"), legendDot("bg-rose-500", "Uscite")),
        ),
        if report.buckets.isEmpty then emptyState("Nessun periodo da mostrare.")
        else
          div(
            cls := "overflow-x-auto",
            div(
              cls := "flex min-w-full items-end gap-3",
              report.buckets.zip(bars).map { (bucket, pair) =>
                val label = WarehouseViews.bucketLabel(bucket)
                div(
                  cls := "flex min-w-[4.5rem] flex-1 flex-col items-center gap-1",
                  div(
                    cls := "flex h-40 w-full items-end justify-center gap-1 border-b border-slate-200",
                    bar("bg-emerald-500", pair.inPercent, s"$label — Entrate: ${WarehouseFormats.formatEuro(bucket.totals.inValue)}"),
                    bar("bg-rose-500", pair.outPercent, s"$label — Uscite: ${WarehouseFormats.formatEuro(bucket.totals.outValue)}"),
                  ),
                  div(cls := "text-center text-[11px] leading-tight text-slate-500", label),
                )
              },
            ),
          )
        ,
        Option.when(report.buckets.exists(bucket => WarehouseViews.signOf(bucket.totals.outValue) < 0))(
          div(
            cls := "mt-3 text-xs text-slate-500",
            "Uscite negative: in quel periodo gli storni di lavorazioni riaperte superano gli scarichi (nessuna barra, l'importo è nel dettaglio).",
          ),
        ),
      )
    end chart

    def quantitiesCell(flows: List[UnitFlowResponse]): HtmlElement =
      if flows.isEmpty then div(cls := "text-slate-400", "—")
      else
        div(flows.map { flow =>
          div(
            cls := "whitespace-nowrap tabular-nums",
            span(cls := "text-emerald-700", s"+${WarehouseFormats.formatQty(flow.quantityIn, flow.unit)}"),
            span(cls := "text-slate-300", " / "),
            span(cls := "text-rose-700", s"−${WarehouseFormats.formatQty(flow.quantityOut, flow.unit)}"),
          )
        })

    def amountCells(totals: FlowTotalsResponse): List[HtmlElement] =
      List(
        td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums text-emerald-700", WarehouseFormats.formatEuro(totals.inValue)),
        td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums text-slate-500", WarehouseFormats.formatEuro(totals.purchaseValue)),
        td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums text-rose-700", WarehouseFormats.formatEuro(totals.outValue)),
        td(cls := s"px-4 py-2 whitespace-nowrap text-right font-medium tabular-nums ${signCls(totals.net)}", WarehouseFormats.formatEuro(totals.net)),
        td(cls := "px-4 py-2 text-xs", quantitiesCell(totals.quantities)),
      )

    def bucketsTable(report: FlowReportResponse): HtmlElement =
      card(
        cls := "overflow-hidden",
        div(cls := "border-b border-slate-100 px-4 py-3", sectionTitle("Dettaglio per periodo")),
        if report.buckets.isEmpty then div(cls := "p-4", emptyState("Nessun periodo da mostrare."))
        else
          div(
            cls := "overflow-x-auto",
            table(
              cls := "w-full text-sm",
              thead(
                cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                tr(
                  th(cls := "px-4 py-2", "Periodo"),
                  th(cls := "px-4 py-2 text-right", "Entrate"),
                  th(cls := "px-4 py-2 text-right", "di cui acquisti"),
                  th(cls := "px-4 py-2 text-right", "Uscite"),
                  th(cls := "px-4 py-2 text-right", "Netto"),
                  th(cls := "px-4 py-2", "Quantità (entrate / uscite)"),
                ),
              ),
              tbody(
                report.buckets.map { bucket =>
                  tr(
                    cls := "border-b border-slate-100 align-top last:border-0",
                    td(
                      cls := "px-4 py-2",
                      div(cls := "font-medium text-slate-800", WarehouseViews.bucketLabel(bucket)),
                      div(cls := "text-xs text-slate-400", WarehouseViews.bucketRange(bucket.from, bucket.to)),
                    ),
                    amountCells(bucket.totals),
                  )
                },
              ),
              tfoot(
                cls := "border-t border-slate-200 bg-slate-50 font-medium",
                tr(cls := "align-top", td(cls := "px-4 py-2 text-slate-800", "Totale"), amountCells(report.totals)),
              ),
            ),
          ),
      )

    def supplierRows(s: SupplierSpendResponse): HtmlElement =
      val isOpen = expanded.signal.map(_.contains(s.supplierId))
      tbody(
        cls := "border-b border-slate-100 last:border-0",
        tr(
          cls := "cursor-pointer hover:bg-slate-50",
          onClick --> (_ => expanded.update(set => if set.contains(s.supplierId) then set - s.supplierId else set + s.supplierId)),
          td(
            cls := "px-4 py-2",
            span(cls := "mr-2 inline-block w-3 text-slate-400", child.text <-- isOpen.map(open => if open then "▾" else "▸")),
            span(cls := (if s.active then "font-medium text-slate-800" else "font-medium text-slate-400 line-through"), s.name),
            Option.when(!s.active)(span(cls := "ml-2", badge("Eliminato", "bg-slate-100 text-slate-500 border-slate-200"))),
          ),
          td(cls := "px-4 py-2 text-right tabular-nums text-slate-600", s.purchases.toString),
          td(cls := "px-4 py-2 whitespace-nowrap text-right font-medium tabular-nums text-slate-800", WarehouseFormats.formatEuro(s.amount)),
        ),
        children <-- isOpen.map { open =>
          if !open then Nil
          else
            s.mpns.map { mpn =>
              tr(
                cls := "bg-slate-50/60 text-xs",
                td(
                  cls := "py-1.5 pl-11 pr-4",
                  div(cls := "text-slate-700", s"${mpn.internalCode} — ${mpn.description}"),
                  div(
                    cls := "text-slate-400",
                    s"MPN ${mpn.mpnCode} · ${WarehouseFormats.formatQty(mpn.quantity, mpn.unit)}",
                  ),
                ),
                td(cls := "px-4 py-1.5 text-right tabular-nums text-slate-500", mpn.purchases.toString),
                td(cls := "px-4 py-1.5 whitespace-nowrap text-right tabular-nums text-slate-700", WarehouseFormats.formatEuro(mpn.amount)),
              )
            }
        },
      )
    end supplierRows

    def spendTable(report: SupplierSpendReportResponse): HtmlElement =
      if report.suppliers.isEmpty then div(cls := "p-4", emptyState("Nessun acquisto nel periodo selezionato."))
      else
        div(
          cls := "overflow-x-auto",
          table(
            cls := "w-full text-sm",
            thead(
              cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
              tr(th(cls := "px-4 py-2", "Fornitore"), th(cls := "px-4 py-2 text-right", "Acquisti"), th(cls := "px-4 py-2 text-right", "Importo")),
            ),
            report.suppliers.map(supplierRows),
            tfoot(
              cls := "border-t border-slate-200 bg-slate-50 font-medium",
              tr(
                td(cls := "px-4 py-2 text-slate-800", "Totale"),
                td(cls := "px-4 py-2 text-right tabular-nums text-slate-600", report.suppliers.map(_.purchases).sum.toString),
                td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums text-slate-800", WarehouseFormats.formatEuro(report.total)),
              ),
            ),
          ),
        )

    div(
      cls := "space-y-4",
      check --> {
        case Right(valid) => applied.set(valid)
        case Left(_) => ()
      },
      toolbar,
      child.maybe <-- check.map(_.left.toOption.map(message => div(cls := "text-xs text-rose-600", message))),
      div(cls := "text-xs text-slate-500", child.text <-- appliedPeriod.map(p => s"Periodo: ${IsoDay.display(p.from)} – ${IsoDay.display(p.to)}")),
      renderResult(flows)(report => div(cls := "space-y-4", summary(report), chart(report), bucketsTable(report))),
      card(
        cls := "overflow-hidden",
        div(
          cls := "border-b border-slate-100 px-4 py-3",
          sectionTitle("Spesa per fornitore"),
          div(cls := "mt-0.5 text-xs text-slate-400", "Acquisti del periodo; clicca un fornitore per il dettaglio per MPN."),
        ),
        renderResult(spend)(spendTable),
      ),
    )
  end apply
end FlowsTab
