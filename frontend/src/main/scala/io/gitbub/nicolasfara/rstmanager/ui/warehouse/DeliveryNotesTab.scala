package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, Pagination, Paging, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.DeliveryNoteEditor.Target

/**
 * Delivery notes ("DDT") of the warehouse: the history of the registered notes — newest first, filtered by period and supplier on the server and by
 * number on screen — each expandable to its lines. "Registra DDT" and "Modifica" (Operator) open the [[DeliveryNoteEditor]]; "Annulla DDT" (Admin)
 * cancels a note, removing the stock it loaded, after asking for a reason. A cancelled note stays listed as such, without actions; a line whose stock
 * load the server has not applied yet is marked "non ancora applicato".
 */
object DeliveryNotesTab:

  private val pendingBadge = "bg-amber-50 text-amber-700 border-amber-200"

  def apply(): HtmlElement =
    val today = WarehousePage.today()
    val from = Var("")
    val to = Var("")
    val supplierFilter = Var("")
    val search = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val range = from.signal.combineWith(to.signal).map(WarehouseViews.validateOptionalRange).distinct
    // The last valid filter: an invalid period shows a hint and keeps the list of the previous one.
    val applied = Var((Option.empty[String], Option.empty[String], Option.empty[UUID]))
    val notes = loadable(AppBus.warehouseTicks.combineWith(applied.signal.distinct)) { () =>
      applied.now() match
        case (start, end, supplier) => ApiClient.listDeliveryNotes(start, end, supplier)
    }
    val suppliers = loadable(AppBus.warehouseTicks)(() => ApiClient.listSuppliers())
    val editor = Var(Option.empty[Target])
    val expanded = Var(Set.empty[UUID])
    val pendingCancel = Var(Option.empty[DeliveryNoteResponse])
    val cancelReason = Var("")
    val actionError = Var(Option.empty[ApiError])
    val closeEditor: () => Unit = () => editor.set(None)

    val supplierFilterOptions: Signal[List[(String, String)]] = suppliers.map { loaded =>
      val listed = loaded match
        case Some(Right(list)) => DeliveryNoteForm.supplierOptions(list, None)
        case _ => Nil
      ("" -> "Tutti i fornitori") :: listed
    }

    def toggle(id: UUID): Unit = expanded.update(open => if open.contains(id) then open - id else open + id)

    def cancelCancel(): Unit =
      pendingCancel.set(None)
      cancelReason.set("")

    def confirmCancel(): Unit =
      val reason = cancelReason.now().trim.nn
      pendingCancel.now().filter(_ => reason.nonEmpty).foreach { note =>
        cancelCancel()
        actionError.set(None)
        ApiClient.cancelDeliveryNote(note.id, reason).foreach {
          case Right(_) => AppBus.mutatedWarehouse()
          case Left(err) => showError(actionError, "Annullo DDT")(err)
        }
      }

    val cancelDialog =
      confirmDialog(
        isOpen = pendingCancel.signal.map(_.isDefined),
        titleText = Signal.fromValue("Annulla DDT"),
        message = pendingCancel.signal.map(_.fold("") { n =>
          s"Annullare il DDT n. ${n.number} del ${IsoDay.display(n.date)} (${n.supplierName})? I carichi registrati con le sue righe vengono tolti " +
            "dalle giacenze; il DDT resta nello storico come annullato."
        }),
        confirmLabel = "Annulla DDT",
        tone = DialogTone.Danger,
        body = div(cls := "mt-4 text-left", field("Motivo dell'annullo", textInput(cancelReason, "Es. DDT registrato due volte"))),
        confirmEnabled = cancelReason.signal.map(_.trim.nn.nonEmpty),
        onCancel = () => cancelCancel(),
        onConfirm = () => confirmCancel(),
      )

    def toolbar: HtmlElement =
      card(
        cls := "flex flex-wrap items-end gap-3 p-4",
        div(cls := "w-40", field("Dal", textInput(from.signal, from.writer, "", "date").amend(maxAttr := today))),
        div(cls := "w-40", field("Al", textInput(to.signal, to.writer, "", "date").amend(maxAttr := today))),
        button(
          tpe := "button",
          cls := s"$btnGhost mb-0.5 disabled:opacity-40",
          "Tutto il periodo",
          disabled <-- from.signal.combineWith(to.signal).map((f, t) => f.isEmpty && t.isEmpty),
          onClick --> { _ => from.set(""); to.set("") },
        ),
        div(cls := "w-56", field("Fornitore", selectInput(supplierFilter, supplierFilterOptions))),
        div(cls := "min-w-[10rem] flex-1", field("Cerca", searchBox(search, "Numero DDT", pager))),
        div(
          cls := "ml-auto",
          roleGated(Role.Operator)(button(tpe := "button", cls := btnPrimary, "Registra DDT", onClick --> (_ => editor.set(Some(Target.New))))),
        ),
      )

    def statusCell(note: DeliveryNoteResponse): HtmlElement =
      if WarehouseViews.isCancelled(note) then
        div(
          badge(note.cancelledOn.fold("Annullato")(day => s"Annullato il ${IsoDay.display(day)}"), "bg-rose-50 text-rose-700 border-rose-200"),
          note.cancelReason.map(reason => div(cls := "mt-1 text-xs text-slate-500", reason)),
        )
      else
        div(
          if WarehouseViews.hasPendingLines(note) then
            span(title := "Il carico di alcune righe in magazzino non è ancora stato applicato", badge("Non ancora applicato", pendingBadge))
          else badge("Registrato", "bg-emerald-50 text-emerald-700 border-emerald-200"),
          note.changedOn.map(day => div(cls := "mt-1 text-xs text-slate-400", s"Modificato il ${IsoDay.display(day)}")),
        )

    def actionsCell(note: DeliveryNoteResponse): HtmlElement =
      div(
        cls := "flex flex-wrap justify-end gap-1.5",
        Option.when(!WarehouseViews.isCancelled(note))(
          div(
            cls := "contents",
            roleGated(Role.Operator)(button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => editor.set(Some(Target.Edit(note)))))),
            roleGated(Role.Admin)(button(tpe := "button", cls := btnDanger, "Annulla DDT", onClick --> (_ => pendingCancel.set(Some(note))))),
          ),
        ),
      )

    def linesTable(note: DeliveryNoteResponse): HtmlElement =
      div(
        cls := "space-y-2 px-4 py-3",
        note.note.map(text => div(cls := "text-xs text-slate-500", span(cls := "font-medium text-slate-600", "Note: "), text)),
        div(
          cls := "overflow-x-auto rounded-md border border-slate-200 bg-white",
          table(
            cls := "w-full text-sm",
            thead(
              cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
              tr(
                th(cls := "px-3 py-2", "Codice"),
                th(cls := "px-3 py-2", "Descrizione"),
                th(cls := "px-3 py-2", "MPN"),
                th(cls := "px-3 py-2 text-right", "Quantità"),
                th(cls := "px-3 py-2 text-right", "Costo unit."),
                th(cls := "px-3 py-2 text-right", "Totale"),
                th(cls := "px-3 py-2"),
              ),
            ),
            tbody(
              note.lines.map { line =>
                tr(
                  cls := "border-b border-slate-100 last:border-0",
                  td(cls := "px-3 py-2 font-medium text-slate-800", line.internalCode),
                  td(cls := "px-3 py-2 text-slate-600", line.description),
                  td(cls := "px-3 py-2 text-slate-600", line.mpnCode),
                  td(cls := "px-3 py-2 whitespace-nowrap text-right tabular-nums", WarehouseFormats.formatQty(line.quantity, line.unit)),
                  td(cls := "px-3 py-2 whitespace-nowrap text-right tabular-nums", WarehouseFormats.formatUnitPrice(line.unitCost)),
                  td(cls := "px-3 py-2 whitespace-nowrap text-right font-medium tabular-nums", WarehouseFormats.formatEuro(line.total)),
                  td(
                    cls := "px-3 py-2 text-right",
                    Option.when(!line.applied && !WarehouseViews.isCancelled(note))(badge("non ancora applicato", pendingBadge)),
                  ),
                )
              },
            ),
          ),
        ),
      )

    def renderNote(note: DeliveryNoteResponse): List[HtmlElement] =
      val cancelled = WarehouseViews.isCancelled(note)
      val isOpen = expanded.signal.map(_.contains(note.id))
      List(
        tr(
          cls := "border-b border-slate-100 align-top",
          cls := (if cancelled then "bg-slate-50 text-slate-400" else ""),
          td(
            cls := "px-2 py-2",
            button(
              tpe := "button",
              cls := "px-1 text-slate-400 hover:text-slate-700",
              title <-- isOpen.map(open => if open then "Nascondi le righe" else "Mostra le righe"),
              child.text <-- isOpen.map(open => if open then "▾" else "▸"),
              onClick --> (_ => toggle(note.id)),
            ),
          ),
          td(cls := "px-4 py-2 whitespace-nowrap", IsoDay.display(note.date)),
          td(cls := s"px-4 py-2 font-medium ${if cancelled then "line-through" else "text-slate-800"}", note.number),
          td(
            cls := "px-4 py-2",
            note.supplierName,
            Option.when(!note.supplierActive)(span(cls := "text-xs text-slate-400", " (eliminato)")),
          ),
          td(cls := "px-4 py-2 text-right tabular-nums", note.lines.size.toString),
          td(
            cls := s"px-4 py-2 whitespace-nowrap text-right font-medium tabular-nums ${if cancelled then "line-through" else "text-slate-800"}",
            WarehouseFormats.formatEuro(note.total),
          ),
          td(cls := "px-4 py-2", statusCell(note)),
          td(cls := "px-4 py-2 text-right", actionsCell(note)),
        ),
        tr(
          cls := "border-b border-slate-100 bg-slate-50/60",
          cls <-- isOpen.map(open => if open then "" else "hidden"),
          td(colSpan := 8, linesTable(note)),
        ),
      )
    end renderNote

    def notesTable(list: List[DeliveryNoteResponse]): HtmlElement =
      card(
        cls := "overflow-hidden",
        child <-- search.signal.map(WarehouseViews.filterDeliveryNotes(list, _)).map { shown =>
          div(
            div(
              cls := "flex items-center justify-between border-b border-slate-100 px-4 py-2 text-xs text-slate-500",
              span(if shown.size == list.size then s"${list.size} DDT" else s"${shown.size} di ${list.size} DDT"),
              span(
                "Totale (esclusi annullati): ",
                span(cls := "font-semibold text-slate-700", WarehouseFormats.formatEuro(WarehouseViews.deliveryNotesTotal(shown))),
              ),
            ),
            if list.isEmpty then div(cls := "p-4", emptyState("Nessun DDT nel periodo selezionato."))
            else if shown.isEmpty then div(cls := "p-4", emptyState("Nessun DDT corrisponde alla ricerca."))
            else
              pager.view(shown) { page =>
                div(
                  cls := "overflow-x-auto",
                  table(
                    cls := "w-full text-sm",
                    thead(
                      cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                      tr(
                        th(cls := "w-8 px-2 py-2"),
                        th(cls := "px-4 py-2", "Data"),
                        th(cls := "px-4 py-2", "Numero"),
                        th(cls := "px-4 py-2", "Fornitore"),
                        th(cls := "px-4 py-2 text-right", "Righe"),
                        th(cls := "px-4 py-2 text-right", "Totale"),
                        th(cls := "px-4 py-2", "Stato"),
                        th(cls := "px-4 py-2"),
                      ),
                    ),
                    tbody(page.flatMap(renderNote)),
                  ),
                )
              },
          )
        },
      )

    div(
      cls := "space-y-4",
      range.combineWith(supplierFilter.signal) --> {
        case (Right((start, end)), supplier) =>
          applied.set((start, end, Try(UUID.fromString(supplier).nn).toOption)); pager.reset()
        case (Left(_), _) => ()
      },
      toolbar,
      child.maybe <-- range.map(_.left.toOption.map(message => div(cls := "text-xs text-rose-600", message))),
      child.maybe <-- actionError.signal.map(_.map(errorBanner)),
      renderResult(notes)(notesTable),
      cancelDialog,
      modal(editor.signal.map(_.isDefined), editor.signal.map(_.fold("")(DeliveryNoteEditor.title)), "max-w-6xl", closeEditor)(
        div(child.maybe <-- editor.signal.distinct.map(_.map(DeliveryNoteEditor(_, closeEditor)))),
      ),
    )
  end apply
end DeliveryNotesTab
