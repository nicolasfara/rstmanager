package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, Pagination, Paging, TableSort, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column
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
    val sort = TableSort[DeliveryNoteResponse](pager)(
      "date" -> Column.text(_.date),
      "number" -> Column.text(_.number),
      "supplier" -> Column.text(_.supplierName),
      "lines" -> Column.by(_.lines.size),
      "total" -> Column.decimal(_.total),
      "status" -> Column.by(WarehouseViews.isCancelled),
    )
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
    // What the modal's ✕ and Escape run: the open editor swaps in its draft-guarded cancel (asks before discarding a dirty DDT).
    val cancelEditor = Var(closeEditor)

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

    /** "Modifica" (Operator) and "Annulla DDT" (Admin); a cancelled note has none. */
    def actionButtons(note: DeliveryNoteResponse): List[Modifier[HtmlElement]] =
      if WarehouseViews.isCancelled(note) then Nil
      else
        List(
          roleGated(Role.Operator)(button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => editor.set(Some(Target.Edit(note)))))),
          roleGated(Role.Admin)(button(tpe := "button", cls := btnDanger, "Annulla DDT", onClick --> (_ => pendingCancel.set(Some(note))))),
        )

    def actionsCell(note: DeliveryNoteResponse): HtmlElement = div(cls := "flex flex-wrap justify-end gap-1.5", actionButtons(note))

    def pendingLineBadge(note: DeliveryNoteResponse, line: DeliveryNoteLineResponse): Modifier[HtmlElement] =
      Option.when(!line.applied && !WarehouseViews.isCancelled(note))(badge("non ancora applicato", pendingBadge))

    /** A line of an expanded note below `md`: code, description, and MPN on the left, total and quantity × unit cost on the right. */
    def lineCard(note: DeliveryNoteResponse, line: DeliveryNoteLineResponse): HtmlElement =
      div(
        cls := "space-y-1 p-3 text-sm",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "break-words font-medium text-slate-800", line.internalCode),
            div(cls := "break-words text-xs text-slate-600", line.description),
            div(cls := "break-words text-xs text-slate-400", s"MPN ${line.mpnCode}"),
          ),
          div(
            cls := "shrink-0 text-right",
            div(cls := "whitespace-nowrap font-medium tabular-nums text-slate-800", WarehouseFormats.formatEuro(line.total)),
            div(
              cls := "whitespace-nowrap text-xs tabular-nums text-slate-500",
              s"${WarehouseFormats.formatQty(line.quantity, line.unit)} × ${WarehouseFormats.formatUnitPrice(line.unitCost)}",
            ),
          ),
        ),
        pendingLineBadge(note, line),
      )

    def linesTable(note: DeliveryNoteResponse): HtmlElement =
      div(
        cls := "space-y-2 px-4 py-3",
        note.note.map(text => div(cls := "text-xs text-slate-500", span(cls := "font-medium text-slate-600", "Note: "), text)),
        // Mobile: one mini-card per line. Desktop (md+): the table.
        div(cls := "divide-y divide-slate-100 rounded-md border border-slate-200 bg-white md:hidden", note.lines.map(lineCard(note, _))),
        div(
          cls := "hidden overflow-x-auto rounded-md border border-slate-200 bg-white md:block",
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
                  td(cls := "px-3 py-2 wrap-anywhere font-medium text-slate-800", line.internalCode),
                  td(cls := "px-3 py-2 wrap-anywhere text-slate-600", line.description),
                  td(cls := "px-3 py-2 wrap-anywhere text-slate-600", line.mpnCode),
                  td(cls := "px-3 py-2 whitespace-nowrap text-right tabular-nums", WarehouseFormats.formatQty(line.quantity, line.unit)),
                  td(cls := "px-3 py-2 whitespace-nowrap text-right tabular-nums", WarehouseFormats.formatUnitPrice(line.unitCost)),
                  td(cls := "px-3 py-2 whitespace-nowrap text-right font-medium tabular-nums", WarehouseFormats.formatEuro(line.total)),
                  td(cls := "px-3 py-2 text-right", pendingLineBadge(note, line)),
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
              aria.label <-- isOpen.map(open =>
                if open then s"Nascondi le righe del DDT n. ${note.number}" else s"Mostra le righe del DDT n. ${note.number}",
              ),
              aria.expanded <-- isOpen,
              child.text <-- isOpen.map(open => if open then "▾" else "▸"),
              onClick --> (_ => toggle(note.id)),
            ),
          ),
          td(cls := "px-3 py-2 whitespace-nowrap", IsoDay.display(note.date)),
          td(cls := s"px-3 py-2 wrap-anywhere font-medium ${if cancelled then "line-through" else "text-slate-800"}", note.number),
          td(
            cls := "px-3 py-2 wrap-anywhere",
            note.supplierName,
            Option.when(!note.supplierActive)(span(cls := "text-xs text-slate-400", " (eliminato)")),
          ),
          td(cls := "px-3 py-2 text-right tabular-nums", note.lines.size.toString),
          td(
            cls := s"px-3 py-2 whitespace-nowrap text-right font-medium tabular-nums ${if cancelled then "line-through" else "text-slate-800"}",
            WarehouseFormats.formatEuro(note.total),
          ),
          td(cls := "px-3 py-2", statusCell(note)),
          td(cls := "px-3 py-2 text-right", actionsCell(note)),
        ),
        tr(
          cls := "border-b border-slate-100 bg-slate-50/60",
          cls <-- isOpen.map(open => if open then "" else "hidden"),
          td(colSpan := 8, linesTable(note)),
        ),
      )
    end renderNote

    /** The card of a note below `md`: number, date, and supplier with the status on the right; lines · total; the expand toggle and the actions. */
    def renderNoteCard(note: DeliveryNoteResponse): HtmlElement =
      val cancelled = WarehouseViews.isCancelled(note)
      val isOpen = expanded.signal.map(_.contains(note.id))
      div(
        cls := "space-y-3 p-4",
        cls := (if cancelled then "bg-slate-50 text-slate-400" else ""),
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := s"break-words font-medium ${if cancelled then "line-through" else "text-slate-800"}", s"DDT n. ${note.number}"),
            div(cls := "text-xs text-slate-500", IsoDay.display(note.date)),
            div(
              cls := "mt-0.5 break-words text-sm",
              cls := (if cancelled then "" else "text-slate-600"),
              note.supplierName,
              Option.when(!note.supplierActive)(span(cls := "text-xs text-slate-400", " (eliminato)")),
            ),
          ),
          div(cls := "shrink-0 text-right", statusCell(note)),
        ),
        div(
          cls := "flex items-baseline justify-between gap-3 text-sm",
          span(cls := "text-slate-500", s"${note.lines.size} ${if note.lines.size == 1 then "riga" else "righe"}"),
          span(
            cls := s"whitespace-nowrap font-medium tabular-nums ${if cancelled then "line-through" else "text-slate-800"}",
            WarehouseFormats.formatEuro(note.total),
          ),
        ),
        div(
          cls := cardActionsCls,
          button(
            tpe := "button",
            cls := btnSmall,
            aria.expanded <-- isOpen,
            child.text <-- isOpen.map(open => if open then "▾ Nascondi righe" else "▸ Mostra righe"),
            onClick --> (_ => toggle(note.id)),
          ),
          actionButtons(note),
        ),
        child.maybe <-- isOpen.map(open => Option.when(open)(div(cls := "-mx-4 -mb-4 bg-slate-50/60", linesTable(note)))),
      )
    end renderNoteCard

    def notesTable(list: List[DeliveryNoteResponse]): HtmlElement =
      card(
        cls := "overflow-hidden",
        child <-- search.signal.map(WarehouseViews.filterDeliveryNotes(list, _)).map { shown =>
          div(
            div(
              cls := "flex flex-wrap items-center justify-between gap-x-3 gap-y-1 border-b border-slate-100 px-4 py-2 text-xs text-slate-500",
              span(if shown.size == list.size then s"${list.size} DDT" else s"${shown.size} di ${list.size} DDT"),
              span(
                "Totale (esclusi annullati): ",
                span(cls := "font-semibold text-slate-700", WarehouseFormats.formatEuro(WarehouseViews.deliveryNotesTotal(shown))),
              ),
            ),
            if list.isEmpty then div(cls := "p-4", emptyState("Nessun DDT nel periodo selezionato."))
            else if shown.isEmpty then div(cls := "p-4", emptyState("Nessun DDT corrisponde alla ricerca."))
            else
              div(
                div(
                  cls := "border-b border-slate-100 px-4 py-2 md:hidden",
                  sort.picker(
                    List(
                      "date" -> "Data",
                      "number" -> "Numero",
                      "supplier" -> "Fornitore",
                      "lines" -> "Righe",
                      "total" -> "Totale",
                      "status" -> "Stato",
                    ),
                  ),
                ),
                pager.view(sort.sorted(shown)) { page =>
                  div(
                    // Mobile: stacked cards. Desktop (md+): the table.
                    div(cls := "divide-y divide-slate-100 md:hidden", page.map(renderNoteCard)),
                    div(
                      cls := "hidden overflow-x-auto md:block",
                      table(
                        cls := "w-full text-sm",
                        thead(
                          cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                          tr(
                            th(cls := "w-8 px-2 py-2"),
                            sort.header("date", "Data", "px-3 py-2"),
                            sort.header("number", "Numero", "px-3 py-2"),
                            sort.header("supplier", "Fornitore", "px-3 py-2"),
                            sort.header("lines", "Righe", "px-3 py-2 text-right"),
                            sort.header("total", "Totale", "px-3 py-2 text-right"),
                            sort.header("status", "Stato", "px-3 py-2"),
                            th(cls := "px-3 py-2"),
                          ),
                        ),
                        tbody(page.flatMap(renderNote)),
                      ),
                    ),
                  )
                },
              ),
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
      modal(editor.signal.map(_.isDefined), editor.signal.map(_.fold("")(DeliveryNoteEditor.title)), "max-w-6xl", () => cancelEditor.now()())(
        div(child.maybe <-- editor.signal.distinct.map(_.map(DeliveryNoteEditor(_, closeEditor, cancelEditor.set)))),
      ),
    )
  end apply
end DeliveryNotesTab
