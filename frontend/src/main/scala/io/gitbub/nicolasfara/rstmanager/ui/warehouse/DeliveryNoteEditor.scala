package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, DirtyTracker, MaterialDrafts, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.DeliveryNoteForm.{ Context, Row }

/**
 * The delivery note ("DDT") editor shown in a wide modal of the "DDT" tab: the header (day, number, supplier, note) and the lines, each searched by
 * internal code, MPN, or description among the MPNs of the selected supplier, with quantity (in the material's unit), unit cost (prefilled with the
 * expected price of the MPN), and line total. Registering loads the stock of every material; saving a change makes the server void the previous loads
 * and record the new ones.
 *
 * Every rule lives in the pure [[DeliveryNoteForm]]; this object only binds it to Laminar. The materials (archived ones too, for the lines of a note
 * being edited) and the suppliers are loaded on opening and refreshed on [[AppBus.warehouseTicks]]; the form is built once the materials are loaded.
 */
object DeliveryNoteEditor:

  /** What the editor is open for: a new delivery note, or a change of `note`. */
  enum Target derives CanEqual:
    case New
    case Edit(note: DeliveryNoteResponse)

  def title(target: Target): String = target match
    case Target.New => "Registra DDT"
    case Target.Edit(note) => s"Modifica DDT n. ${note.number} — ${note.supplierName}"

  private def hint(text: String): HtmlElement = div(cls := "mt-1 text-xs text-slate-400", text)

  /** The editor of `target`, rebuilt on every opening; `close` runs on "Annulla" and after a successful save. */
  def apply(target: Target, close: () => Unit): HtmlElement =
    val kept: Set[UUID] = target match
      case Target.New => Set.empty
      case Target.Edit(note) => note.lines.map(_.materialId).toSet
    val materials = loadable(AppBus.warehouseTicks)(() => ApiClient.listMaterials(true))
    val suppliers = loadable(AppBus.warehouseTicks)(() => ApiClient.listSuppliers())
    val context = Var(Context.empty)
    // The outcome of the first load: the form is built once from it and kept while later reloads only refresh `context`.
    val firstLoad = Var(Option.empty[ApiClient.Result[Unit]])
    div(
      materials --> {
        case Some(Right(list)) =>
          context.set(Context.of(list, kept))
          if !firstLoad.now().exists(_.isRight) then firstLoad.set(Some(Right(())))
        case Some(Left(err)) => if !firstLoad.now().exists(_.isRight) then firstLoad.set(Some(Left(err)))
        case None => ()
      },
      child <-- firstLoad.signal.distinct.map {
        case None => spinner
        case Some(Left(err)) => errorBanner(err)
        case Some(Right(_)) => form(target, context, suppliers, close)
      },
    )
  end apply

  private def form(
      target: Target,
      context: Var[Context],
      suppliers: Signal[Option[ApiClient.Result[List[SupplierResponse]]]],
      close: () => Unit,
  ): HtmlElement =
    val today = WarehousePage.today()
    val initial = target match
      case Target.New => DeliveryNoteForm.empty(today)
      case Target.Edit(note) => DeliveryNoteForm.fromResponse(note, context.now(), today)
    val state = Var(initial)
    val tracker = DirtyTracker(initial, state)
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    val withContext = state.signal.combineWith(context.signal)
    val errors = withContext.map((s, ctx) => if s.pristine then Nil else s.errors(ctx))
    val canSubmit = withContext.combineWith(tracker.isDirty).map((s, ctx, dirty) => dirty && s.request(ctx).isDefined)

    val currentSupplier: Option[(UUID, String)] = target match
      case Target.New => None
      case Target.Edit(note) => Some(note.supplierId -> note.supplierName)
    val supplierOptions: Signal[List[(String, String)]] = suppliers.map {
      case Some(Right(list)) => DeliveryNoteForm.supplierOptions(list, currentSupplier)
      case _ => currentSupplier.toList.map((id, name) => id.toString -> name)
    }
    val lineOptions: Signal[List[(String, String, String)]] =
      state.signal.map(_.supplierId).distinct.combineWith(context.signal).map((supplierId, ctx) => ctx.selectOptions(supplierId))

    def submit(): Unit =
      state.now().request(context.now()).filter(_ => !submitting.now()).foreach { request =>
        submitting.set(true)
        error.set(None)
        val effect = state.now().editingId.fold(ApiClient.createDeliveryNote(request))(ApiClient.updateDeliveryNote(_, request))
        effect.foreach { result =>
          submitting.set(false)
          result match
            case Right(_) =>
              AppBus.mutatedWarehouse()
              close()
            case Left(err) => showError(error, "Registrazione DDT")(err)
        }
      }

    def update(key: Int)(patch: Row => Row): Unit = state.update(_.updateRow(key)(patch))

    def renderRow(key: Int, rowSignal: Signal[Row]): HtmlElement =
      // What the row shows besides its inputs: the selected option, the price note, and the line total.
      val view = rowSignal.combineWith(state.signal, context.signal).map { (row, s, ctx) =>
        (s.optionOf(row, ctx), s.priceNote(row, ctx), s.lineTotal(row, ctx))
      }
      val selected = view.map(_._1)
      div(
        cls := "grid grid-cols-1 gap-2 rounded-md border border-slate-200 bg-white p-2 sm:grid-cols-[minmax(0,1fr)_7.5rem_9rem_6.5rem_auto] sm:items-start",
        div(
          field(
            "Materiale",
            searchableSelect(
              rowSignal.map(_.choice),
              Observer[String](v => state.update(_.choose(key, v, context.now()))),
              lineOptions,
              "Codice, MPN o descrizione…",
              20,
            ),
          ),
          child.maybe <-- selected.map(_.exists(_.isNewMpn)).distinct.map { isNew =>
            Option.when(isNew)(
              div(
                cls := "mt-2",
                field(
                  "Codice nuovo MPN",
                  textInput(rowSignal.map(_.newMpnCode), Observer[String](v => update(key)(_.copy(newMpnCode = v))), "Es. AC-152-NAVY"),
                ),
                hint("Il materiale non ha MPN di questo fornitore: viene aggiunto con questo codice."),
              ),
            )
          },
        ),
        field(
          "Quantità",
          div(
            textInput(rowSignal.map(_.quantity), Observer[String](v => update(key)(_.copy(quantity = v))), "25,5").amend(inputMode := "decimal"),
            div(
              cls := "mt-1 text-xs text-slate-400",
              child.text <-- selected.map(
                _.fold("")(o => WarehouseFormats.unitLabel(o.unit) + (if MaterialDrafts.isPieces(o.unit) then " (interi)" else "")),
              ),
            ),
          ),
        ),
        field(
          "Costo unit. (€)",
          div(
            textInput(rowSignal.map(_.unitCost), Observer[String](v => state.update(_.setCost(key, v))), "")
              .amend(
                inputMode := "decimal",
                placeholder <-- selected.map(_.flatMap(_.expectedUnitCost).fold("3,50")(DeliveryNoteForm.costText)),
              ),
            div(cls := "mt-1 text-xs text-slate-400", child.text <-- view.map(_._2.getOrElse(""))),
          ),
        ),
        div(
          span(cls := "mb-1 block text-xs font-medium text-slate-500", "Totale"),
          div(cls := "py-1.5 text-sm font-medium tabular-nums text-slate-800", child.text <-- view.map(_._3.fold("—")(WarehouseFormats.formatEuro))),
        ),
        button(tpe := "button", cls := s"$btnDanger justify-center sm:mt-5", "Rimuovi", onClick --> (_ => state.update(_.removeRow(key)))),
      )
    end renderRow

    div(
      cls := "space-y-4",
      div(
        cls := "grid gap-3 sm:grid-cols-4",
        field(
          "Data DDT",
          textInput(state.signal.map(_.date), Observer[String](v => state.update(_.copy(date = v))), "", "date").amend(maxAttr := today),
        ),
        field("Numero DDT", textInput(state.signal.map(_.number), Observer[String](v => state.update(_.copy(number = v))), "Es. 1234/A")),
        div(
          cls := "sm:col-span-2",
          field(
            "Fornitore",
            searchableSelect(
              state.signal.map(_.supplierId),
              Observer[String](v => state.update(_.withSupplier(v, context.now()))),
              supplierOptions,
              "Cerca un fornitore…",
              8,
            ),
          ),
        ),
        div(
          cls := "sm:col-span-4",
          field("Note", textInput(state.signal.map(_.note), Observer[String](v => state.update(_.copy(note = v))), "Opzionale")),
        ),
      ),
      div(
        cls := "space-y-2",
        div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Righe"),
        child.maybe <-- state.signal
          .map(_.supplierId.trim.nn.isEmpty)
          .distinct
          .map(noSupplier =>
            Option.when(noSupplier)(hint("Seleziona il fornitore: i materiali si cercano tra i suoi MPN per codice, MPN o descrizione.")),
          ),
        div(cls := "space-y-2", children <-- state.signal.map(_.rows).split(_.key)((key, _, rowSignal) => renderRow(key, rowSignal))),
        div(
          cls := "flex flex-wrap items-center justify-between gap-2",
          button(tpe := "button", cls := btnSmall, "+ Riga", onClick --> (_ => state.update(_.addRow))),
          div(
            cls := "text-sm text-slate-600",
            "Totale DDT: ",
            span(
              cls := "font-semibold tabular-nums text-slate-800",
              child.text <-- withContext.map((s, ctx) => WarehouseFormats.formatEuro(s.total(ctx))),
            ),
          ),
        ),
      ),
      Option.when(initial.editingId.isDefined)(
        hint("Salvando, i carichi registrati con questo DDT vengono annullati e registrati di nuovo con le righe aggiornate."),
      ),
      child.maybe <-- errors.map(list => Option.when(list.nonEmpty)(ul(cls := "list-disc pl-5 text-xs text-rose-600", list.map(e => li(e))))),
      child.maybe <-- error.signal.map(_.map(errorBanner)),
      StockDialogs.formActions(
        if initial.editingId.isDefined then "Salva modifiche" else "Registra DDT",
        canSubmit,
        submitting.signal,
        close,
        () => submit(),
      ),
    )
  end form
end DeliveryNoteEditor
