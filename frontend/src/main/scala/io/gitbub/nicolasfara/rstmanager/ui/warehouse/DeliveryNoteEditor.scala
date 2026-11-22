package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, DirtyTracker, DraftGuard, FormFocus, FormTouch, MaterialDrafts, Notifications, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.FieldIssue
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.DeliveryNoteForm.{ Context, Field, Row, RowField }

/**
 * The delivery note ("DDT") editor: a full page shown in place of the "DDT" list (see [[DeliveryNotesTab]]), the same page for registering a note and
 * for changing one. The main column has two numbered sections, "Documento" (day, number, supplier, note) and "Righe", each line searched by internal
 * code, MPN, or description among the MPNs of the selected supplier, with quantity (in the material's unit), unit cost (prefilled with the expected
 * price of the MPN), and line total. From `lg` a sticky sidebar sums it up (day, number, supplier, lines and the live "Totale DDT"), lists what is
 * still missing ("Da completare") and holds the actions; below `lg` a fixed bar shows the total and the primary action. Registering loads the stock
 * of every material; saving a change makes the server void the previous loads and record the new ones.
 *
 * Every rule lives in the pure [[DeliveryNoteForm]]; this object only binds it to Laminar. Errors show under their input once it was left or after a
 * submit attempt ([[FormTouch]]); an invalid submit shows them all and focuses the first. The materials (archived ones too, for the lines of a note
 * being edited) and the suppliers are loaded on opening and refreshed on [[AppBus.warehouseTicks]]; the form is built once the materials are loaded
 * (until then the page shows its header over a spinner). Leaving ("← Torna ai DDT", "Annulla", the browser back button, the top navigation, a tab
 * switch) asks first while the draft is dirty.
 */
object DeliveryNoteEditor:

  /** What the editor is open for: a new delivery note, or a change of `note`. */
  enum Target derives CanEqual:
    case New
    case Edit(note: DeliveryNoteResponse)

  def title(target: Target): String = target match
    case Target.New => "Registra DDT"
    case Target.Edit(note) => s"Modifica DDT n. ${note.number} — ${note.supplierName}"

  /** The DOM id of the banner of a refused save (focused when it appears). */
  private val submitErrorId = "ddt-submit-error"

  /**
   * The page of `target`, mounted fresh on every opening. `onExit` goes back to the list ("Annulla" or the back link, once a dirty draft was
   * confirmed discarded; or saving an unchanged note); `onSaved` runs with the id of the note after a successful save (the toast and the [[AppBus]]
   * tick are already done). `cancelled` turns true once a reload of the list shows the edited note as cancelled (by another user): the page then says
   * so, cannot be saved, and leaves without asking.
   */
  def apply(target: Target, onExit: () => Unit, onSaved: UUID => Unit, cancelled: Signal[Boolean]): HtmlElement =
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
      // Until the materials are loaded: the same header (its back link leaves at once: there is no draft yet) over a spinner or the load error.
      child <-- firstLoad.signal.distinct.map {
        case None => loadingPage(target, onExit, spinner)
        case Some(Left(err)) => loadingPage(target, onExit, errorBanner(err))
        case Some(Right(_)) => form(target, context, suppliers, onExit, onSaved, cancelled)
      },
    )
  end apply

  /** The page header of `target`: a create page focuses its first field, an edit page its title. */
  private def header(target: Target, onBack: () => Unit, notes: Modifier[HtmlElement]): PageHeader =
    PageHeader(
      "← Torna ai DDT",
      onBack,
      Signal.fromValue(title(target)),
      context = List("Magazzino", "DDT"),
      notes = notes,
      initialFocus = target match
        case Target.New => PageFocus.FirstField
        case Target.Edit(_) => PageFocus.Title,
    )

  /** The page while the form cannot be built yet: the header (focused on its title) over `body`. */
  private def loadingPage(target: Target, onExit: () => Unit, body: HtmlElement): HtmlElement =
    formPage(header(target, onExit, emptyMod).copy(requiredLegend = false, initialFocus = PageFocus.Title))(card(cls := "p-4", body))

  private def isHeaderIssue(issue: FieldIssue[Field]): Boolean = issue.field match
    case Field.Date | Field.Number | Field.Supplier => true
    case _ => false

  private def form(
      target: Target,
      context: Var[Context],
      suppliers: Signal[Option[ApiClient.Result[List[SupplierResponse]]]],
      onExit: () => Unit,
      onSaved: UUID => Unit,
      cancelled: Signal[Boolean],
  ): HtmlElement =
    val today = WarehousePage.today()
    val initial = target match
      case Target.New => DeliveryNoteForm.empty(today)
      case Target.Edit(note) => DeliveryNoteForm.fromResponse(note, context.now(), today)
    val state = Var(initial)
    val tracker = DirtyTracker(initial, state)
    val (leaveTitle, leaveMessage) = target match
      case Target.New => (DraftGuard.discardTitle(None, "il nuovo DDT"), "Il DDT non è stato registrato: i dati inseriti andranno persi.")
      case Target.Edit(note) => (DraftGuard.discardTitle(Some(s"DDT n. ${note.number}"), "il nuovo DDT"), "Le modifiche non salvate andranno perse.")
    val draft = DraftGuard[DeliveryNoteForm.State](() => state.now(), _ => leaveTitle)
    // Sticky for this opening (see DraftGuard.trackGone).
    val gone = Var(false)
    val leave: () => Unit = () => if gone.now() then onExit() else draft.discardThen(onExit)
    val touch = FormTouch()
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    val withContext = state.signal.combineWith(context.signal)
    val issues = withContext.map((s, ctx) => s.issues(ctx)).distinct
    val totalText = withContext.map((s, ctx) => WarehouseFormats.formatEuro(s.total(ctx))).distinct
    val linesText = state.signal.map(s => DeliveryNoteForm.linesText(s.filledCount)).distinct
    val blocked: Signal[Option[String]] = gone.signal.map(isGone => Option.when(isGone)("DDT annullato: le modifiche non si possono salvare."))
    val editing = initial.editingId.isDefined
    val submitLabel = if editing then "Salva modifiche" else "Registra DDT"
    val busyLabel = if editing then "Salvataggio…" else "Registrazione…"

    /** The raw (ungated) inline error of `field`. */
    def rawError(field: Field): Signal[Option[String]] = withContext.map((s, ctx) => s.inlineError(field, ctx)).distinct

    /** The [[FieldSpec]] of `field`, its error shown once the field was left or a submit was attempted. */
    def spec(labelText: String, field: Field, required: Boolean): FieldSpec =
      touch.spec(labelText, DeliveryNoteForm.fieldId(field), required, rawError(field))

    /** Neutral until the first submit attempt, then the number of `issues` of the section that `count` finds. */
    def sectionStatus(count: List[FieldIssue[Field]] => Int): Signal[SectionStatus] =
      touch.submittedSignal.combineWith(issues).map { (attempted, list) =>
        val n = count(list)
        if !attempted then SectionStatus.Neutral else if n == 0 then SectionStatus.Complete else SectionStatus.Pending(n)
      }

    /** Focuses the input a "Da completare" issue is about. */
    def reveal(issue: FieldIssue[Field]): Unit = FormFocus.focus(state.now().focusId(issue.field))

    /** The checklist stays neutral until a submit was attempted, then every issue is shown as an error. */
    val emphasized: Signal[FieldIssue[Field] => Boolean] = touch.submittedSignal.map(attempted => _ => attempted)

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
      if !submitting.now() && !gone.now() then
        val current = state.now()
        val ctx = context.now()
        current.validated(ctx) match
          case Left(_) =>
            touch.attemptSubmit()
            current.issues(ctx).headOption.foreach(reveal)
          // An unchanged note has nothing to save: sending it again would only void and record the same loads.
          case Right(_) if current.editingId.isDefined && !tracker.isDirtyNow => onExit()
          case Right(request) =>
            submitting.set(true)
            error.set(None)
            val effect = current.editingId.fold(ApiClient.createDeliveryNote(request))(ApiClient.updateDeliveryNote(_, request))
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(saved) =>
                  touch.reset()
                  Notifications.success(if current.editingId.isDefined then s"DDT n. ${request.number} aggiornato"
                  else s"DDT n. ${request.number} registrato")
                  AppBus.mutatedWarehouse()
                  onSaved(saved.id)
                case Left(err) =>
                  showError(error, "Registrazione DDT")(err)
                  FormFocus.focus(submitErrorId)
            }
        end match

    def update(key: Int)(patch: Row => Row): Unit = state.update(_.updateRow(key)(patch))

    /** "+ Riga": a blank line at the end, its material search focused. */
    def addRow(): Unit =
      val key = state.now().nextKey
      state.update(_.addRow)
      FormFocus.focus(DeliveryNoteForm.fieldId(Field.Row(key, RowField.Material)))

    /** Removes a line; the focus moves to "+ Riga" (the removed button is gone). */
    def removeRow(key: Int): Unit =
      state.update(_.removeRow(key))
      FormFocus.focus(DeliveryNoteForm.addRowId)

    def renderRow(key: Int, rowSignal: Signal[Row]): HtmlElement =
      // What the row shows besides its inputs: the selected option, the price note, and the line total.
      val view = rowSignal.combineWith(state.signal, context.signal).map { (row, s, ctx) =>
        (s.optionOf(row, ctx), s.priceNote(row, ctx), s.lineTotal(row, ctx))
      }
      val selected = view.map(_._1)
      def rowSpec(labelText: String, rowField: RowField): FieldSpec = spec(labelText, Field.Row(key, rowField), true)
      div(
        // Up to xl (the main column is narrow next to the sidebar): the material on its own row, then quantity | cost and total | remove in two
        // columns; from xl: one row.
        cls := "grid grid-cols-2 gap-2 rounded-md border border-slate-200 bg-white p-2 xl:grid-cols-[minmax(0,1fr)_7.5rem_9rem_6.5rem_auto] xl:items-start",
        div(
          cls := "col-span-2 min-w-0 xl:col-span-1",
          field(rowSpec("Materiale", RowField.Material))(bind =>
            searchableSelect(
              rowSignal.map(_.choice),
              Observer[String](v => state.update(_.choose(key, v, context.now()))),
              lineOptions,
              "Codice, MPN o descrizione…",
              20,
              bind,
            ),
          ),
          child.maybe <-- selected.map(_.exists(_.isNewMpn)).distinct.map { isNew =>
            Option.when(isNew)(
              div(
                cls := "mt-2",
                field(
                  rowSpec("Codice nuovo MPN", RowField.NewMpnCode)
                    .copy(hintText = Some("Il materiale non ha MPN di questo fornitore: viene aggiunto con questo codice.")),
                )(bind =>
                  textInput(rowSignal.map(_.newMpnCode), Observer[String](v => update(key)(_.copy(newMpnCode = v))), "Es. AC-152-NAVY").amend(bind),
                ),
              ),
            )
          },
        ),
        field(rowSpec("Quantità", RowField.Quantity))(bind =>
          div(
            textInput(rowSignal.map(_.quantity), Observer[String](v => update(key)(_.copy(quantity = v))), "25,5")
              .amend(inputMode := "decimal", bind),
            div(
              cls := "mt-1 text-xs text-slate-400",
              child.text <-- selected.map(
                _.fold("")(o => WarehouseFormats.unitLabel(o.unit) + (if MaterialDrafts.isPieces(o.unit) then " (interi)" else "")),
              ),
            ),
          ),
        ),
        field(rowSpec("Costo unit. (€)", RowField.UnitCost))(bind =>
          div(
            textInput(rowSignal.map(_.unitCost), Observer[String](v => state.update(_.setCost(key, v))), "")
              .amend(
                inputMode := "decimal",
                placeholder <-- selected.map(_.flatMap(_.expectedUnitCost).fold("3,50")(DeliveryNoteForm.costText)),
                bind,
              ),
            div(cls := "mt-1 text-xs text-slate-400", child.text <-- view.map(_._2.getOrElse(""))),
          ),
        ),
        div(
          span(cls := "mb-1 block text-xs font-medium text-slate-500", "Totale"),
          div(cls := "py-1.5 text-sm font-medium tabular-nums text-slate-800", child.text <-- view.map(_._3.fold("—")(WarehouseFormats.formatEuro))),
        ),
        button(
          tpe := "button",
          cls := s"$btnDanger min-h-10 justify-center self-end xl:mt-5 xl:min-h-0 xl:self-start",
          aria.label := "Rimuovi riga",
          "Rimuovi",
          onClick --> (_ => removeRow(key)),
        ),
      )
    end renderRow

    val linesId = DeliveryNoteForm.fieldId(Field.Lines)
    val linesError = touch.visible(linesId, rawError(Field.Lines))
    // A def: it shows twice (sidebar from lg, after the sections below), and an element can only be mounted once.
    def editHint: Modifier[HtmlElement] =
      Option.when(editing)(hint("Salvando, i carichi registrati con questo DDT vengono annullati e registrati di nuovo con le righe aggiornate."))

    val documentSection =
      formSection(1, "Documento", "Data, numero e fornitore del DDT.", sectionStatus(_.count(isHeaderIssue)))(
        div(
          cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2 xl:grid-cols-4",
          field(spec("Data DDT", Field.Date, true))(bind =>
            textInput(state.signal.map(_.date), Observer[String](v => state.update(_.copy(date = v))), "", "date").amend(maxAttr := today, bind),
          ),
          field(spec("Numero DDT", Field.Number, true))(bind =>
            textInput(state.signal.map(_.number), Observer[String](v => state.update(_.copy(number = v))), "Es. 1234/A").amend(bind),
          ),
          div(
            cls := "sm:col-span-2",
            field(spec("Fornitore", Field.Supplier, true))(bind =>
              searchableSelect(
                state.signal.map(_.supplierId),
                Observer[String](v => state.update(_.withSupplier(v, context.now()))),
                supplierOptions,
                "Cerca un fornitore…",
                8,
                bind,
              ),
            ),
          ),
          div(
            cls := "sm:col-span-2 xl:col-span-4",
            field(FieldSpec("Note", "ddt-note"))(bind =>
              textInput(state.signal.map(_.note), Observer[String](v => state.update(_.copy(note = v))), "Opzionale").amend(bind),
            ),
          ),
        ),
      )

    val linesSection =
      formSection(
        2,
        "Righe",
        "Materiali, quantità e costi. Le righe vuote vengono ignorate se ce n'è almeno una compilata.",
        sectionStatus(_.count(!isHeaderIssue(_))),
      )(
        div(
          idAttr := linesId,
          cls := "space-y-2",
          child.maybe <-- state.signal
            .map(_.supplierId.trim.nn.isEmpty)
            .distinct
            .map(noSupplier =>
              Option.when(noSupplier)(hint("Seleziona il fornitore: i materiali si cercano tra i suoi MPN per codice, MPN o descrizione.")),
            ),
          div(cls := "space-y-2", children <-- state.signal.map(_.rows).split(_.key)((key, _, rowSignal) => renderRow(key, rowSignal))),
          div(aria.live := "polite", cls := "text-xs font-medium text-rose-600", child.text <-- linesError.map(_.getOrElse(""))),
          div(
            cls := "flex flex-wrap items-center justify-between gap-2",
            button(
              tpe := "button",
              idAttr := DeliveryNoteForm.addRowId,
              cls := s"$btnSmall min-h-10 md:min-h-0",
              "+ Riga",
              onClick --> (_ => addRow()),
            ),
            div(cls := "text-sm text-slate-600", "Totale DDT: ", span(cls := "font-semibold tabular-nums text-slate-800", child.text <-- totalText)),
          ),
        ),
        child.maybe <-- error.signal.map(_.map(err => div(idAttr := submitErrorId, errorBanner(err)))),
      )

    def issuesHeading: HtmlElement =
      div(cls := subheadingCls, child.text <-- issues.map(list => if list.isEmpty then "Da completare" else s"Da completare (${list.size})"))

    // Below lg the sidebar is hidden: what it lists, the hint, and "Annulla" follow the sections (the primary action is in the bottom bar).
    val mobileFooter =
      div(
        cls := "space-y-3 lg:hidden",
        card(cls := "space-y-2 p-4", issuesHeading, issueList(issues, _.message, emphasized, reveal)),
        editHint,
        div(
          cls := "flex justify-end border-t border-slate-100 pt-3",
          button(tpe := "button", cls := s"$btnGhost min-h-10 justify-center", "Annulla", onClick --> (_ => leave())),
        ),
      )

    val sidebar =
      card(
        cls := "space-y-4 p-4",
        h2(cls := "text-sm font-semibold text-slate-800", "Riepilogo"),
        div(
          cls := "space-y-1.5",
          summaryRow("Data", state.signal.map(s => orDash(IsoDay.display(s.date.trim.nn)))),
          summaryRow("Numero", state.signal.map(s => orDash(s.number.trim.nn))),
          summaryRow(
            "Fornitore",
            state.signal
              .map(_.supplierId)
              .combineWith(supplierOptions)
              .map((id, options) => DeliveryNoteForm.supplierName(id, options).getOrElse("—")),
          ),
          summaryRow("Righe", state.signal.map(_.filledCount.toString)),
        ),
        div(
          cls := "flex items-baseline justify-between gap-3 border-t border-slate-100 pt-3",
          span(cls := "text-sm text-slate-500", "Totale DDT"),
          span(aria.live := "polite", cls := "text-lg font-semibold tabular-nums text-slate-900", child.text <-- totalText),
        ),
        div(cls := "space-y-2 border-t border-slate-100 pt-3", issuesHeading, issueList(issues, _.message, emphasized, reveal)),
        editHint,
        dirtyNote(tracker.isDirty),
        formActions(submitLabel, busyLabel, submitting.signal, blocked, Some(leave), () => submit()),
      )

    val mobileBar =
      mobileSubmitBar(
        List(
          div(cls := "truncate text-xs text-slate-500", "Totale DDT · ", child.text <-- linesText),
          div(cls := "truncate text-sm font-semibold tabular-nums text-slate-900", child.text <-- totalText),
        ),
        issueBadge(issues.map(_.size).distinct, () => state.now().issues(context.now()).headOption.foreach(reveal)),
        submitLabel,
        busyLabel,
        submitting.signal,
        submitting.signal.combineWith(blocked).map((busy, reason) => busy || reason.isDefined),
        () => submit(),
      )

    val goneNote: Modifier[HtmlElement] = target match
      case Target.New => emptyMod
      case Target.Edit(note) =>
        child.maybe <-- gone.signal.map(isGone =>
          Option.when(isGone)(
            warningNote(
              s"Il DDT n. ${note.number} è stato annullato, forse da un altro utente: le modifiche non possono essere salvate.",
            ),
          ),
        )

    formPage(header(target, leave, goneNote))(
      DraftGuard.trackGone(cancelled, gone)(() => ()),
      // Leaving the page (top navigation, a tab switch, closing the tab) asks while dirty, except once the note cannot be saved anyway.
      draft.bind(leaveTitle, leaveMessage, () => gone.now()),
      draft.dialog,
      summaryLayout(div(cls := "space-y-6", documentSection, linesSection, mobileFooter), sidebar, mobileBar),
    )
  end form
end DeliveryNoteEditor
