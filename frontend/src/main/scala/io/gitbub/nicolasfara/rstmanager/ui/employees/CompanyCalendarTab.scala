package io.gitbub.nicolasfara.rstmanager.ui.employees

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, Components, DraftGuard, FormFocus, FormTouch, Notifications, Pagination, Paging, SubView }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, FieldIssue }
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.BusinessDay

/**
 * Company calendar ("Calendario aziendale"): the closures in which nobody works, either a period (first and last day inclusive) or a day recurring
 * every year. A closure wins over any hours override, and every change triggers a planning recalculation.
 *
 * The list is full width; "+ Nuova chiusura" and "Modifica" open the closure form page in its place ([[SubView]]), guarded by a [[DraftGuard]].
 */
object CompanyCalendarTab:

  /** What the closure form page shows: a new closure, or the stored one the user clicked. */
  private enum ClosureTarget derives CanEqual:
    case New
    case Edit(closure: ClosureResponse)

  private val explanation =
    "Nei giorni di chiusura nessun dipendente è disponibile, anche con un override «Giorno lavorativo». La pianificazione si ricalcola da sola."

  private val listErrorId = "closures-list-error"

  def apply(): HtmlElement =
    val view = SubView[ClosureTarget]()
    val pageError = Var(Option.empty[ApiError])
    val pendingDelete = Var(Option.empty[ClosureResponse])

    val data = loadable(AppBus.calendarTicks)(() => ApiClient.listClosures())
    val periodsPager = Pagination(Paging.defaultPageSize)
    val annualsPager = Pagination(Paging.defaultPageSize)

    def open(target: ClosureTarget): Unit =
      pageError.set(None)
      view.open(target)

    def delete(c: ClosureResponse): Unit =
      ApiClient.deleteClosure(c.id).foreach {
        case Right(_) =>
          Notifications.success(s"Chiusura ${c.reason} eliminata")
          AppBus.mutatedCalendar()
        case Left(err) =>
          showBannerError(pageError, "Eliminazione chiusura aziendale")(err)
          FormFocus.focus(listErrorId)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(c => s"Eliminare la chiusura «${c.reason}»?")),
        message = pendingDelete.signal.map(
          _.fold("")(c => s"${CompanyCalendarView.describe(c)}: quei giorni tornano lavorativi. L'operazione non è reversibile."),
        ),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { c => delete(c); pendingDelete.set(None) },
      )

    /** The form page of `target`: one "Chiusura" section, the sticky action bar, the discard guard. Mounted fresh for each opening. */
    def formView(target: ClosureTarget): HtmlElement =
      val (initial, editedName) = target match
        case ClosureTarget.New => (ClosureForm.empty, None)
        case ClosureTarget.Edit(c) => (ClosureForm.fromResponse(c), Some(c.reason))
      val form = Var(initial)
      val touch = FormTouch()
      val submitting = Var(false)
      val submitError = Var(Option.empty[ApiError])
      // The baseline is the content at construction: the blank form, or the stored closure.
      val draft = DraftGuard[ClosureForm.State](() => form.now(), s => DraftGuard.discardTitle(s.editingId.map(_ => s.reason), "la nuova chiusura"))

      // The closure being edited disappeared from a successful reload (deleted elsewhere): it cannot be saved, and leaving does not ask.
      val gone = Var(false)

      val leave: () => Unit = () => if gone.now() then view.closeIfShowing(target) else draft.discardThen(() => view.closeIfShowing(target))

      // Always clickable: an invalid form reveals every error and focuses the first one; a pristine edit leaves without a request.
      def submit(): Unit =
        val current = form.now()
        if !submitting.now() && !gone.now() then
          if current.editingId.isDefined && !draft.isDirtyNow then view.closeIfShowing(target)
          else
            current.firstIssueOnScreen match
              case Some(first) =>
                touch.attemptSubmit()
                FormFocus.focus(ClosureForm.fieldId(first.field))
              case None =>
                submitting.set(true)
                submitError.set(None)
                val effect = current.editingId match
                  case Some(id) => ApiClient.updateClosure(id, current.request)
                  case None => ApiClient.createClosure(current.request)
                effect.foreach { result =>
                  submitting.set(false)
                  result match
                    case Right(_) =>
                      draft.markClean()
                      Notifications.success(s"Chiusura ${current.reason.trim} ${if current.editingId.isDefined then "salvata" else "creata"}")
                      AppBus.mutatedCalendar()
                      view.closeIfShowing(target)
                    case Left(err) =>
                      showError(submitError, "Salvataggio chiusura aziendale")(err)
                      FormFocus.focus(ClosureForm.submitErrorId)
                }
        end if
      end submit

      val issues: Signal[List[FieldIssue[ClosureForm.Field]]] = form.signal.map(_.issues)

      def spec(labelText: String, field: ClosureForm.Field): FieldSpec =
        touch.spec(labelText, ClosureForm.fieldId(field), true, issues.map(_.messageFor(field)).distinct)

      def textField(labelText: String, field: ClosureForm.Field, placeholderText: String, kind: String)(
          get: ClosureForm.State => String,
          set: (ClosureForm.State, String) => ClosureForm.State,
      ): HtmlElement =
        Components.field(spec(labelText, field))(bind =>
          textInput(form.signal.map(get), Observer[String](v => form.update(set(_, v))), placeholderText, kind).amend(bind),
        )

      def kindFields(annual: Boolean): HtmlElement =
        if annual then
          div(
            cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
            textField("Giorno", ClosureForm.Field.Day, "1–31", "number")(_.day, (s, v) => s.copy(day = v)),
            Components.field(spec("Mese", ClosureForm.Field.Month))(bind =>
              staticSelect(form.signal.map(_.month), Observer[String](v => form.update(_.copy(month = v))), ClosureForm.monthOptions).amend(bind),
            ),
          )
        else
          div(
            cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
            textField("Dal", ClosureForm.Field.StartDate, "", "date")(_.startDate, (s, v) => s.copy(startDate = v)),
            textField("Al (incluso)", ClosureForm.Field.EndDate, "", "date")(_.endDate, (s, v) => s.copy(endDate = v)),
          )

      val dirty: Signal[Boolean] = form.signal.combineWith(gone.signal).map((_, isGone) => !isGone && draft.isDirtyNow)
      val creating = initial.editingId.isEmpty

      formPage(
        PageHeader(
          backLabel = "← Torna al calendario",
          onBack = leave,
          title = Signal.fromValue(editedName.fold("Nuova chiusura")(name => s"Modifica chiusura «$name»")),
          context = List("Dipendenti", "Calendario aziendale"),
          notes = child.maybe <-- gone.signal.map(isGone =>
            Option.when(isGone)(
              warningNote(
                s"La chiusura «${editedName.getOrElse("")}» non esiste più: potrebbe essere stata eliminata da un altro utente. Le modifiche non possono essere salvate.",
              ),
            ),
          ),
          initialFocus = if creating then PageFocus.FirstField else PageFocus.Title,
        ),
      )(
        draft.bind,
        draft.dialog,
        DraftGuard.trackGone(data, initial.editingId, _.id, gone)(() => draft.markClean()),
        formColumn(
          formSection("Chiusura", "I giorni in cui l'azienda è chiusa: un periodo, oppure un giorno che si ripete ogni anno.")(
            Components.field(FieldSpec("Tipo", "closure-kind"))(bind =>
              segmented(
                form.signal.map(_.kind),
                Observer[String](v => form.update(_.copy(kind = v))),
                ClosureForm.kindOptions,
                List(bind, aria.label := "Tipo di chiusura"),
              ),
            ),
            // `distinct`: the date/day inputs are rebuilt only when the kind changes, not on every keystroke.
            child <-- form.signal.map(_.isAnnual).distinct.map(kindFields),
            child.maybe <-- form.signal
              .map(_.isAnnual)
              .distinct
              .map(annual => Option.when(annual)(hint("Il 29 febbraio vale solo negli anni bisestili."))),
            textField("Motivo", ClosureForm.Field.Reason, "Es. Ferragosto", "text")(_.reason, (s, v) => s.copy(reason = v)),
            hint(explanation),
            child.maybe <-- submitError.signal.map(_.map(err => div(idAttr := ClosureForm.submitErrorId, errorBanner(err)))),
          ),
          stickyActionBar(
            dirtyNote(dirty),
            formActionsInline(
              if creating then "Crea chiusura" else "Salva modifiche",
              if creating then "Creazione…" else "Salvataggio…",
              submitting.signal,
              gone.signal.map(isGone => Option.when(isGone)("La chiusura non esiste più.")),
              Some(leave),
              () => submit(),
            ),
          ),
        ),
      )
    end formView

    def row(c: ClosureResponse, dimmed: Boolean): HtmlElement =
      div(
        cls := s"flex flex-wrap items-center justify-between gap-3 px-4 py-2${if dimmed then " opacity-60" else ""}",
        div(
          cls := "min-w-0",
          div(cls := "font-medium text-slate-800", c.reason),
          div(cls := "text-xs tabular-nums text-slate-500", CompanyCalendarView.describe(c), if dimmed then " · conclusa" else ""),
        ),
        roleGated(Role.Admin)(
          div(
            cls := s"flex shrink-0 gap-2 $touchChildrenCls",
            button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => open(ClosureTarget.Edit(c)))),
            button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(c)))),
          ),
        ),
      )

    def section(title: String, emptyText: String, closures: List[ClosureResponse], pager: Pagination)(
        dimmed: ClosureResponse => Boolean,
    ): HtmlElement =
      div(
        div(cls := "border-b border-slate-100 bg-slate-50 px-4 py-2 text-xs font-semibold uppercase tracking-wide text-slate-500", title),
        if closures.isEmpty then div(cls := "px-4 py-3 text-xs text-slate-400", emptyText)
        else pager.view(closures)(shown => div(cls := "divide-y divide-slate-100", shown.map(c => row(c, dimmed(c))))),
      )

    div(
      div(
        cls := "space-y-4",
        view.listMods,
        child.maybe <-- pageError.signal.map(_.map(err => errorBanner(err, () => pageError.set(None)).amend(idAttr := listErrorId))),
        card(
          cls := "overflow-hidden",
          div(
            cls := "flex flex-wrap items-center gap-3 border-b border-slate-100 p-3",
            p(cls := "min-w-0 flex-1 text-xs text-slate-500", explanation),
            roleGated(Role.Admin)(
              button(
                tpe := "button",
                cls := s"$btnPrimary min-h-10 shrink-0 md:min-h-0",
                "+ Nuova chiusura",
                onClick --> (_ => open(ClosureTarget.New)),
              ),
            ),
          ),
          renderResult(data) { closures =>
            if closures.isEmpty then div(cls := "p-4", emptyState("Nessuna chiusura aziendale."))
            else
              val today = BusinessDay.today()
              div(
                section("Periodi", "Nessun periodo di chiusura.", CompanyCalendarView.periods(closures, today), periodsPager)(
                  CompanyCalendarView.isPast(_, today),
                ),
                section("Ricorrenze annuali", "Nessuna ricorrenza annuale.", CompanyCalendarView.annuals(closures), annualsPager)(_ => false),
              )
          },
        ),
      ),
      view.page(formView),
      confirmDeleteModal,
    )
  end apply
end CompanyCalendarTab
