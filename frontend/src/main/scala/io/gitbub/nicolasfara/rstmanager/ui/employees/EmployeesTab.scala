package io.gitbub.nicolasfara.rstmanager.ui.employees

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{
  AppBus,
  Components,
  DraftGuard,
  FormFocus,
  FormTouch,
  Formats,
  ListSearch,
  Notifications,
  Pagination,
  Paging,
  SubView,
}
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, FieldIssue }

/**
 * Employee registry: a full-width list, and one form page for creating or editing an employee ([[SubView]]). The page edits the registry data and the
 * hours overrides (ferie e variazioni orarie) together: one "Salva modifiche" sends the whole record.
 */
object EmployeesTab:

  /** What the employee form page shows: a new employee, or the stored one the user clicked (`focusOverrides`: opened from «Override»). */
  private enum EmployeeTarget derives CanEqual:
    case New
    case Edit(employee: EmployeeResponse, focusOverrides: Boolean)

  private val listErrorId = "emp-list-error"

  /** The inline error of an override typed but not added, under "+ Aggiungi". */
  private val pendingOverrideErrorId = "emp-ovr-pending-error"

  private def chipRemoveId(position: Int): String = s"emp-ovr-remove-$position"

  private def describeContract(c: EmployeeContractDto): String = c.kind match
    case "full_time" => s"Tempo pieno · dal ${Formats.date(c.startDate)}"
    case "fixed_term" => s"Determinato · ${Formats.date(c.startDate)} → ${c.endDate.map(Formats.date).getOrElse("?")}"
    case "part_time" => s"Part-time · ${c.weeklyHours.getOrElse(0)}h/sett · dal ${Formats.date(c.startDate)}"
    case other => other

  private def fullName(emp: EmployeeResponse): String = s"${emp.name} ${emp.surname}".trim.nn

  def apply(): HtmlElement =
    val view = SubView[EmployeeTarget]()
    val pageError = Var(Option.empty[ApiError])
    val pendingDelete = Var(Option.empty[EmployeeResponse])

    val data = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)

    val closures: Signal[List[ClosureResponse]] = loadable(AppBus.calendarTicks)(() => ApiClient.listClosures()).map {
      case Some(Right(list)) => list
      case _ => Nil
    }

    def open(target: EmployeeTarget): Unit =
      pageError.set(None)
      view.open(target)

    def deleteEmployee(emp: EmployeeResponse): Unit =
      ApiClient.deleteEmployee(emp.id).foreach {
        case Right(_) =>
          Notifications.success(s"Dipendente ${fullName(emp)} eliminato")
          AppBus.mutatedEmployees()
        case Left(err) =>
          showBannerError(pageError, "Eliminazione dipendente")(err)
          FormFocus.focus(listErrorId)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(e => s"Eliminare il dipendente «${fullName(e)}»?")),
        message = Signal.fromValue("Il dipendente viene rimosso dall'anagrafica. L'operazione non è reversibile."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { e => deleteEmployee(e); pendingDelete.set(None) },
      )

    /** The form page of `target`: Anagrafica / Contratto / Capacità / Override, the sticky action bar, the discard guard. Mounted per opening. */
    def formView(target: EmployeeTarget): HtmlElement =
      val (initial, editedName, initialFocus) = target match
        case EmployeeTarget.New => (EmployeeForm.empty, None, PageFocus.FirstField)
        case EmployeeTarget.Edit(emp, focusOverrides) =>
          (
            EmployeeForm.fromResponse(emp),
            Some(fullName(emp)),
            if focusOverrides then PageFocus.Element(EmployeeForm.overridesSectionId) else PageFocus.Title,
          )
      val creating = initial.editingId.isEmpty
      val form = Var(initial)
      val touch = FormTouch()
      // The override draft has its own tracker: its errors show after leaving one of its fields or after a refused "+ Aggiungi".
      val overrideTouch = FormTouch()
      val submitting = Var(false)
      val submitError = Var(Option.empty[ApiError])
      // The baseline is the content at construction (the blank form, or the stored employee); a blank override draft is not an edit.
      val draft = DraftGuard[EmployeeForm.State](
        () => form.now().comparable,
        s => DraftGuard.discardTitle(s.editingId.map(_ => s"${s.name} ${s.surname}".trim.nn), "il nuovo dipendente"),
      )

      // The employee being edited disappeared from a successful reload (deleted elsewhere): it cannot be saved, and leaving does not ask.
      val gone = Var(false)

      val leave: () => Unit = () => if gone.now() then view.closeIfShowing(target) else draft.discardThen(() => view.closeIfShowing(target))

      // Always clickable: an invalid form reveals every error and focuses the first one; a pristine edit leaves without a request.
      def submit(): Unit =
        val current = form.now()
        if !submitting.now() && !gone.now() then
          if !creating && !draft.isDirtyNow then view.closeIfShowing(target)
          else
            current.firstIssueOnScreen match
              case Some(first) =>
                touch.attemptSubmit()
                FormFocus.focus(EmployeeForm.fieldId(first.field))
              case None =>
                submitting.set(true)
                submitError.set(None)
                val effect = current.editingId match
                  case Some(id) => ApiClient.updateEmployee(id, current.request)
                  case None => ApiClient.createEmployee(current.request)
                effect.foreach { result =>
                  submitting.set(false)
                  result match
                    case Right(saved) =>
                      draft.markClean()
                      Notifications.success(s"Dipendente ${fullName(saved)} ${if creating then "creato" else "salvato"}")
                      AppBus.mutatedEmployees()
                      view.closeIfShowing(target)
                    case Left(err) =>
                      showError(submitError, "Salvataggio dipendente")(err)
                      FormFocus.focus(EmployeeForm.submitErrorId)
                }
        end if
      end submit

      val issues: Signal[List[FieldIssue[EmployeeForm.Field]]] = form.signal.map(_.issues)

      /** A text field of the employee form; `required` fields get the `*` and an inline error gated by [[FormTouch]]. */
      def textField(
          labelText: String,
          field: EmployeeForm.Field,
          get: EmployeeForm.State => String,
          set: (EmployeeForm.State, String) => EmployeeForm.State,
          kind: String,
          hintText: Option[String],
      ): HtmlElement =
        val error = issues.map(_.messageFor(field)).distinct
        val id = EmployeeForm.fieldId(field)
        val spec = hintText.fold(touch.spec(labelText, id, true, error))(text => touch.spec(labelText, id, true, text, error))
        Components.field(spec)(bind => textInput(form.signal.map(get), Observer[String](v => form.update(set(_, v))), "", kind).amend(bind))

      // ---- Override section ------------------------------------------------------------------------

      val overrideDraft: Signal[EmployeeForm.OverrideDraft] = form.signal.map(_.overrideDraft).distinct

      def updateDraft(patch: EmployeeForm.OverrideDraft => EmployeeForm.OverrideDraft): Unit =
        form.update(s => s.copy(overrideDraft = patch(s.overrideDraft)))

      val overrideIssues: Signal[List[FieldIssue[EmployeeForm.OverrideField]]] = form.signal.map(_.overrideIssues)

      def overrideField(labelText: String, field: EmployeeForm.OverrideField, required: Boolean, placeholderText: String, kind: String)(
          get: EmployeeForm.OverrideDraft => String,
          set: (EmployeeForm.OverrideDraft, String) => EmployeeForm.OverrideDraft,
      ): HtmlElement =
        val spec = overrideTouch.spec(labelText, EmployeeForm.overrideFieldId(field), required, overrideIssues.map(_.messageFor(field)).distinct)
        Components.field(spec)(bind =>
          textInput(overrideDraft.map(get), Observer[String](v => updateDraft(set(_, v))), placeholderText, kind).amend(bind),
        )

      def addOverride(): Unit =
        form.now().addOverride match
          case Some(next) =>
            form.set(next)
            overrideTouch.reset()
          case None =>
            overrideTouch.attemptSubmit()
            form.now().overrideIssues.headOption.foreach(first => FormFocus.focus(EmployeeForm.overrideFieldId(first.field)))

      def clearDraft(): Unit =
        updateDraft(d => EmployeeForm.OverrideDraft.empty.copy(kind = d.kind))
        overrideTouch.reset()

      /** Removes the chip at display `position`; the focus moves to the chip now there (or the last one), else to "+ Aggiungi". */
      def removeOverride(index: Int, position: Int): Unit =
        form.update(_.removeOverride(index))
        val remaining = form.now().overrides.size
        FormFocus.focus(if remaining == 0 then EmployeeForm.overrideAddId else chipRemoveId(position.min(remaining - 1)))

      // Informational only: an override over a company closure is still saved (the closure already makes those days non-working).
      val closureNote: Signal[Option[HtmlElement]] =
        overrideDraft.combineWith(closures).map { (d, list) =>
          val hits =
            if d.isVacation then CompanyCalendarView.closuresDuring(d.start, d.end, list)
            else CompanyCalendarView.closureOn(d.day, list).toList
          Option
            .when(hits.nonEmpty) {
              val names = hits.map(c => s"${c.reason} (${CompanyCalendarView.describe(c)})").mkString(", ")
              if d.isVacation then s"Già coperto da chiusura aziendale: $names."
              else s"Giorno di chiusura aziendale: $names. Nessuno lavora, anche con questo override."
            }
            .map(text => div(role := "note", cls := "rounded-md border border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-800", text))
        }

      val chips: HtmlElement =
        div(
          aria.live := "polite",
          children <-- form.signal.map(_.overridesByDay).distinct.map { list =>
            if list.isEmpty then List(p(cls := "text-sm text-slate-500", "Nessun override."))
            else
              List(
                ul(
                  aria.label := "Override inseriti",
                  cls := "flex flex-wrap gap-2",
                  list.zipWithIndex.map { case ((o, index), position) =>
                    val text = EmployeeForm.describeOverride(o)
                    li(
                      cls := "inline-flex items-center gap-1 rounded-full border border-slate-200 bg-white py-0.5 pr-1 pl-3 text-sm text-slate-700",
                      span(text),
                      button(
                        tpe := "button",
                        idAttr := chipRemoveId(position),
                        cls := "inline-flex h-8 w-8 items-center justify-center rounded-full text-slate-400 hover:bg-rose-50 hover:text-rose-600 md:h-6 md:w-6",
                        aria.label := s"Rimuovi override $text",
                        title := "Rimuovi override",
                        span(aria.hidden := true, "✕"),
                        onClick --> (_ => removeOverride(index, position)),
                      ),
                    )
                  },
                ),
              )
          },
        )

      val pendingError: Signal[Option[String]] =
        touch.visible(EmployeeForm.overrideAddId, issues.map(_.messageFor(EmployeeForm.Field.PendingOverride)).distinct)

      val addForm: HtmlElement =
        div(
          cls := "space-y-4 rounded-lg border border-slate-200 bg-slate-50 p-3",
          subheading("Aggiungi override"),
          Components.field(FieldSpec("Tipo", "emp-ovr-kind"))(bind =>
            segmented(
              overrideDraft.map(_.kind),
              Observer[String](v => updateDraft(_.copy(kind = v))),
              EmployeeForm.overrideKinds,
              List(bind, aria.label := "Tipo di override"),
            ),
          ),
          // `distinct`: the inputs are rebuilt only when the kind changes, not on every keystroke (that would drop the input focus).
          child <-- overrideDraft.map(_.isVacation).distinct.map { vacation =>
            if vacation then
              div(
                cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
                overrideField("Da", EmployeeForm.OverrideField.Start, true, "", "date")(_.start, (d, v) => d.copy(start = v)),
                overrideField("A (incluso)", EmployeeForm.OverrideField.End, true, "", "date")(_.end, (d, v) => d.copy(end = v)),
              )
            else
              div(
                cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-3",
                overrideField("Giorno", EmployeeForm.OverrideField.Day, true, "", "date")(_.day, (d, v) => d.copy(day = v)),
                overrideField("Ore", EmployeeForm.OverrideField.Hours, true, "0–24", "number")(_.hours, (d, v) => d.copy(hours = v)),
                Components.field(FieldSpec("Motivo", "emp-ovr-reason"))(bind =>
                  textInput(overrideDraft.map(_.reason), Observer[String](v => updateDraft(_.copy(reason = v))), "Opzionale").amend(bind),
                ),
              )
          },
          child.maybe <-- closureNote,
          div(
            cls := s"flex flex-wrap items-center gap-2 $touchChildrenCls",
            button(
              tpe := "button",
              idAttr := EmployeeForm.overrideAddId,
              cls := btnSmall,
              aria.describedBy := pendingOverrideErrorId,
              "+ Aggiungi",
              onClick --> (_ => addOverride()),
            ),
            child.maybe <-- overrideDraft
              .map(!_.isBlank)
              .distinct
              .map(typed => Option.when(typed)(button(tpe := "button", cls := btnSmall, "Svuota", onClick --> (_ => clearDraft())))),
          ),
          fieldError(pendingOverrideErrorId, pendingError),
        )

      // ---- Page ------------------------------------------------------------------------------------

      val dirty: Signal[Boolean] = form.signal.combineWith(gone.signal).map((_, isGone) => !isGone && draft.isDirtyNow)
      val saveLabel = if creating then "Crea dipendente" else "Salva modifiche"

      formPage(
        PageHeader(
          backLabel = "← Torna ai dipendenti",
          onBack = leave,
          title = Signal.fromValue(editedName.fold("Nuovo dipendente")(name => s"Modifica dipendente «$name»")),
          context = List("Dipendenti"),
          notes = child.maybe <-- gone.signal.map(isGone =>
            Option.when(isGone)(
              warningNote(
                s"Il dipendente «${editedName.getOrElse("")}» non esiste più: potrebbe essere stato eliminato da un altro utente. Le modifiche non possono essere salvate.",
              ),
            ),
          ),
          initialFocus = initialFocus,
        ),
      )(
        draft.bind,
        draft.dialog,
        DraftGuard.trackGone(data, initial.editingId, _.id, gone)(() => draft.markClean()),
        formColumn(
          formSection("Anagrafica", "")(
            div(
              cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
              textField("Nome", EmployeeForm.Field.Name, _.name, (s, v) => s.copy(name = v), "text", None),
              textField("Cognome", EmployeeForm.Field.Surname, _.surname, (s, v) => s.copy(surname = v), "text", None),
            ),
          ),
          formSection("Contratto", "")(
            Components.field(FieldSpec("Tipo di contratto", "emp-contract-kind"))(bind =>
              segmented(
                form.signal.map(_.contractKind),
                Observer[String](v => form.update(_.copy(contractKind = v))),
                EmployeeForm.contractOptions,
                List(bind, aria.label := "Tipo di contratto"),
              ),
            ),
            div(
              cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
              textField("Inizio", EmployeeForm.Field.StartDate, _.startDate, (s, v) => s.copy(startDate = v), "date", None),
              // Rebuilt only when the contract kind changes, not on every keystroke.
              child <-- form.signal.map(_.contractKind).distinct.map {
                case "fixed_term" =>
                  textField("Fine (inclusa)", EmployeeForm.Field.EndDate, _.endDate, (s, v) => s.copy(endDate = v), "date", None)
                case "part_time" =>
                  textField(
                    "Ore/sett. da contratto",
                    EmployeeForm.Field.WeeklyHours,
                    _.weeklyHours,
                    (s, v) => s.copy(weeklyHours = v),
                    "number",
                    None,
                  )
                case _ => emptyNode
              },
            ),
          ),
          formSection("Capacità", "")(
            div(
              cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
              textField(
                "Budget ore/sett.",
                EmployeeForm.Field.Budget,
                _.budget,
                (s, v) => s.copy(budget = v),
                "number",
                Some("Ore disponibili per la pianificazione in una settimana normale."),
              ),
            ),
          ),
          formSection(
            "Override",
            s"Ferie e variazioni dell'orario di singoli giorni. Si salvano con il dipendente («$saveLabel»).",
          )(
            idAttr := EmployeeForm.overridesSectionId,
            chips,
            addForm,
            child.maybe <-- submitError.signal.map(_.map(err => div(idAttr := EmployeeForm.submitErrorId, errorBanner(err)))),
          ),
          stickyActionBar(
            dirtyNote(dirty),
            formActionsInline(
              saveLabel,
              if creating then "Creazione…" else "Salvataggio…",
              submitting.signal,
              gone.signal.map(isGone => Option.when(isGone)("Il dipendente non esiste più.")),
              Some(leave),
              () => submit(),
            ),
          ),
        ),
      )
    end formView

    def employeeRow(emp: EmployeeResponse): HtmlElement =
      div(
        cls := "flex flex-wrap items-start justify-between gap-3 px-4 py-3",
        div(
          cls := "min-w-0",
          div(cls := "font-medium text-slate-800", fullName(emp)),
          div(cls := "text-xs text-slate-500", describeContract(emp.contract)),
          div(
            cls := "mt-0.5 text-xs text-slate-400",
            s"Budget ${emp.budgetWeeklyHours}h/sett · ${emp.overrides.size} override",
          ),
        ),
        roleGated(Role.Admin)(
          div(
            cls := s"flex flex-wrap justify-end gap-2 $touchChildrenCls",
            button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => open(EmployeeTarget.Edit(emp, false)))),
            button(
              tpe := "button",
              cls := btnSmall,
              aria.label := s"Override di ${fullName(emp)}",
              "Override",
              onClick --> (_ => open(EmployeeTarget.Edit(emp, true))),
            ),
            button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(emp)))),
          ),
        ),
      )

    div(
      div(
        cls := "space-y-4",
        view.listMods,
        child.maybe <-- pageError.signal.map(_.map(err => errorBanner(err, () => pageError.set(None)).amend(idAttr := listErrorId))),
        card(
          cls := "overflow-hidden",
          div(
            cls := "flex flex-wrap items-center gap-2 border-b border-slate-100 p-3",
            div(cls := "min-w-0 flex-1 basis-64", searchBox(query, "Cerca dipendente per nome o cognome…", pager)),
            roleGated(Role.Admin)(
              button(
                tpe := "button",
                cls := s"$btnPrimary min-h-10 shrink-0 md:min-h-0",
                "+ Nuovo dipendente",
                onClick --> (_ => open(EmployeeTarget.New)),
              ),
            ),
          ),
          renderResult(data) { all =>
            if all.isEmpty then div(cls := "p-4", emptyState("Nessun dipendente."))
            else
              searchedPages(Signal.fromValue(all), query.signal, ListSearch.employee, pager, "Nessun dipendente corrisponde alla ricerca.") {
                employees => div(cls := "divide-y divide-slate-100", employees.map(employeeRow))
              }
          },
        ),
      ),
      view.page(formView),
      confirmDeleteModal,
    )
  end apply
end EmployeesTab
