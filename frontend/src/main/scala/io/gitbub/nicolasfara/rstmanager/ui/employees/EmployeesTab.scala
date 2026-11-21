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
}
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, FieldIssue }

/** Employee registry: create/edit form, and a dedicated editor for the per-employee hours overrides. */
object EmployeesTab:

  private val overrideKinds = List("working_day" -> "Giorno lavorativo", "vacation" -> "Ferie")

  private def describeContract(c: EmployeeContractDto): String = c.kind match
    case "full_time" => s"Tempo pieno · dal ${Formats.date(c.startDate)}"
    case "fixed_term" => s"Determinato · ${Formats.date(c.startDate)} → ${c.endDate.map(Formats.date).getOrElse("?")}"
    case "part_time" => s"Part-time · ${c.weeklyHours.getOrElse(0)}h/sett · dal ${Formats.date(c.startDate)}"
    case other => other

  private def describeOverride(o: HoursOverrideDto): String = o.kind match
    case "working_day" => s"${o.hours.getOrElse(0)}h il ${o.day.map(Formats.date).getOrElse("?")}${o.reason.map(r => s" · $r").getOrElse("")}"
    case "vacation" => s"Ferie ${o.startDate.map(Formats.date).getOrElse("?")} → ${o.endDate.map(Formats.date).getOrElse("?")}"
    case other => other

  private final case class OverrideFormState(
      kind: String,
      hours: String,
      reason: String,
      day: String,
      start: String,
      end: String,
  ):
    def toOverride: HoursOverrideDto = kind match
      case "vacation" => HoursOverrideDto("vacation", None, None, None, Some(Formats.toIso(start)), Some(Formats.toIso(end)))
      case _ =>
        HoursOverrideDto(
          "working_day",
          hours.toIntOption,
          Some(reason.trim.nn).filter(_.nonEmpty),
          Some(Formats.toIso(day)),
          None,
          None,
        )

  private object OverrideFormState:
    val empty: OverrideFormState = OverrideFormState("working_day", "8", "", "", "", "")

  private final case class OverrideEditorState(
      editing: Option[EmployeeResponse],
      workingOverrides: List[HoursOverrideDto],
      form: OverrideFormState,
  )

  private object OverrideEditorState:
    val empty: OverrideEditorState = OverrideEditorState(None, Nil, OverrideFormState.empty)

    def fromEmployee(emp: EmployeeResponse): OverrideEditorState =
      OverrideEditorState(Some(emp), emp.overrides, OverrideFormState.empty)

  def apply(): HtmlElement =
    val pageError = Var(Option.empty[ApiError])
    val form = Var(EmployeeForm.empty)
    val pendingDelete = Var(Option.empty[EmployeeResponse])
    val overrideEditor = Var(OverrideEditorState.empty)

    val touch = FormTouch()
    val submitting = Var(false)
    val draft = DraftGuard[EmployeeForm.State](
      () => form.now(),
      s => DraftGuard.discardTitle(Option.when(s.editingId.isDefined)(s"${s.name} ${s.surname}".trim.nn), "il nuovo dipendente"),
    )

    def resetForm(): Unit =
      form.set(EmployeeForm.empty)
      pageError.set(None)
      touch.reset()
      draft.markClean()

    val formAnchor = FormAnchor()

    // Replacing a dirty draft asks first; the form is revealed once the new content is loaded.
    def edit(emp: EmployeeResponse): Unit =
      draft.discardThen { () =>
        formAnchor.revealAndFocus()
        form.set(EmployeeForm.fromResponse(emp))
        pageError.set(None)
        touch.reset()
        draft.markClean()
      }

    /** "+ Nuovo dipendente" (mobile, where the form follows the list): leaves a record being edited, then shows the form. */
    def startNew(): Unit =
      if form.now().editingId.isDefined then
        draft.discardThen(() =>
          resetForm(); formAnchor.revealAndFocus(),
        )
      else formAnchor.revealAndFocus()

    // Both editors write the whole record: after a save, the other one (if open on the same employee) adopts the stored version, so its next
    // save does not revert what this one changed.
    // Always clickable: an invalid form reveals every error and focuses the first one instead of sending; re-entrant clicks are ignored.
    def submit(): Unit =
      val current = form.now()
      if !submitting.now() then
        current.firstIssueOnScreen match
          case Some(first) =>
            touch.attemptSubmit()
            FormFocus.focus(EmployeeForm.fieldId(first.field))
          case None =>
            submitting.set(true)
            val effect = current.editingId match
              case Some(id) => ApiClient.updateEmployee(id, current.request)
              case None => ApiClient.createEmployee(current.request)
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(saved) =>
                  resetForm()
                  overrideEditor.update(state => if state.editing.exists(_.id == saved.id) then state.copy(editing = Some(saved)) else state)
                  Notifications.success(s"Dipendente ${saved.name} ${saved.surname} ${if current.editingId.isDefined then "salvato" else "creato"}")
                  AppBus.mutatedEmployees()
                case Left(err) => showError(pageError, "Salvataggio dipendente")(err)
            }
      end if
    end submit

    def openEditor(emp: EmployeeResponse): Unit =
      overrideEditor.set(OverrideEditorState.fromEmployee(emp))
      pageError.set(None)

    def addOverride(): Unit =
      val entry = overrideEditor.now().form.toOverride
      overrideEditor.update(state => state.copy(workingOverrides = state.workingOverrides :+ entry, form = OverrideFormState.empty))

    def saveOverrides(): Unit = overrideEditor.now().editing.foreach { emp =>
      val request = EmployeeRequest(emp.name, emp.surname, emp.contract, emp.budgetWeeklyHours, overrideEditor.now().workingOverrides)
      ApiClient.updateEmployee(emp.id, request).foreach {
        case Right(saved) =>
          overrideEditor.set(OverrideEditorState.empty)
          // The stored overrides are not an edit of the form: the clean content follows them.
          val adopt: EmployeeForm.State => EmployeeForm.State =
            state => if state.editingId == Some(saved.id) then state.copy(overrides = saved.overrides) else state
          form.update(adopt)
          draft.rebase(adopt)
          Notifications.success(s"Override di ${saved.name} ${saved.surname} salvati")
          AppBus.mutatedEmployees()
        case Left(err) => showError(pageError, "Salvataggio override dipendente")(err)
      }
    }

    def deleteEmployee(emp: EmployeeResponse): Unit =
      ApiClient.deleteEmployee(emp.id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(emp.id) then resetForm()
          if overrideEditor.now().editing.exists(_.id == emp.id) then overrideEditor.set(OverrideEditorState.empty)
          Notifications.success(s"Dipendente ${emp.name} ${emp.surname} eliminato")
          AppBus.mutatedEmployees()
        case Left(err) => showError(pageError, "Eliminazione dipendente")(err)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(e => s"Eliminare il dipendente «${e.name} ${e.surname}»?")),
        message = Signal.fromValue("Il dipendente viene rimosso dall'anagrafica. L'operazione non è reversibile."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { e => deleteEmployee(e); pendingDelete.set(None) },
      )

    val data = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)

    val closures: Signal[List[ClosureResponse]] = loadable(AppBus.calendarTicks)(() => ApiClient.listClosures()).map {
      case Some(Right(list)) => list
      case _ => Nil
    }

    // Informational only: a vacation over a company closure is still saved (the closure already makes those days non-working).
    val vacationClosureNote: Signal[Option[HtmlElement]] =
      overrideEditor.signal.map(_.form).combineWith(closures).map { (f, list) =>
        Option
          .when(f.kind == "vacation")(CompanyCalendarView.closuresDuring(f.start, f.end, list))
          .filter(_.nonEmpty)
          .map(hits =>
            div(
              cls := "mt-2 rounded-md border border-amber-200 bg-amber-50 px-2 py-1 text-xs text-amber-700",
              s"Già coperto da chiusura aziendale: ${hits.map(c => s"${c.reason} (${CompanyCalendarView.describe(c)})").mkString(", ")}",
            ),
          )
      }

    def overrideForm(): HtmlElement =
      div(
        cls := "mt-3 rounded-lg border border-slate-200 bg-slate-50 p-3",
        div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Aggiungi override"),
        div(
          cls := "mt-2 grid grid-cols-2 gap-2",
          field(
            "Tipo",
            staticSelect(
              overrideEditor.signal.map(_.form.kind),
              Observer[String](v => overrideEditor.update(s => s.copy(form = s.form.copy(kind = v)))),
              overrideKinds,
            ),
          ),
          div(),
          child <-- overrideEditor.signal.map(_.form.kind).distinct.map {
            case "vacation" =>
              div(
                cls := "col-span-2 grid grid-cols-2 gap-2",
                field(
                  "Da",
                  textInput(
                    overrideEditor.signal.map(_.form.start),
                    Observer[String](v => overrideEditor.update(s => s.copy(form = s.form.copy(start = v)))),
                    "",
                    "date",
                  ),
                ),
                field(
                  "A",
                  textInput(
                    overrideEditor.signal.map(_.form.end),
                    Observer[String](v => overrideEditor.update(s => s.copy(form = s.form.copy(end = v)))),
                    "",
                    "date",
                  ),
                ),
              )
            case _ =>
              div(
                cls := "col-span-2 grid grid-cols-3 gap-2",
                field(
                  "Ore",
                  textInput(
                    overrideEditor.signal.map(_.form.hours),
                    Observer[String](v => overrideEditor.update(s => s.copy(form = s.form.copy(hours = v)))),
                    "",
                    "number",
                  ),
                ),
                field(
                  "Giorno",
                  textInput(
                    overrideEditor.signal.map(_.form.day),
                    Observer[String](v => overrideEditor.update(s => s.copy(form = s.form.copy(day = v)))),
                    "",
                    "date",
                  ),
                ),
                field(
                  "Motivo",
                  textInput(
                    overrideEditor.signal.map(_.form.reason),
                    Observer[String](v => overrideEditor.update(s => s.copy(form = s.form.copy(reason = v)))),
                    "Opzionale",
                  ),
                ),
              )
          },
        ),
        child.maybe <-- vacationClosureNote,
        button(tpe := "button", cls := s"$btnSmall mt-2", "+ Aggiungi", onClick --> (_ => addOverride())),
      )

    def editorPanel(): HtmlElement =
      div(
        cls := "mt-3 border-t border-slate-100 pt-3",
        div(
          cls := "flex flex-wrap gap-2",
          children <-- overrideEditor.signal.map(_.workingOverrides).map { list =>
            if list.isEmpty then List(span(cls := "text-xs text-slate-400", "Nessun override."))
            else
              list.zipWithIndex.map { case (o, index) =>
                span(
                  cls := "inline-flex items-center gap-1 rounded-full border border-slate-200 bg-white px-2 py-0.5 text-xs text-slate-600",
                  describeOverride(o),
                  button(
                    tpe := "button",
                    cls := "text-slate-400 hover:text-rose-600",
                    aria.label := "Rimuovi override",
                    title := "Rimuovi override",
                    "✕",
                    onClick --> (_ => overrideEditor.update(state => state.copy(workingOverrides = state.workingOverrides.patch(index, Nil, 1)))),
                  ),
                )
              }
          },
        ),
        overrideForm(),
        div(
          cls := "mt-3 flex gap-2",
          button(tpe := "button", cls := btnPrimary, "Salva override", onClick --> (_ => saveOverrides())),
          button(tpe := "button", cls := btnGhost, "Chiudi", onClick --> (_ => overrideEditor.set(OverrideEditorState.empty))),
        ),
      )

    val issues: Signal[List[FieldIssue[EmployeeForm.Field]]] = form.signal.map(_.issues)

    /** A text field of the employee form; `required` fields get the `*` and an inline error gated by [[FormTouch]]. */
    def textField(
        labelText: String,
        field: EmployeeForm.Field,
        get: EmployeeForm.State => String,
        set: (EmployeeForm.State, String) => EmployeeForm.State,
        kind: String,
    ): HtmlElement =
      val error = issues.map(_.messageFor(field)).distinct
      Components.field(touch.spec(labelText, EmployeeForm.fieldId(field), true, error))(bind =>
        textInput(form.signal.map(get), Observer[String](v => form.update(set(_, v))), "", kind).amend(bind),
      )

    val formCard = card(
      // Mobile: the list comes first (finding a record must not mean scrolling past the form); "+ Nuovo dipendente" reaches the form.
      cls := "order-last self-start p-4 lg:order-none",
      formAnchor.bind,
      draft.bind,
      child <-- form.signal
        .map(_.editingId.isDefined)
        .distinct
        .map(editing => sectionTitle(if editing then "Modifica dipendente" else "Nuovo dipendente")),
      requiredLegend.amend(cls := "mt-0.5"),
      div(
        cls := "mt-4 space-y-5",
        fieldGroup("Anagrafica")(
          div(
            cls := "grid grid-cols-2 gap-3",
            textField("Nome", EmployeeForm.Field.Name, _.name, (s, v) => s.copy(name = v), "text"),
            textField("Cognome", EmployeeForm.Field.Surname, _.surname, (s, v) => s.copy(surname = v), "text"),
          ),
        ),
        fieldGroup("Contratto")(
          Components.field(FieldSpec("Tipo di contratto", "emp-contract-kind"))(bind =>
            staticSelect(
              form.signal.map(_.contractKind),
              Observer[String](v => form.update(_.copy(contractKind = v))),
              EmployeeForm.contractOptions,
            ).amend(bind),
          ),
          div(
            cls := "grid grid-cols-2 gap-3",
            textField("Inizio", EmployeeForm.Field.StartDate, _.startDate, (s, v) => s.copy(startDate = v), "date"),
            // Rebuilt only when the contract kind changes, not on every keystroke.
            child <-- form.signal.map(_.contractKind).distinct.map {
              case "fixed_term" => textField("Fine", EmployeeForm.Field.EndDate, _.endDate, (s, v) => s.copy(endDate = v), "date")
              case "part_time" =>
                textField("Ore/sett. da contratto", EmployeeForm.Field.WeeklyHours, _.weeklyHours, (s, v) => s.copy(weeklyHours = v), "number")
              case _ => div()
            },
          ),
        ),
        fieldGroup("Capacità")(
          div(
            cls := "grid grid-cols-2 gap-3",
            textField("Budget ore/sett.", EmployeeForm.Field.Budget, _.budget, (s, v) => s.copy(budget = v), "number"),
          ),
          child.maybe <-- form.signal.map(s =>
            Option.when(s.editingId.isDefined && s.overrides.nonEmpty)(
              hint(s"${s.overrides.size} override invariati (si modificano con «Override»)."),
            ),
          ),
        ),
      ),
      child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      div(
        cls := "mt-4",
        child <-- form.signal.map(_.editingId.isDefined).distinct.map { editing =>
          formActions(
            if editing then "Salva" else "Crea",
            "Salvataggio…",
            submitting.signal,
            Option.when(editing)(() => draft.discardThen(() => resetForm())),
            () => submit(),
          )
        },
      ),
    )

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[22rem_1fr]"),
      roleGated(Role.Admin)(formCard),
      card(
        cls := "overflow-hidden",
        div(
          cls := "flex items-center gap-2 border-b border-slate-100 p-3",
          div(cls := "min-w-0 flex-1", searchBox(query, "Cerca dipendente per nome o cognome…", pager)),
          roleGated(Role.Admin)(
            button(tpe := "button", cls := s"$btnSmall min-h-10 shrink-0 lg:hidden", "+ Nuovo dipendente", onClick --> (_ => startNew())),
          ),
        ),
        renderResult(data) { all =>
          if all.isEmpty then div(cls := "p-4", emptyState("Nessun dipendente."))
          else
            searchedPages(Signal.fromValue(all), query.signal, ListSearch.employee, pager, "Nessun dipendente corrisponde alla ricerca.") {
              employees =>
                div(
                  cls := "space-y-3 p-4",
                  employees.map { emp =>
                    div(
                      cls := "rounded-lg border border-slate-200 p-3",
                      div(
                        cls := "flex items-start justify-between gap-3",
                        div(
                          cls := "min-w-0",
                          div(cls := "font-medium text-slate-800", s"${emp.name} ${emp.surname}"),
                          div(cls := "text-xs text-slate-500", describeContract(emp.contract)),
                          div(cls := "mt-0.5 text-xs text-slate-400", s"Budget ${emp.budgetWeeklyHours}h/sett · ${emp.overrides.size} override"),
                        ),
                        roleGated(Role.Admin)(
                          div(
                            cls := "flex flex-wrap justify-end gap-2",
                            button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(emp))),
                            button(tpe := "button", cls := btnSmall, "Override", onClick --> (_ => openEditor(emp))),
                            button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(emp)))),
                          ),
                        ),
                      ),
                      // `distinct`: the panel is rebuilt only when it opens or closes, not on every keystroke (that would drop the input focus).
                      child <-- overrideEditor.signal
                        .map(_.editing.exists(_.id == emp.id))
                        .distinct
                        .map(open => if open then editorPanel() else emptyNode),
                    )
                  },
                )
            }
        },
      ),
      confirmDeleteModal,
      draft.dialog,
    )
  end apply
end EmployeesTab
