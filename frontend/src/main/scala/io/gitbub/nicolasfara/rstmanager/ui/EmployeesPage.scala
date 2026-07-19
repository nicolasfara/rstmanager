package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Employee registry with a dedicated editor for the per-employee hours overrides. */
object EmployeesPage:

  private val contractOptions = List("full_time" -> "Tempo pieno", "fixed_term" -> "Tempo determinato", "part_time" -> "Part-time")
  private val overrideKinds = List("working_day" -> "Giorno lavorativo", "vacation" -> "Ferie")

  /** HTML date inputs yield `yyyy-MM-dd`; the API expects a full ISO-8601 instant. */
  private def toIso(day: String): String = if day.isEmpty then "" else s"${day}T00:00:00.000Z"

  private def describeContract(c: EmployeeContractDto): String = c.kind match
    case "full_time" => s"Tempo pieno · dal ${Formats.date(c.startDate)}"
    case "fixed_term" => s"Determinato · ${Formats.date(c.startDate)} → ${c.endDate.map(Formats.date).getOrElse("?")}"
    case "part_time" => s"Part-time · ${c.weeklyHours.getOrElse(0)}h/sett · dal ${Formats.date(c.startDate)}"
    case other => other

  private def describeOverride(o: HoursOverrideDto): String = o.kind match
    case "working_day" => s"${o.hours.getOrElse(0)}h il ${o.day.map(Formats.date).getOrElse("?")}${o.reason.map(r => s" · $r").getOrElse("")}"
    case "vacation" => s"Ferie ${o.startDate.map(Formats.date).getOrElse("?")} → ${o.endDate.map(Formats.date).getOrElse("?")}"
    case other => other

  private final case class CreateEmployeeFormState(
      name: String,
      surname: String,
      contractKind: String,
      startDate: String,
      endDate: String,
      weeklyHours: String,
      budget: String,
  ):
    def contract: EmployeeContractDto = contractKind match
      case "fixed_term" => EmployeeContractDto("fixed_term", toIso(startDate), Some(toIso(endDate)), None)
      case "part_time" => EmployeeContractDto("part_time", toIso(startDate), None, weeklyHours.toIntOption)
      case _ => EmployeeContractDto("full_time", toIso(startDate), None, None)

    def request: EmployeeRequest =
      EmployeeRequest(name.trim.nn, surname.trim.nn, contract, budget.toIntOption.getOrElse(0), Nil)

    def errors: List[String] =
      List(
        Option.when(name.trim.nn.isEmpty)("Nome obbligatorio"),
        Option.when(surname.trim.nn.isEmpty)("Cognome obbligatorio"),
        Option.when(startDate.trim.nn.isEmpty)("Data inizio obbligatoria"),
        Option.when(budget.toIntOption.isEmpty)("Budget non valido"),
        Option.when(contractKind == "fixed_term" && endDate.trim.nn.isEmpty)("Data fine obbligatoria"),
        Option.when(contractKind == "part_time" && weeklyHours.toIntOption.forall(_ <= 0))("Ore settimanali non valide"),
      ).flatten
  end CreateEmployeeFormState

  private object CreateEmployeeFormState:
    val empty: CreateEmployeeFormState =
      CreateEmployeeFormState("", "", "full_time", "", "", "20", "40")

  private final case class OverrideFormState(
      kind: String,
      hours: String,
      reason: String,
      day: String,
      start: String,
      end: String,
  ):
    def toOverride: HoursOverrideDto = kind match
      case "vacation" => HoursOverrideDto("vacation", None, None, None, Some(toIso(start)), Some(toIso(end)))
      case _ =>
        HoursOverrideDto(
          "working_day",
          hours.toIntOption,
          Some(reason.trim.nn).filter(_.nonEmpty),
          Some(toIso(day)),
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

    val createForm = Var(CreateEmployeeFormState.empty)

    def resetCreate(): Unit =
      createForm.set(CreateEmployeeFormState.empty)

    def createEmployee(): Unit =
      ApiClient.createEmployee(createForm.now().request).foreach {
        case Right(_) => resetCreate(); pageError.set(None); AppBus.mutatedEmployees()
        case Left(err) => showError(pageError, "Creazione dipendente")(err)
      }

    val overrideEditor = Var(OverrideEditorState.empty)

    def openEditor(emp: EmployeeResponse): Unit =
      overrideEditor.set(OverrideEditorState.fromEmployee(emp))
      pageError.set(None)

    def addOverride(): Unit =
      val entry = overrideEditor.now().form.toOverride
      overrideEditor.update(state => state.copy(workingOverrides = state.workingOverrides :+ entry, form = OverrideFormState.empty))

    def saveOverrides(): Unit = overrideEditor.now().editing.foreach { emp =>
      val request = EmployeeRequest(emp.name, emp.surname, emp.contract, emp.budgetWeeklyHours, overrideEditor.now().workingOverrides)
      ApiClient.updateEmployee(emp.id, request).foreach {
        case Right(_) => overrideEditor.set(OverrideEditorState.empty); AppBus.mutatedEmployees()
        case Left(err) => showError(pageError, "Salvataggio override dipendente")(err)
      }
    }

    def deleteEmployee(id: UUID): Unit =
      ApiClient.deleteEmployee(id).foreach {
        case Right(_) => AppBus.mutatedEmployees()
        case Left(err) => showError(pageError, "Eliminazione dipendente")(err)
      }

    val createFormErrors: Signal[List[String]] = createForm.signal.map(_.errors)

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
          child <-- overrideEditor.signal.map(_.form.kind).map {
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

    val data = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[22rem_1fr]"),
      roleGated(Role.Admin)(
        card(
          cls := "self-start p-4",
          sectionTitle("Nuovo dipendente"),
          div(
            cls := "mt-3 grid grid-cols-2 gap-3",
            field("Nome", textInput(createForm.signal.map(_.name), Observer[String](v => createForm.update(_.copy(name = v))), "")),
            field("Cognome", textInput(createForm.signal.map(_.surname), Observer[String](v => createForm.update(_.copy(surname = v))), "")),
            field(
              "Contratto",
              staticSelect(createForm.signal.map(_.contractKind), Observer[String](v => createForm.update(_.copy(contractKind = v))), contractOptions),
            ),
            field(
              "Inizio",
              textInput(createForm.signal.map(_.startDate), Observer[String](v => createForm.update(_.copy(startDate = v))), "", "date"),
            ),
            child <-- createForm.signal.map(_.contractKind).map {
              case "fixed_term" =>
                field("Fine", textInput(createForm.signal.map(_.endDate), Observer[String](v => createForm.update(_.copy(endDate = v))), "", "date"))
              case "part_time" =>
                field(
                  "Ore/sett.",
                  textInput(createForm.signal.map(_.weeklyHours), Observer[String](v => createForm.update(_.copy(weeklyHours = v))), "", "number"),
                )
              case _ => div()
            },
            field(
              "Budget ore/sett.",
              textInput(createForm.signal.map(_.budget), Observer[String](v => createForm.update(_.copy(budget = v))), "", "number"),
            ),
          ),
          child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
          div(
            cls := "mt-3",
            button(
              tpe := "button",
              cls := btnPrimary,
              "Crea dipendente",
              disabled <-- createFormErrors.map(_.nonEmpty),
              onClick --> (_ => createEmployee()),
            ),
          ),
        ),
      ),
      card(
        cls := "p-4",
        renderResult(data) { employees =>
          if employees.isEmpty then emptyState("Nessun dipendente.")
          else
            div(
              cls := "space-y-3",
              employees.map { emp =>
                div(
                  cls := "rounded-lg border border-slate-200 p-3",
                  div(
                    cls := "flex items-start justify-between gap-3",
                    div(
                      div(cls := "font-medium text-slate-800", s"${emp.name} ${emp.surname}"),
                      div(cls := "text-xs text-slate-500", describeContract(emp.contract)),
                      div(cls := "mt-0.5 text-xs text-slate-400", s"Budget ${emp.budgetWeeklyHours}h/sett · ${emp.overrides.size} override"),
                    ),
                    roleGated(Role.Admin)(
                      div(
                        cls := "flex gap-2",
                        button(tpe := "button", cls := btnSmall, "Override", onClick --> (_ => openEditor(emp))),
                        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => deleteEmployee(emp.id))),
                      ),
                    ),
                  ),
                  child <-- overrideEditor.signal.map(e => if e.editing.exists(_.id == emp.id) then editorPanel() else emptyNode),
                )
              },
            )
        },
      ),
    )
  end apply
end EmployeesPage
