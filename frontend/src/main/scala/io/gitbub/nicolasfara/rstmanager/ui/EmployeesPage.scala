package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
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

  def apply(): HtmlElement =
    val pageError = Var(Option.empty[ApiError])

    // Create-employee form state
    val name = Var("")
    val surname = Var("")
    val contractKind = Var("full_time")
    val startDate = Var("")
    val endDate = Var("")
    val weeklyHours = Var("20")
    val budget = Var("40")

    def resetCreate(): Unit =
      name.set(""); surname.set(""); contractKind.set("full_time")
      startDate.set(""); endDate.set(""); weeklyHours.set("20"); budget.set("40")

    def buildContract(): EmployeeContractDto = contractKind.now() match
      case "fixed_term" => EmployeeContractDto("fixed_term", toIso(startDate.now()), Some(toIso(endDate.now())), None)
      case "part_time" => EmployeeContractDto("part_time", toIso(startDate.now()), None, weeklyHours.now().toIntOption)
      case _ => EmployeeContractDto("full_time", toIso(startDate.now()), None, None)

    def createEmployee(): Unit =
      val request = EmployeeRequest(name.now().trim.nn, surname.now().trim.nn, buildContract(), budget.now().toIntOption.getOrElse(0), Nil)
      ApiClient.createEmployee(request).foreach {
        case Right(_) => resetCreate(); pageError.set(None); AppBus.mutated()
        case Left(err) => pageError.set(Some(err))
      }

    // Override editor state
    val editing = Var(Option.empty[EmployeeResponse])
    val workingOverrides = Var(List.empty[HoursOverrideDto])
    val ovKind = Var("working_day")
    val ovHours = Var("8")
    val ovReason = Var("")
    val ovDay = Var("")
    val ovStart = Var("")
    val ovEnd = Var("")

    def openEditor(emp: EmployeeResponse): Unit =
      editing.set(Some(emp)); workingOverrides.set(emp.overrides); pageError.set(None)

    def addOverride(): Unit =
      val entry = ovKind.now() match
        case "vacation" => HoursOverrideDto("vacation", None, None, None, Some(toIso(ovStart.now())), Some(toIso(ovEnd.now())))
        case _ =>
          HoursOverrideDto(
            "working_day",
            ovHours.now().toIntOption,
            Some(ovReason.now().trim.nn).filter(_.nonEmpty),
            Some(toIso(ovDay.now())),
            None,
            None,
          )
      workingOverrides.update(_ :+ entry)
      ovHours.set("8"); ovReason.set(""); ovDay.set(""); ovStart.set(""); ovEnd.set("")

    def saveOverrides(): Unit = editing.now().foreach { emp =>
      val request = EmployeeRequest(emp.name, emp.surname, emp.contract, emp.budgetWeeklyHours, workingOverrides.now())
      ApiClient.updateEmployee(emp.id, request).foreach {
        case Right(_) => editing.set(None); AppBus.mutated()
        case Left(err) => pageError.set(Some(err))
      }
    }

    def deleteEmployee(id: UUID): Unit =
      ApiClient.deleteEmployee(id).foreach {
        case Right(_) => AppBus.mutated()
        case Left(err) => pageError.set(Some(err))
      }

    def overrideForm(): HtmlElement =
      div(
        cls := "mt-3 rounded-lg border border-slate-200 bg-slate-50 p-3",
        div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Aggiungi override"),
        div(
          cls := "mt-2 grid grid-cols-2 gap-2",
          field("Tipo", staticSelect(ovKind, overrideKinds)),
          div(),
          child <-- ovKind.signal.map {
            case "vacation" =>
              div(cls := "col-span-2 grid grid-cols-2 gap-2", field("Da", textInput(ovStart, "", "date")), field("A", textInput(ovEnd, "", "date")))
            case _ =>
              div(
                cls := "col-span-2 grid grid-cols-3 gap-2",
                field("Ore", textInput(ovHours, "", "number")),
                field("Giorno", textInput(ovDay, "", "date")),
                field("Motivo", textInput(ovReason, "Opzionale")),
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
          children <-- workingOverrides.signal.map { list =>
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
                    onClick --> (_ => workingOverrides.update(_.patch(index, Nil, 1))),
                  ),
                )
              }
          },
        ),
        overrideForm(),
        div(
          cls := "mt-3 flex gap-2",
          button(tpe := "button", cls := btnPrimary, "Salva override", onClick --> (_ => saveOverrides())),
          button(tpe := "button", cls := btnGhost, "Chiudi", onClick --> (_ => editing.set(None))),
        ),
      )

    val data = loadable(AppBus.ticks)(() => ApiClient.listEmployees())

    div(
      cls := "grid gap-6 lg:grid-cols-[22rem_1fr]",
      card(
        cls := "self-start p-4",
        sectionTitle("Nuovo dipendente"),
        div(
          cls := "mt-3 grid grid-cols-2 gap-3",
          field("Nome", textInput(name)),
          field("Cognome", textInput(surname)),
          field("Contratto", staticSelect(contractKind, contractOptions)),
          field("Inizio", textInput(startDate, "", "date")),
          child <-- contractKind.signal.map {
            case "fixed_term" => field("Fine", textInput(endDate, "", "date"))
            case "part_time" => field("Ore/sett.", textInput(weeklyHours, "", "number"))
            case _ => div()
          },
          field("Budget ore/sett.", textInput(budget, "", "number")),
        ),
        child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
        div(cls := "mt-3", button(tpe := "button", cls := btnPrimary, "Crea dipendente", onClick --> (_ => createEmployee()))),
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
                    div(
                      cls := "flex gap-2",
                      button(tpe := "button", cls := btnSmall, "Override", onClick --> (_ => openEditor(emp))),
                      button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => deleteEmployee(emp.id))),
                    ),
                  ),
                  child <-- editing.signal.map(e => if e.exists(_.id == emp.id) then editorPanel() else emptyNode),
                )
              },
            )
        },
      ),
    )
  end apply
end EmployeesPage
