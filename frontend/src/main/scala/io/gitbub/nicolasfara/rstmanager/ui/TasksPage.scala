package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Catalog of reusable tasks (name, description, required hours) referenced when building orders. */
object TasksPage:

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val editingId = Var(Option.empty[UUID])
    val name = Var("")
    val description = Var("")
    val hours = Var("8")
    val defaultEmployee = Var("")
    val blockedDelete = Var(Option.empty[ApiError])
    // Task awaiting delete confirmation (double-check before the destructive action).
    val confirmDelete = Var(Option.empty[TaskResponse])

    val employeesData = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val employeeOptions: Signal[List[(String, String)]] = employeesData.map {
      case Some(Right(list)) => ("" -> "— nessuno —") :: list.map(e => e.id.toString -> s"${e.name} ${e.surname}")
      case _ => List("" -> "—")
    }
    val employeesById: Signal[Map[String, EmployeeResponse]] = employeesData.map {
      case Some(Right(list)) => list.map(e => e.id.toString -> e).toMap
      case _ => Map.empty
    }

    def resetForm(): Unit =
      editingId.set(None); name.set(""); description.set(""); hours.set("8"); defaultEmployee.set(""); formError.set(None)

    def edit(task: TaskResponse): Unit =
      editingId.set(Some(task.id))
      name.set(task.name)
      description.set(task.description.getOrElse(""))
      hours.set(task.requiredHours.toString)
      defaultEmployee.set(task.defaultEmployeeId.map(_.toString).getOrElse(""))
      formError.set(None)

    def submit(): Unit =
      val request = TaskRequest(
        name.now().trim.nn,
        Some(description.now().trim.nn).filter(_.nonEmpty),
        hours.now().toIntOption.getOrElse(0),
        Try(UUID.fromString(defaultEmployee.now()).nn).toOption,
      )
      val effect = editingId.now() match
        case Some(id) => ApiClient.updateTask(id, request)
        case None => ApiClient.createTask(request)
      effect.foreach {
        case Right(_) => resetForm(); AppBus.mutatedTasks()
        case Left(err) => showError(formError, "Salvataggio task")(err)
      }

    def delete(id: UUID): Unit =
      ApiClient.deleteTask(id).foreach {
        case Right(_) => AppBus.mutatedTasks()
        case Left(err) if err.code == "task-in-use" => showError(blockedDelete, "Eliminazione task")(err)
        case Left(err) => showError(formError, "Eliminazione task")(err)
      }

    val data = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())

    def defaultEmployeeLabel(task: TaskResponse): Signal[String] =
      employeesById.map { employees =>
        task.defaultEmployeeId.flatMap(id => employees.get(id.toString)) match
          case Some(e) => s"${e.name} ${e.surname}"
          case None => "—"
      }

    def actionButtons(task: TaskResponse): List[HtmlElement] =
      List(
        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(task))),
        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => confirmDelete.set(Some(task)))),
      )

    def renderRow(task: TaskResponse): HtmlElement =
      tr(
        cls := "border-b border-slate-100 last:border-0",
        td(cls := "px-4 py-2 font-medium text-slate-800", task.name),
        td(cls := "px-4 py-2 text-slate-500", task.description.getOrElse("—")),
        td(cls := "px-4 py-2 tabular-nums", task.requiredHours.toString),
        td(cls := "px-4 py-2 text-slate-500", child.text <-- defaultEmployeeLabel(task)),
        td(
          cls := "px-4 py-2 text-right",
          roleGated(Role.Admin)(div(cls := "flex justify-end gap-2", actionButtons(task))),
        ),
      )

    def renderCard(task: TaskResponse): HtmlElement =
      div(
        cls := "space-y-3 p-4",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "break-words font-medium text-slate-800", task.name),
            task.description.map(d => div(cls := "mt-1 break-words text-sm text-slate-500", d)).getOrElse(emptyNode),
          ),
          div(
            cls := "shrink-0 text-right",
            div(cls := "text-xs font-medium text-slate-500", "Ore"),
            div(cls := "text-sm tabular-nums text-slate-800", task.requiredHours.toString),
          ),
        ),
        div(
          cls := "text-sm text-slate-600",
          span(cls := "text-xs font-medium text-slate-500", "Dipendente predefinito: "),
          span(child.text <-- defaultEmployeeLabel(task)),
        ),
        roleGated(Role.Admin)(div(cls := "flex flex-wrap gap-2 border-t border-slate-100 pt-3", actionButtons(task))),
      )

    // Double-check before the destructive delete.
    val confirmDeleteModal =
      confirmDialog(
        isOpen = confirmDelete.signal.map(_.isDefined),
        titleText = Signal.fromValue("Elimina task"),
        message = confirmDelete.signal.map(_.fold("")(t => s"Eliminare definitivamente il task “${t.name}”? L'operazione non è reversibile.")),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => confirmDelete.set(None),
        onConfirm = () => confirmDelete.now().foreach { t => delete(t.id); confirmDelete.set(None) },
      )

    div(
      div(
        cls := "grid gap-6",
        roleGatedGridCols(Role.Admin, "lg:grid-cols-[20rem_1fr]"),
        roleGated(Role.Admin)(
          card(
            cls := "self-start p-4",
            sectionTitle("Nuovo task"),
            div(
              cls := "mt-3 space-y-3",
              field("Nome", textInput(name, "Es. Taglio")),
              field("Descrizione", textInput(description, "Opzionale")),
              field("Ore richieste", textInput(hours, "8", inputType = "number")),
              field("Dipendente predefinito", selectInput(defaultEmployee, employeeOptions)),
              child.maybe <-- formError.signal.map(_.map(errorBanner)),
              div(
                cls := "flex gap-2",
                button(
                  tpe := "button",
                  cls := btnPrimary,
                  child.text <-- editingId.signal.map(_.fold("Crea")(_ => "Salva")),
                  onClick --> (_ => submit()),
                ),
                child.maybe <-- editingId.signal.map(_.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
              ),
            ),
          ),
        ),
        card(
          cls := "overflow-hidden",
          renderResult(data) { tasks =>
            if tasks.isEmpty then emptyState("Nessun task nel catalogo.")
            else
              div(
                // Mobile: stacked cards. Desktop (md+): the table.
                div(cls := "divide-y divide-slate-100 md:hidden", tasks.map(renderCard)),
                div(
                  cls := "hidden overflow-x-auto md:block",
                  table(
                    cls := "w-full text-sm",
                    thead(
                      cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                      tr(
                        th(cls := "px-4 py-2", "Nome"),
                        th(cls := "px-4 py-2", "Descrizione"),
                        th(cls := "px-4 py-2", "Ore"),
                        th(cls := "px-4 py-2", "Dipendente"),
                        th(cls := "px-4 py-2"),
                      ),
                    ),
                    tbody(tasks.map(renderRow)),
                  ),
                ),
              )
          },
        ),
      ),
      div(
        cls := "fixed inset-0 z-50 items-center justify-center overflow-y-auto bg-slate-900/50 p-4",
        cls <-- blockedDelete.signal.map(err => if err.isDefined then "flex" else "hidden"),
        onClick --> (_ => blockedDelete.set(None)),
        div(
          cls := "w-full max-w-sm",
          card(
            cls := "p-6",
            onClick.stopPropagation --> (_ => ()),
            div(
              cls := "flex flex-col items-center gap-4 text-center",
              div(
                cls := "flex h-12 w-12 items-center justify-center rounded-full bg-amber-100 text-amber-600",
                span(cls := "text-2xl leading-none", "⚠"),
              ),
              div(
                cls := "space-y-1",
                h2(cls := "text-base font-semibold text-slate-900", "Task usato in lavorazioni"),
                p(cls := "text-sm text-slate-500", child.text <-- blockedDelete.signal.map(_.map(_.message).getOrElse(""))),
              ),
            ),
            ul(
              cls := "mt-4 list-disc space-y-1 rounded-md bg-slate-50 py-2 pl-8 pr-3 text-sm text-slate-700",
              children <-- blockedDelete.signal.map(_.fold(List.empty[String])(_.details).map(item => li(item))),
            ),
            div(
              cls := "mt-6",
              button(tpe := "button", cls := s"$btnPrimary w-full justify-center", "Ok", onClick --> (_ => blockedDelete.set(None))),
            ),
          ),
        ),
      ),
      confirmDeleteModal,
    )
  end apply
end TasksPage
