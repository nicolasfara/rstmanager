package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

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
    val blockedDelete = Var(Option.empty[ApiError])
    // Task awaiting delete confirmation (double-check before the destructive action).
    val confirmDelete = Var(Option.empty[TaskResponse])

    def resetForm(): Unit =
      editingId.set(None); name.set(""); description.set(""); hours.set("8"); formError.set(None)

    def edit(task: TaskResponse): Unit =
      editingId.set(Some(task.id))
      name.set(task.name)
      description.set(task.description.getOrElse(""))
      hours.set(task.requiredHours.toString)
      formError.set(None)

    def submit(): Unit =
      val request = TaskRequest(name.now().trim.nn, Some(description.now().trim.nn).filter(_.nonEmpty), hours.now().toIntOption.getOrElse(0))
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
        roleGated(Role.Admin)(div(cls := "flex flex-wrap gap-2 border-t border-slate-100 pt-3", actionButtons(task))),
      )

    // Double-check before the destructive delete.
    val confirmDeleteModal =
      div(
        cls := "fixed inset-0 z-50 items-start justify-center overflow-y-auto bg-slate-900/50 p-2 sm:p-4",
        cls <-- confirmDelete.signal.map(c => if c.isDefined then "flex" else "hidden"),
        div(
          cls := "mt-12 w-full max-w-md sm:mt-24",
          card(
            div(
              cls := "border-b border-slate-100 px-4 py-3",
              h2(cls := "text-sm font-semibold text-slate-800", "Elimina task"),
            ),
            div(
              cls := "space-y-4 p-4",
              p(
                cls := "text-sm text-slate-600",
                child.text <-- confirmDelete.signal.map(
                  _.fold("")(t => s"Eliminare definitivamente il task “${t.name}”? L'operazione non è reversibile."),
                ),
              ),
              div(
                cls := "flex flex-col-reverse gap-2 sm:flex-row sm:justify-end",
                button(tpe := "button", cls := s"$btnGhost justify-center", "Annulla", onClick --> (_ => confirmDelete.set(None))),
                button(
                  tpe := "button",
                  cls := s"$btnDanger justify-center",
                  "Elimina",
                  onClick --> (_ => confirmDelete.now().foreach { t => delete(t.id); confirmDelete.set(None) }),
                ),
              ),
            ),
          ),
        ),
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
                      tr(th(cls := "px-4 py-2", "Nome"), th(cls := "px-4 py-2", "Descrizione"), th(cls := "px-4 py-2", "Ore"), th(cls := "px-4 py-2")),
                    ),
                    tbody(tasks.map(renderRow)),
                  ),
                ),
              )
          },
        ),
      ),
      div(
        cls := "fixed inset-0 z-50 items-start justify-center overflow-y-auto bg-slate-900/50 p-4",
        cls <-- blockedDelete.signal.map(err => if err.isDefined then "flex" else "hidden"),
        div(
          cls := "mt-24 w-full max-w-md",
          card(
            div(
              cls := "border-b border-slate-100 px-4 py-3",
              h2(cls := "text-sm font-semibold text-slate-800", "Task usato in lavorazioni"),
            ),
            div(
              cls := "space-y-4 p-4",
              p(cls := "text-sm text-slate-600", child.text <-- blockedDelete.signal.map(_.map(_.message).getOrElse(""))),
              ul(
                cls := "list-disc space-y-1 pl-5 text-sm text-slate-700",
                children <-- blockedDelete.signal.map(_.fold(List.empty[String])(_.details).map(item => li(item))),
              ),
              div(
                cls := "flex justify-end",
                button(tpe := "button", cls := btnPrimary, "Ok", onClick --> (_ => blockedDelete.set(None))),
              ),
            ),
          ),
        ),
      ),
      confirmDeleteModal,
    )
  end apply
end TasksPage
