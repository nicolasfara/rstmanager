package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Catalog of reusable tasks (name, description, required hours) referenced when building orders. */
object TasksPage:

  def apply(): HtmlElement =
    val tick = Var(0)
    val formError = Var(Option.empty[ApiError])
    val editingId = Var(Option.empty[UUID])
    val name = Var("")
    val description = Var("")
    val hours = Var("8")

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
        case None     => ApiClient.createTask(request)
      effect.foreach {
        case Right(_)  => resetForm(); tick.update(_ + 1)
        case Left(err) => formError.set(Some(err))
      }

    def delete(id: UUID): Unit =
      ApiClient.deleteTask(id).foreach {
        case Right(_)  => tick.update(_ + 1)
        case Left(err) => formError.set(Some(err))
      }

    val data = loadable(tick.signal)(() => ApiClient.listTasks())

    div(
      cls := "grid gap-6 lg:grid-cols-[20rem_1fr]",
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
            button(tpe := "button", cls := btnPrimary, child.text <-- editingId.signal.map(_.fold("Crea")(_ => "Salva")), onClick --> (_ => submit())),
            child.maybe <-- editingId.signal.map(_.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
          ),
        ),
      ),
      card(
        cls := "overflow-hidden",
        renderResult(data) { tasks =>
          if tasks.isEmpty then emptyState("Nessun task nel catalogo.")
          else
            table(
              cls := "w-full text-sm",
              thead(
                cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                tr(th(cls := "px-4 py-2", "Nome"), th(cls := "px-4 py-2", "Descrizione"), th(cls := "px-4 py-2", "Ore"), th(cls := "px-4 py-2")),
              ),
              tbody(
                tasks.map { task =>
                  tr(
                    cls := "border-b border-slate-100 last:border-0",
                    td(cls := "px-4 py-2 font-medium text-slate-800", task.name),
                    td(cls := "px-4 py-2 text-slate-500", task.description.getOrElse("—")),
                    td(cls := "px-4 py-2 tabular-nums", task.requiredHours.toString),
                    td(
                      cls := "px-4 py-2 text-right",
                      div(
                        cls := "flex justify-end gap-2",
                        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(task))),
                        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => delete(task.id))),
                      ),
                    ),
                  )
                },
              ),
            )
        },
      ),
    )
end TasksPage
