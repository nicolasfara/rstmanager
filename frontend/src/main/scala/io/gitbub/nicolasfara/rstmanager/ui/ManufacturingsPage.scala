package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Catalog of reusable manufacturings composed from live catalog task references. */
object ManufacturingsPage:

  private final case class TaskRow(key: Int, taskId: Var[String], dependsOn: Var[Set[String]])

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val editingId = Var(Option.empty[UUID])
    val code = Var("")
    val name = Var("")
    val description = Var("")
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1; keyCounter
    def newTaskRow(): TaskRow = TaskRow(nextKey(), Var(""), Var(Set.empty))
    val taskRows = Var(List(newTaskRow()))

    val tasksData = loadable(AppBus.ticks)(() => ApiClient.listTasks())
    val manufacturingsData = loadable(AppBus.ticks)(() => ApiClient.listManufacturingCatalog())

    val taskOptions: Signal[List[(String, String)]] = tasksData.map {
      case Some(Right(list)) => ("" -> "— task —") :: list.map(t => t.id.toString -> s"${t.name} (${t.requiredHours}h)")
      case _ => List("" -> "—")
    }
    val tasksById: Signal[Map[String, TaskResponse]] = tasksData.map {
      case Some(Right(list)) => list.map(task => task.id.toString -> task).toMap
      case _ => Map.empty
    }

    def resetForm(): Unit =
      editingId.set(None)
      code.set("")
      name.set("")
      description.set("")
      taskRows.set(List(newTaskRow()))
      formError.set(None)

    def edit(manufacturing: ManufacturingCatalogResponse): Unit =
      val dependencyMap = manufacturing.dependencies.map(d => d.taskId.toString -> d.dependsOn.map(_.toString).toSet).toMap
      editingId.set(Some(manufacturing.id))
      code.set(manufacturing.code)
      name.set(manufacturing.name)
      description.set(manufacturing.description.getOrElse(""))
      taskRows.set(manufacturing.taskIds.map(id => TaskRow(nextKey(), Var(id.toString), Var(dependencyMap.getOrElse(id.toString, Set.empty)))).toList)
      formError.set(None)

    def requestFromForm(): ManufacturingCatalogRequest =
      val selectedIds = taskRows.now().flatMap(row => parseUuid(row.taskId.now()))
      val selectedStrings = selectedIds.map(_.toString).toSet
      val dependencies = taskRows.now().flatMap { row =>
        parseUuid(row.taskId.now()).map { taskId =>
          val dependsOn = row.dependsOn.now().toList.filter(id => selectedStrings.contains(id) && id != taskId.toString).flatMap(parseUuid)
          ManufacturingCatalogDependencyDto(taskId, dependsOn)
        }
      }
      ManufacturingCatalogRequest(
        code.now().trim.nn,
        name.now().trim.nn,
        Some(description.now().trim.nn).filter(_.nonEmpty),
        selectedIds,
        dependencies,
      )

    def submit(): Unit =
      val request = requestFromForm()
      val effect = editingId.now() match
        case Some(id) => ApiClient.updateManufacturingCatalog(id, request)
        case None => ApiClient.createManufacturingCatalog(request)
      effect.foreach {
        case Right(_) => resetForm(); AppBus.mutated()
        case Left(err) => formError.set(Some(err))
      }

    def delete(id: UUID): Unit =
      ApiClient.deleteManufacturingCatalog(id).foreach {
        case Right(_) => AppBus.mutated()
        case Left(err) => formError.set(Some(err))
      }

    def taskSelect(row: TaskRow): HtmlElement =
      select(
        cls := "w-full rounded-md border border-slate-300 px-2.5 py-1.5 text-sm text-slate-800 focus:border-slate-500 focus:outline-none focus:ring-1 focus:ring-slate-500",
        controlled(
          value <-- row.taskId.signal,
          onChange.mapToValue --> { next =>
            row.taskId.set(next)
            row.dependsOn.update(_.filter(_ != next))
            taskRows.update(_.map(identity))
          },
        ),
        children <-- taskOptions.map(_.map { case (optValue, optLabel) => option(value := optValue, optLabel) }),
      )

    def dependencyChoices(row: TaskRow): Signal[List[(String, String)]] =
      taskRows.signal.combineWith(tasksById).map { case (rows, tasks) =>
        rows.flatMap { other =>
          val id = other.taskId.now()
          if other.key == row.key || id.isEmpty then None
          else tasks.get(id).map(task => id -> s"${task.name} (${task.requiredHours}h)")
        }
      }

    def renderTaskRow(row: TaskRow): HtmlElement =
      div(
        cls := "rounded-md border border-slate-200 p-2",
        div(
          cls := "flex items-end gap-2",
          div(cls := "flex-1", field("Task", taskSelect(row))),
          button(
            tpe := "button",
            cls := s"$btnDanger mb-0.5",
            "Rimuovi",
            disabled <-- taskRows.signal.map(_.size <= 1),
            onClick --> (_ => if taskRows.now().size > 1 then taskRows.update(_.filterNot(_.key == row.key))),
          ),
        ),
        child <-- dependencyChoices(row).map { choices =>
          if choices.isEmpty then emptyNode
          else
            div(
              cls := "mt-2 flex flex-wrap gap-2 text-xs text-slate-600",
              span(cls := "font-medium text-slate-500", "Dipende da"),
              choices.map { case (depId, labelText) =>
                label(
                  cls := "inline-flex items-center gap-1 rounded-md border border-slate-200 px-2 py-1",
                  input(
                    typ := "checkbox",
                    checked <-- row.dependsOn.signal.map(_.contains(depId)),
                    onChange.mapToChecked --> { checked =>
                      if checked then row.dependsOn.update(_ + depId)
                      else row.dependsOn.update(_ - depId)
                    },
                  ),
                  labelText,
                )
              },
            )
        },
      )

    div(
      cls := "grid gap-6 lg:grid-cols-[22rem_1fr]",
      card(
        cls := "self-start p-4",
        sectionTitle("Nuova lavorazione"),
        div(
          cls := "mt-3 space-y-3",
          field("Codice", textInput(code, "MFG-2026-001")),
          field("Nome", textInput(name, "Serramento standard")),
          field("Descrizione", textInput(description, "Opzionale")),
          div(
            cls := "space-y-2",
            div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Task"),
            children <-- taskRows.signal.split(_.key)((_, initial, _) => renderTaskRow(initial)),
            button(tpe := "button", cls := btnSmall, "+ Task", onClick --> (_ => taskRows.update(_ :+ newTaskRow()))),
          ),
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
      card(
        cls := "overflow-hidden",
        renderResult(manufacturingsData) { manufacturings =>
          if manufacturings.isEmpty then emptyState("Nessuna lavorazione nel catalogo.")
          else
            table(
              cls := "w-full text-sm",
              thead(
                cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                tr(
                  th(cls := "px-4 py-2", "Codice"),
                  th(cls := "px-4 py-2", "Nome"),
                  th(cls := "px-4 py-2", "Task"),
                  th(cls := "px-4 py-2", "Ore"),
                  th(cls := "px-4 py-2"),
                ),
              ),
              tbody(
                manufacturings.map { manufacturing =>
                  tr(
                    cls := "border-b border-slate-100 align-top last:border-0",
                    td(cls := "px-4 py-2 font-medium text-slate-800", manufacturing.code),
                    td(
                      cls := "px-4 py-2",
                      div(cls := "font-medium text-slate-700", manufacturing.name),
                      manufacturing.description.map(d => div(cls := "text-xs text-slate-400", d)).getOrElse(emptyNode),
                    ),
                    td(cls := "px-4 py-2 text-slate-500", manufacturing.tasks.map(_.name).mkString(", ")),
                    td(cls := "px-4 py-2 tabular-nums", manufacturing.totalRequiredHours.toString),
                    td(
                      cls := "px-4 py-2 text-right",
                      div(
                        cls := "flex justify-end gap-2",
                        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(manufacturing))),
                        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => delete(manufacturing.id))),
                      ),
                    ),
                  )
                },
              ),
            )
        },
      ),
    )
  end apply
end ManufacturingsPage
