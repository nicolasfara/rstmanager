package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Catalog of reusable manufacturings composed from live catalog task references. */
object ManufacturingsPage:

  private final case class TaskRowState(key: Int, taskId: String, dependsOn: Set[String])

  private final case class ManufacturingFormState(
      editingId: Option[UUID],
      code: String,
      codeManuallyEdited: Boolean,
      name: String,
      description: String,
      taskRows: List[TaskRowState],
  ):
    def errors: List[String] =
      List(
        Option.when(code.trim.nn.isEmpty)("Codice obbligatorio"),
        Option.when(name.trim.nn.isEmpty)("Nome obbligatorio"),
        Option.when(!taskRows.exists(_.taskId.nonEmpty))("Seleziona almeno un task"),
      ).flatten

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1; keyCounter
    def newTaskRow(): TaskRowState = TaskRowState(nextKey(), "", Set.empty)

    val tasksData = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())
    val manufacturingsData = loadable(AppBus.manufacturingsTicks)(() => ApiClient.listManufacturingCatalog())
    val manufacturingsSnapshot = Var(List.empty[ManufacturingCatalogResponse])

    def freshForm(): ManufacturingFormState =
      ManufacturingFormState(None, GeneratedCodes.next("MFG", manufacturingsSnapshot.now().map(_.code)), false, "", "", List(newTaskRow()))

    val form = Var(freshForm())

    val taskOptions: Signal[List[(String, String)]] = tasksData.map {
      case Some(Right(list)) => ("" -> "— task —") :: list.map(t => t.id.toString -> s"${t.name} (${t.requiredHours}h)")
      case _ => List("" -> "—")
    }
    val tasksById: Signal[Map[String, TaskResponse]] = tasksData.map {
      case Some(Right(list)) => list.map(task => task.id.toString -> task).toMap
      case _ => Map.empty
    }

    def resetForm(): Unit =
      form.set(freshForm())
      formError.set(None)

    def edit(manufacturing: ManufacturingCatalogResponse): Unit =
      val dependencyMap = manufacturing.dependencies.map(d => d.taskId.toString -> d.dependsOn.map(_.toString).toSet).toMap
      form.set(
        ManufacturingFormState(
          Some(manufacturing.id),
          manufacturing.code,
          false,
          manufacturing.name,
          manufacturing.description.getOrElse(""),
          manufacturing.taskIds.map(id => TaskRowState(nextKey(), id.toString, dependencyMap.getOrElse(id.toString, Set.empty))).toList,
        )
      )
      formError.set(None)

    def requestFromForm(): ManufacturingCatalogRequest =
      val current = form.now()
      val selectedIds = current.taskRows.flatMap(row => parseUuid(row.taskId))
      val selectedStrings = selectedIds.map(_.toString).toSet
      val dependencies = current.taskRows.flatMap { row =>
        parseUuid(row.taskId).map { taskId =>
          val dependsOn = row.dependsOn.toList.filter(id => selectedStrings.contains(id) && id != taskId.toString).flatMap(parseUuid)
          ManufacturingCatalogDependencyDto(taskId, dependsOn)
        }
      }
      ManufacturingCatalogRequest(
        current.code.trim.nn,
        current.name.trim.nn,
        Some(current.description.trim.nn).filter(_.nonEmpty),
        selectedIds,
        dependencies,
      )

    def submit(): Unit =
      val request = requestFromForm()
      val editedId = form.now().editingId
      val effect = editedId match
        case Some(id) => ApiClient.updateManufacturingCatalog(id, request)
        case None => ApiClient.createManufacturingCatalog(request)
      effect.foreach {
        case Right(_) =>
          resetForm(); AppBus.mutatedManufacturings()
        case Left(err) => showError(formError, "Salvataggio lavorazione")(err)
      }

    val pendingDelete = Var(Option.empty[ManufacturingCatalogResponse])

    def delete(id: UUID): Unit =
      ApiClient.deleteManufacturingCatalog(id).foreach {
        case Right(_) => AppBus.mutatedManufacturings()
        case Left(err) => showError(formError, "Eliminazione lavorazione")(err)
      }

    // ---- Confirmation (ACK) modal ----------------------------------------------------------------
    val confirmDeleteModal =
      div(
        cls := "fixed inset-0 z-50 items-start justify-center overflow-y-auto bg-slate-900/50 p-2 sm:p-4",
        cls <-- pendingDelete.signal.map(p => if p.isDefined then "flex" else "hidden"),
        div(
          cls := "mt-12 w-full max-w-md sm:mt-24",
          card(
            div(
              cls := "border-b border-slate-100 px-4 py-3",
              h2(cls := "text-sm font-semibold text-slate-800", "Eliminare la lavorazione?"),
            ),
            div(
              cls := "space-y-4 p-4",
              p(
                cls := "text-sm text-slate-600",
                child.text <-- pendingDelete.signal.map(
                  _.map(m => s"Stai per eliminare la lavorazione ${m.code} — ${m.name} dal catalogo. L'operazione è definitiva.").getOrElse("")
                ),
              ),
              div(
                cls := "flex flex-col-reverse gap-2 sm:flex-row sm:justify-end",
                button(tpe := "button", cls := s"$btnGhost justify-center", "Annulla", onClick --> (_ => pendingDelete.set(None))),
                button(
                  tpe := "button",
                  cls := s"$btnDanger justify-center",
                  "Elimina",
                  onClick --> (_ => pendingDelete.now().foreach { m => delete(m.id); pendingDelete.set(None) }),
                ),
              ),
            ),
          ),
        ),
      )

    def rowSignal(row: TaskRowState): Signal[TaskRowState] =
      form.signal.map(_.taskRows.find(_.key == row.key).getOrElse(row))

    def updateTaskRow(key: Int)(patch: TaskRowState => TaskRowState): Unit =
      form.update(state => state.copy(taskRows = state.taskRows.map(row => if row.key == key then patch(row) else row)))

    val formErrors: Signal[List[String]] = form.signal.map(_.errors)

    def taskSelect(row: TaskRowState): HtmlElement =
      selectInput(
        rowSignal(row).map(_.taskId),
        Observer[String](next => updateTaskRow(row.key)(current => current.copy(taskId = next, dependsOn = current.dependsOn.filter(_ != next)))),
        taskOptions,
      )

    def dependencyChoices(row: TaskRowState): Signal[List[(String, String)]] =
      form.signal.combineWith(tasksById).map { case (state, tasks) =>
        state.taskRows.flatMap { other =>
          val id = other.taskId
          if other.key == row.key || id.isEmpty then None
          else tasks.get(id).map(task => id -> s"${task.name} (${task.requiredHours}h)")
        }
      }

    def renderTaskRow(row: TaskRowState): HtmlElement =
      div(
        cls := "rounded-md border border-slate-200 p-2",
        div(
          cls := "flex items-end gap-2",
          div(cls := "flex-1", field("Task", taskSelect(row))),
          button(
            tpe := "button",
            cls := s"$btnDanger mb-0.5",
            "Rimuovi",
            disabled <-- form.signal.map(_.taskRows.size <= 1),
            onClick --> (_ => if form.now().taskRows.size > 1 then form.update(state => state.copy(taskRows = state.taskRows.filterNot(_.key == row.key)))),
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
                    checked <-- rowSignal(row).map(_.dependsOn.contains(depId)),
                    onChange.mapToChecked --> { checked =>
                      updateTaskRow(row.key)(current =>
                        if checked then current.copy(dependsOn = current.dependsOn + depId)
                        else current.copy(dependsOn = current.dependsOn - depId)
                      )
                    },
                  ),
                  labelText,
                )
              },
            )
        },
      )

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[22rem_1fr]"),
      manufacturingsData --> {
        case Some(Right(list)) =>
          manufacturingsSnapshot.set(list)
          val current = form.now()
          if current.editingId.isEmpty && !current.codeManuallyEdited then form.update(_.copy(code = GeneratedCodes.next("MFG", list.map(_.code))))
        case _ => ()
      },
      roleGated(Role.Admin)(
        card(
          cls := "self-start p-4",
          sectionTitle("Nuova lavorazione"),
          div(
            cls := "mt-3 space-y-3",
            field(
              "Codice",
              textInput(form.signal.map(_.code), Observer[String](v => form.update(_.copy(code = v))), "MFG-2026-001")
                .amend(onInput.mapToValue --> (_ => form.update(_.copy(codeManuallyEdited = true)))),
            ),
            field("Nome", textInput(form.signal.map(_.name), Observer[String](v => form.update(_.copy(name = v))), "Serramento standard")),
            field("Descrizione", textInput(form.signal.map(_.description), Observer[String](v => form.update(_.copy(description = v))), "Opzionale")),
            div(
              cls := "space-y-2",
              div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Task"),
              children <-- form.signal.map(_.taskRows).split(_.key)((_, initial, _) => renderTaskRow(initial)),
              button(tpe := "button", cls := btnSmall, "+ Task", onClick --> (_ => form.update(state => state.copy(taskRows = state.taskRows :+ newTaskRow())))),
            ),
            child.maybe <-- formError.signal.map(_.map(errorBanner)),
            div(
              cls := "flex gap-2",
              button(
                tpe := "button",
                cls := btnPrimary,
                child.text <-- form.signal.map(_.editingId.fold("Crea")(_ => "Salva")),
                disabled <-- formErrors.map(_.nonEmpty),
                onClick --> (_ => submit()),
              ),
              child.maybe <-- form.signal.map(_.editingId.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
            ),
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
                      roleGated(Role.Admin)(
                        div(
                          cls := "flex justify-end gap-2",
                          button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(manufacturing))),
                          button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(manufacturing)))),
                        ),
                      ),
                    ),
                  )
                },
              ),
            )
        },
      ),
      confirmDeleteModal,
    )
  end apply
end ManufacturingsPage
