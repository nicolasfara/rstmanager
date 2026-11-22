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
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/** Catalog of reusable tasks (name, description, required hours) referenced when building orders. */
object TasksPage:

  /** The content of the task form, compared with the loaded one to detect unsaved edits. */
  private final case class Draft(editingId: Option[UUID], name: String, description: String, hours: String, defaultEmployee: String) derives CanEqual

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val editingId = Var(Option.empty[UUID])
    val name = Var("")
    val description = Var("")
    val hours = Var("8:00")
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

    val draft = DraftGuard[Draft](
      () => Draft(editingId.now(), name.now(), description.now(), hours.now(), defaultEmployee.now()),
      d => DraftGuard.discardTitle(Option.when(d.editingId.isDefined)(d.name.trim.nn), "il nuovo task"),
    )

    def resetForm(): Unit =
      editingId.set(None); name.set(""); description.set(""); hours.set("8:00"); defaultEmployee.set(""); formError.set(None)
      draft.markClean()

    val formAnchor = FormAnchor()

    // Replacing a dirty draft asks first; the form is revealed once the new content is loaded.
    def edit(task: TaskResponse): Unit =
      draft.discardThen { () =>
        formAnchor.revealAndFocus()
        editingId.set(Some(task.id))
        name.set(task.name)
        description.set(task.description.getOrElse(""))
        hours.set(Formats.duration(task.requiredMinutes))
        defaultEmployee.set(task.defaultEmployeeId.map(_.toString).getOrElse(""))
        formError.set(None)
        draft.markClean()
      }

    /** "+ Nuovo task" (mobile, where the form follows the list): leaves a task being edited, then shows the form. */
    def startNew(): Unit =
      if editingId.now().isDefined then
        draft.discardThen(() =>
          resetForm(); formAnchor.revealAndFocus(),
        )
      else formAnchor.revealAndFocus()

    def submit(): Unit =
      Formats.parseDuration(hours.now()) match
        case None =>
          formError.set(Some(ApiError("invalid-duration", "Durata non valida. Usa il formato h:mm (es. 1:35).", Nil)))
        case Some(minutes) =>
          val request = TaskRequest(
            name.now().trim.nn,
            Some(description.now().trim.nn).filter(_.nonEmpty),
            minutes,
            Try(UUID.fromString(defaultEmployee.now()).nn).toOption,
          )
          val editing = editingId.now().isDefined
          val effect = editingId.now() match
            case Some(id) => ApiClient.updateTask(id, request)
            case None => ApiClient.createTask(request)
          effect.foreach {
            case Right(_) =>
              resetForm()
              Notifications.success(s"Task ${request.name} ${if editing then "salvato" else "creato"}")
              AppBus.mutatedTasks()
            case Left(err) => showError(formError, "Salvataggio task")(err)
          }

    def delete(task: TaskResponse): Unit =
      ApiClient.deleteTask(task.id).foreach {
        case Right(_) =>
          if editingId.now() == Some(task.id) then resetForm()
          Notifications.success(s"Task ${task.name} eliminato")
          AppBus.mutatedTasks()
        case Left(err) if err.code == "task-in-use" => showError(blockedDelete, "Eliminazione task")(err)
        case Left(err) => showError(formError, "Eliminazione task")(err)
      }

    val data = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val searchFields: Signal[TaskResponse => List[String]] = employeesById.map { employees => task =>
      ListSearch.task(task, task.defaultEmployeeId.flatMap(id => employees.get(id.toString)).map(e => s"${e.name} ${e.surname}"))
    }

    def employeeName(employees: Map[String, EmployeeResponse], task: TaskResponse): Option[String] =
      task.defaultEmployeeId.flatMap(id => employees.get(id.toString)).map(e => s"${e.name} ${e.surname}")
    val sort = TableSort[TaskResponse](
      employeesById.map { employees =>
        Map(
          "name" -> Column.text(_.name),
          "description" -> Column.optionalText(_.description),
          "duration" -> Column.by(_.requiredMinutes),
          "employee" -> Column.optionalText(employeeName(employees, _)),
        )
      },
      pager,
    )

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
        td(cls := "px-4 py-2 tabular-nums", s"${Formats.duration(task.requiredMinutes)}h"),
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
            div(cls := "text-xs font-medium text-slate-500", "Durata"),
            div(cls := "text-sm tabular-nums text-slate-800", s"${Formats.duration(task.requiredMinutes)}h"),
          ),
        ),
        div(
          cls := "text-sm text-slate-600",
          span(cls := "text-xs font-medium text-slate-500", "Dipendente predefinito: "),
          span(child.text <-- defaultEmployeeLabel(task)),
        ),
        roleGated(Role.Admin)(div(cls := cardActionsCls, actionButtons(task))),
      )

    // Double-check before the destructive delete.
    val confirmDeleteModal =
      confirmDialog(
        isOpen = confirmDelete.signal.map(_.isDefined),
        titleText = confirmDelete.signal.map(_.fold("")(t => s"Eliminare il task «${t.name}»?")),
        message = Signal.fromValue("Il task viene rimosso dal catalogo. L'operazione non è reversibile."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => confirmDelete.set(None),
        onConfirm = () => confirmDelete.now().foreach { t => delete(t); confirmDelete.set(None) },
      )

    div(
      div(
        cls := "grid gap-6",
        roleGatedGridCols(Role.Admin, "lg:grid-cols-[20rem_1fr]"),
        roleGated(Role.Admin)(
          card(
            // Mobile: the list comes first (finding a task must not mean scrolling past the form); "+ Nuovo task" reaches the form.
            cls := "order-last self-start p-4 lg:order-none",
            formAnchor.bind,
            draft.bind,
            child <-- editingId.signal.map(_.isDefined).distinct.map(editing => sectionTitle(if editing then "Modifica task" else "Nuovo task")),
            div(
              cls := "mt-3 space-y-3",
              field("Nome", textInput(name, "Es. Taglio")),
              field("Descrizione", textInput(description, "Opzionale")),
              field("Durata richiesta (h:mm)", textInput(hours, "1:35")),
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
                child.maybe <-- editingId.signal.map(
                  _.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => draft.discardThen(() => resetForm())))),
                ),
              ),
            ),
          ),
        ),
        card(
          cls := "overflow-hidden",
          div(
            cls := "space-y-2 border-b border-slate-100 p-3",
            div(
              cls := "flex items-center gap-2",
              div(cls := "min-w-0 flex-1", searchBox(query, "Cerca per nome, descrizione, dipendente…", pager)),
              roleGated(Role.Admin)(
                button(tpe := "button", cls := s"$btnSmall min-h-10 shrink-0 lg:hidden", "+ Nuovo task", onClick --> (_ => startNew())),
              ),
            ),
            div(
              cls := "md:hidden",
              sort.picker(List("name" -> "Nome", "description" -> "Descrizione", "duration" -> "Durata", "employee" -> "Dipendente")),
            ),
          ),
          renderResult(data) { all =>
            if all.isEmpty then div(cls := "p-4", emptyState("Nessun task nel catalogo."))
            else
              searchedPages(sort.sorted(all), query.signal, searchFields, pager, "Nessun task corrisponde alla ricerca.") { tasks =>
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
                          sort.header("name", "Nome", "px-4 py-2"),
                          sort.header("description", "Descrizione", "px-4 py-2"),
                          sort.header("duration", "Durata", "px-4 py-2"),
                          sort.header("employee", "Dipendente", "px-4 py-2"),
                          th(cls := "px-4 py-2"),
                        ),
                      ),
                      tbody(tasks.map(renderRow)),
                    ),
                  ),
                )
              }
          },
        ),
      ),
      // An accessible dialog (Escape, focus trap and restore) explaining why the task cannot be deleted.
      modal(blockedDelete.signal.map(_.isDefined), Signal.fromValue("Task usato in lavorazioni"), "max-w-md", () => blockedDelete.set(None))(
        div(
          p(cls := "text-sm text-slate-500", child.text <-- blockedDelete.signal.map(_.map(_.message).getOrElse(""))),
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
      confirmDeleteModal,
      draft.dialog,
    )
  end apply
end TasksPage
