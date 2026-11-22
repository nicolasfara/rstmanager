package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/**
 * Catalog of reusable tasks (name, description, required hours) referenced when building orders: a full-width, searched and paged list; "+ Nuovo
 * task" and "Modifica" open the [[TaskFormView]] page in its place.
 */
object TasksPage:

  private val listErrorId = "task-list-error"

  def apply(): HtmlElement =
    val view = SubView[TaskTarget]()
    // A refused delete (e.g. the task is used by catalog manufacturings), shown as a dismissible banner over the list.
    val listError = Var(Option.empty[ApiError])

    /** Opens the form page of `target`; a list message (e.g. a refused delete) would be stale on return, so it goes. */
    def openPage(target: TaskTarget): Unit =
      listError.set(None)
      view.open(target)
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

    def delete(task: TaskResponse): Unit =
      ApiClient.deleteTask(task.id).foreach {
        case Right(_) =>
          listError.set(None)
          Notifications.success(s"Task ${task.name} eliminato")
          AppBus.mutatedTasks()
        case Left(err) =>
          val shown =
            if err.code == "task-in-use" then err.copy(message = s"Impossibile eliminare il task «${task.name}»: è usato in queste lavorazioni")
            else err
          showBannerError(listError, "Eliminazione task")(shown)
          FormFocus.focus(listErrorId)
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
        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => openPage(TaskTarget.Edit(task)))),
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
    val confirmDeleteDialog =
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
        view.listMods,
        RegistryPage.listHeader("Catalogo task", "+ Nuovo task", () => openPage(TaskTarget.New)),
        RegistryPage.listError(listError, listErrorId),
        card(
          cls := "overflow-hidden",
          div(
            cls := "space-y-2 border-b border-slate-100 p-3",
            searchBox(query, "Cerca per nome, descrizione, dipendente…", pager),
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
      view.page(target => TaskFormView(target, data, employeeOptions, () => view.close())),
      confirmDeleteDialog,
    )
  end apply
end TasksPage
