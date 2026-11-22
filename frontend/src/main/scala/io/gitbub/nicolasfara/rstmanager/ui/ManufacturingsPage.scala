package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/**
 * Catalog of reusable manufacturings composed from live catalog task references: a full-width, searched and paged list; "+ Nuova lavorazione" and
 * "Modifica" open the [[ManufacturingFormView]] page in its place.
 */
object ManufacturingsPage:

  private val listErrorId = "mfg-list-error"

  def apply(): HtmlElement =
    val view = SubView[ManufacturingTarget]()
    val listError = Var(Option.empty[ApiError])

    /** Opens the form page of `target`; a list message (e.g. a refused delete) would be stale on return, so it goes. */
    def openPage(target: ManufacturingTarget): Unit =
      listError.set(None)
      view.open(target)

    val tasksData = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())
    val manufacturingsData = loadable(AppBus.manufacturingsTicks)(() => ApiClient.listManufacturingCatalog())
    val employeesData = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[ManufacturingCatalogResponse](pager)(
      "code" -> Column.text(_.code),
      "name" -> Column.text(_.name),
      "tasks" -> Column.by(_.tasks.size),
      "duration" -> Column.by(_.totalRequiredMinutes),
    )
    // The codes in use, so a new template proposes the next free one.
    val manufacturingsSnapshot = Var(List.empty[ManufacturingCatalogResponse])

    val employeeOptions: Signal[List[(String, String)]] = employeesData.map {
      case Some(Right(list)) => ("" -> "— nessuno —") :: list.map(e => e.id.toString -> s"${e.name} ${e.surname}")
      case _ => List("" -> "—")
    }
    val employeesById: Signal[Map[String, EmployeeResponse]] = employeesData.map {
      case Some(Right(list)) => list.map(employee => employee.id.toString -> employee).toMap
      case _ => Map.empty
    }

    val formDeps = ManufacturingFormView.Deps(manufacturingsData, () => manufacturingsSnapshot.now().map(_.code), tasksData, employeeOptions)

    val pendingDelete = Var(Option.empty[ManufacturingCatalogResponse])

    def delete(manufacturing: ManufacturingCatalogResponse): Unit =
      ApiClient.deleteManufacturingCatalog(manufacturing.id).foreach {
        case Right(_) =>
          listError.set(None)
          Notifications.success(s"Lavorazione ${manufacturing.code} eliminata")
          AppBus.mutatedManufacturings()
        case Left(err) =>
          showBannerError(listError, "Eliminazione lavorazione")(err)
          FormFocus.focus(listErrorId)
      }

    // ---- Confirmation dialog ---------------------------------------------------------------------
    val confirmDeleteDialog =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(m => s"Eliminare la lavorazione «${m.code} · ${m.name}»?")),
        message = Signal.fromValue("La lavorazione viene rimossa dal catalogo. L'operazione non è reversibile."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { m => delete(m); pendingDelete.set(None) },
      )

    // Reactive "task (dipendente)" summary, shared by the desktop row and the mobile card.
    def tasksSummary(manufacturing: ManufacturingCatalogResponse): Signal[String] =
      employeesById.map { employees =>
        val defaults = manufacturing.defaultEmployees.map(d => d.taskId -> d.employeeId.toString).toMap
        manufacturing.tasks.map { task =>
          defaults.get(task.id).flatMap(employees.get) match
            case Some(employee) => s"${task.name} (${employee.name} ${employee.surname})"
            case None => task.name
        }.mkString(", ")
      }

    /** "CODE 2,5 m, …" of the per-unit materials (archived ones marked); empty without materials. */
    def materialsSummary(manufacturing: ManufacturingCatalogResponse): String =
      manufacturing.materials.map { line =>
        val code = line.internalCode.getOrElse(Formats.shortId(line.materialId))
        val quantity =
          line.unit.fold(WarehouseFormats.formatDecimal(line.quantity, 0, MaterialDrafts.quantityScale))(WarehouseFormats.formatQty(line.quantity, _))
        s"$code $quantity${if line.archived then " (archiviato)" else ""}"
      }.mkString(", ")

    def actionButtons(manufacturing: ManufacturingCatalogResponse): List[HtmlElement] =
      List(
        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => openPage(ManufacturingTarget.Edit(manufacturing)))),
        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(manufacturing)))),
      )

    def renderRow(manufacturing: ManufacturingCatalogResponse): HtmlElement =
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(cls := "whitespace-nowrap px-4 py-2 font-medium text-slate-800", manufacturing.code),
        td(
          cls := "px-4 py-2",
          div(cls := "font-medium text-slate-700", manufacturing.name),
          manufacturing.description.map(d => div(cls := "text-xs text-slate-400", d)).getOrElse(emptyNode),
        ),
        td(
          cls := "px-4 py-2 text-slate-500",
          div(child.text <-- tasksSummary(manufacturing)),
          Option.when(manufacturing.materials.nonEmpty)(
            div(cls := "mt-0.5 text-xs text-slate-500", s"Materiali per unità: ${materialsSummary(manufacturing)}"),
          ),
        ),
        td(cls := "px-4 py-2 tabular-nums", s"${Formats.duration(manufacturing.totalRequiredMinutes)}h"),
        td(
          cls := "px-4 py-2 text-right",
          roleGated(Role.Admin)(div(cls := "flex justify-end gap-2", actionButtons(manufacturing))),
        ),
      )

    def renderCard(manufacturing: ManufacturingCatalogResponse): HtmlElement =
      div(
        cls := "space-y-3 p-4",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "text-xs font-medium text-slate-500", manufacturing.code),
            div(cls := "break-words font-medium text-slate-800", manufacturing.name),
            manufacturing.description.map(d => div(cls := "mt-1 break-words text-sm text-slate-500", d)).getOrElse(emptyNode),
          ),
          div(
            cls := "shrink-0 text-right",
            div(cls := "text-xs font-medium text-slate-500", "Durata"),
            div(cls := "text-sm tabular-nums text-slate-800", s"${Formats.duration(manufacturing.totalRequiredMinutes)}h"),
          ),
        ),
        div(
          cls := "text-sm text-slate-600",
          div(cls := "text-xs font-medium text-slate-500", "Task"),
          div(cls := "break-words", child.text <-- tasksSummary(manufacturing)),
        ),
        Option.when(manufacturing.materials.nonEmpty)(
          div(
            cls := "text-sm text-slate-600",
            div(cls := "text-xs font-medium text-slate-500", "Materiali per unità"),
            div(cls := "break-words", materialsSummary(manufacturing)),
          ),
        ),
        roleGated(Role.Admin)(div(cls := cardActionsCls, actionButtons(manufacturing))),
      )

    div(
      manufacturingsData --> {
        case Some(Right(list)) => manufacturingsSnapshot.set(list)
        case _ => ()
      },
      div(
        view.listMods,
        RegistryPage.listHeader("Catalogo lavorazioni", "+ Nuova lavorazione", () => openPage(ManufacturingTarget.New)),
        RegistryPage.listError(listError, listErrorId),
        card(
          cls := "overflow-hidden",
          div(
            cls := "space-y-2 border-b border-slate-100 p-3",
            searchBox(query, "Cerca per codice, nome, task…", pager),
            div(cls := "md:hidden", sort.picker(List("code" -> "Codice", "name" -> "Nome", "tasks" -> "Task", "duration" -> "Durata"))),
          ),
          renderResult(manufacturingsData) { all =>
            if all.isEmpty then div(cls := "p-4", emptyState("Nessuna lavorazione nel catalogo."))
            else
              searchedPages(sort.sorted(all), query.signal, ListSearch.manufacturing, pager, "Nessuna lavorazione corrisponde alla ricerca.") {
                manufacturings =>
                  div(
                    // Mobile: stacked cards. Desktop (md+): the table.
                    div(cls := "divide-y divide-slate-100 md:hidden", manufacturings.map(renderCard)),
                    div(
                      cls := "hidden overflow-x-auto md:block",
                      table(
                        cls := "w-full text-sm",
                        thead(
                          cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                          tr(
                            sort.header("code", "Codice", "px-4 py-2"),
                            sort.header("name", "Nome", "px-4 py-2"),
                            sort.header("tasks", "Task", "px-4 py-2"),
                            sort.header("duration", "Durata", "px-4 py-2"),
                            th(cls := "px-4 py-2"),
                          ),
                        ),
                        tbody(manufacturings.map(renderRow)),
                      ),
                    ),
                  )
              }
          },
        ),
      ),
      view.page(target => ManufacturingFormView(target, formDeps, () => view.close())),
      confirmDeleteDialog,
    )
  end apply
end ManufacturingsPage
