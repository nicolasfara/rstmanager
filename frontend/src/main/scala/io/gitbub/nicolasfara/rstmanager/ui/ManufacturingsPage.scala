package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, FieldIssue }
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column
import org.scalajs.dom

/** Catalog of reusable manufacturings composed from live catalog task references. */
object ManufacturingsPage:

  private type TaskRowState = ManufacturingCatalogForm.TaskRow
  private type Field = ManufacturingCatalogForm.Field
  private val Field = ManufacturingCatalogForm.Field

  /** `focusout` (bubbling, unlike `blur`), which Laminar does not predefine: tracks leaving the whole task block. */
  private val onFocusOut: EventProp[dom.FocusEvent] = eventProp("focusout")

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1; keyCounter
    def newTaskRow(): TaskRowState = ManufacturingCatalogForm.emptyRow(nextKey())

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
    val manufacturingsSnapshot = Var(List.empty[ManufacturingCatalogResponse])
    // Synchronous view of catalog tasks by id, used to propose a task's default employee when it is picked in a row.
    val tasksSnapshot = Var(Map.empty[String, TaskResponse])

    val employeeOptions: Signal[List[(String, String)]] = employeesData.map {
      case Some(Right(list)) => ("" -> "— nessuno —") :: list.map(e => e.id.toString -> s"${e.name} ${e.surname}")
      case _ => List("" -> "—")
    }
    val employeesById: Signal[Map[String, EmployeeResponse]] = employeesData.map {
      case Some(Right(list)) => list.map(employee => employee.id.toString -> employee).toMap
      case _ => Map.empty
    }

    def freshForm(): ManufacturingCatalogForm.State =
      ManufacturingCatalogForm.fresh(GeneratedCodes.next("MFG", manufacturingsSnapshot.now().map(_.code)), nextKey())

    val form = Var(freshForm())
    // The per-unit material rows of the form, and the stored lines of the template being edited (archived/unknown materials may stay on them).
    val materialRows: Var[List[MaterialDrafts.Row]] = form.zoomLazy(_.materials)((state, rows) => state.copy(materials = rows))
    val editingMaterials = Var(List.empty[CatalogMaterialResponse])
    val warehouseMaterials = MaterialsEditor.activeMaterials()
    val materialsContext: Signal[MaterialDrafts.Context] = warehouseMaterials.combineWith(editingMaterials.signal).map {
      case (Some(Right(list)), current) => MaterialDrafts.Context.ofCatalog(list, current)
      case (_, current) => MaterialDrafts.Context.ofCatalog(Nil, current)
    }
    // Synchronous mirror of the context, read on submit.
    val currentMaterialsContext = Var(MaterialDrafts.Context.empty)
    val draft = DraftGuard[ManufacturingCatalogForm.State](
      () => form.now(),
      s => DraftGuard.discardTitle(Option.when(s.editingId.isDefined)(s"${s.code} · ${s.name}"), "la nuova lavorazione"),
    )

    val taskOptions: Signal[List[(String, String)]] = tasksData.map {
      case Some(Right(list)) => list.map(t => t.id.toString -> s"${t.name} (${Formats.duration(t.requiredMinutes)}h)")
      case _ => Nil
    }
    val tasksById: Signal[Map[String, TaskResponse]] = tasksData.map {
      case Some(Right(list)) => list.map(task => task.id.toString -> task).toMap
      case _ => Map.empty
    }

    val touch = FormTouch()
    val submitting = Var(false)

    def resetForm(): Unit =
      form.set(freshForm())
      editingMaterials.set(Nil)
      formError.set(None)
      touch.reset()
      draft.markClean()

    val formAnchor = FormAnchor()

    // Replacing a dirty draft asks first; the form is revealed once the new content is loaded.
    def edit(manufacturing: ManufacturingCatalogResponse): Unit =
      draft.discardThen { () =>
        formAnchor.revealAndFocus()
        editingMaterials.set(manufacturing.materials)
        form.set(ManufacturingCatalogForm.fromResponse(manufacturing, () => nextKey()))
        formError.set(None)
        touch.reset()
        draft.markClean()
      }

    /** "+ Nuova lavorazione" (mobile, where the form follows the list): leaves a record being edited, then shows the form. */
    def startNew(): Unit =
      if form.now().editingId.isDefined then
        draft.discardThen(() =>
          resetForm(); formAnchor.revealAndFocus(),
        )
      else formAnchor.revealAndFocus()

    /** Where to move the focus for `field`: the "no task" issue points at the first row's task select. */
    def focusTarget(field: Field): String = field match
      case Field.Tasks => form.now().taskRows.headOption.fold(ManufacturingCatalogForm.tasksId)(row => ManufacturingCatalogForm.rowTaskId(row.key))
      case Field.Materials => ManufacturingCatalogForm.materialsId
      case other => ManufacturingCatalogForm.fieldId(other)

    // Always clickable: an invalid form reveals every error and focuses the first one instead of sending; re-entrant clicks are ignored.
    def submit(): Unit =
      val current = form.now()
      if !submitting.now() then
        (current.issues ++ current.materialIssues(currentMaterialsContext.now())).headOption match
          case Some(first) =>
            touch.attemptSubmit()
            FormFocus.focus(focusTarget(first.field))
          case None =>
            submitting.set(true)
            val request = current.request(tasksSnapshot.now().view.mapValues(_.requiredMinutes).toMap, currentMaterialsContext.now())
            val effect = current.editingId match
              case Some(id) => ApiClient.updateManufacturingCatalog(id, request)
              case None => ApiClient.createManufacturingCatalog(request)
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(_) =>
                  resetForm()
                  Notifications.success(s"Lavorazione ${request.code} ${if current.editingId.isDefined then "salvata" else "creata"}")
                  AppBus.mutatedManufacturings()
                case Left(err) => showError(formError, "Salvataggio lavorazione")(err)
            }
      end if
    end submit

    val pendingDelete = Var(Option.empty[ManufacturingCatalogResponse])

    def delete(manufacturing: ManufacturingCatalogResponse): Unit =
      ApiClient.deleteManufacturingCatalog(manufacturing.id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(manufacturing.id) then resetForm()
          Notifications.success(s"Lavorazione ${manufacturing.code} eliminata")
          AppBus.mutatedManufacturings()
        case Left(err) => showError(formError, "Eliminazione lavorazione")(err)
      }

    // ---- Confirmation modal ----------------------------------------------------------------------
    val confirmDeleteModal =
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
        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(manufacturing))),
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
            div(cls := "mt-0.5 text-xs text-slate-400", s"Materiali per unità: ${materialsSummary(manufacturing)}"),
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

    def rowSignal(row: TaskRowState): Signal[TaskRowState] =
      form.signal.map(_.taskRows.find(_.key == row.key).getOrElse(row))

    def updateTaskRow(key: Int)(patch: TaskRowState => TaskRowState): Unit =
      form.update(state => state.copy(taskRows = state.taskRows.map(row => if row.key == key then patch(row) else row)))

    val issues: Signal[List[FieldIssue[Field]]] = form.signal.map(_.issues)
    def errorOf(field: Field): Signal[Option[String]] = issues.map(_.messageFor(field)).distinct

    def taskSelect(row: TaskRowState, bind: Modifier[HtmlElement]): HtmlElement =
      searchableSelect(
        rowSignal(row).map(_.taskId),
        Observer[String] { next =>
          // Propose the newly picked task's default employee and its catalog duration; both fields stay overridable.
          val picked = tasksSnapshot.now().get(next)
          val proposedEmployee = picked.flatMap(_.defaultEmployeeId).map(_.toString).getOrElse("")
          val proposedHours = picked.map(task => Formats.duration(task.requiredMinutes)).getOrElse("")
          updateTaskRow(row.key)(current =>
            current.copy(taskId = next, dependsOn = current.dependsOn.filter(_ != next), employeeId = proposedEmployee, hours = proposedHours),
          )
        },
        taskOptions,
        "Cerca un task per nome…",
        8,
        bind,
      )

    def dependencyChoices(row: TaskRowState): Signal[List[(String, String)]] =
      form.signal.combineWith(tasksById).map { case (state, tasks) =>
        state.taskRows.flatMap { other =>
          val id = other.taskId
          if other.key == row.key || id.isEmpty then None
          else tasks.get(id).map(task => id -> s"${task.name} (${Formats.duration(task.requiredMinutes)}h)")
        }
      }

    def employeeSelect(row: TaskRowState, bind: Modifier[HtmlElement]): HtmlElement =
      selectInput(
        rowSignal(row).map(_.employeeId),
        Observer[String](next => updateTaskRow(row.key)(_.copy(employeeId = next))),
        employeeOptions,
      ).amend(bind)

    def hoursInput(row: TaskRowState, bind: Modifier[HtmlElement]): HtmlElement =
      textInput(
        rowSignal(row).map(_.hours),
        Observer[String](next => updateTaskRow(row.key)(_.copy(hours = next))),
        "1:35",
      ).amend(bind)

    def renderTaskRow(row: TaskRowState): HtmlElement =
      val position: Signal[Int] = form.signal.map(_.taskRows.indexWhere(_.key == row.key) + 1).distinct
      div(
        cls := "rounded-md border border-slate-200 p-2",
        div(
          cls := "flex items-start gap-2",
          div(
            cls := "min-w-0 flex-1",
            Components.field(touch.spec("Task", ManufacturingCatalogForm.rowTaskId(row.key), true, errorOf(Field.RowTask(row.key))))(bind =>
              taskSelect(row, bind),
            ),
          ),
          button(
            tpe := "button",
            cls := s"$btnDanger mt-5",
            aria.label <-- position.map(n => s"Rimuovi task $n"),
            "Rimuovi",
            disabled <-- form.signal.map(_.taskRows.size <= 1),
            onClick --> (_ =>
              if form.now().taskRows.size > 1 then form.update(state => state.copy(taskRows = state.taskRows.filterNot(_.key == row.key)))
            ),
          ),
        ),
        div(
          cls := "mt-2 grid grid-cols-2 gap-2",
          Components.field(FieldSpec("Dipendente predefinito", s"mfg-row-${row.key}-employee"))(bind => employeeSelect(row, bind)),
          Components.field(
            touch.spec("Durata (h:mm)", ManufacturingCatalogForm.rowDurationId(row.key), true, errorOf(Field.RowDuration(row.key))),
          )(bind => hoursInput(row, bind)),
        ),
        checkboxChips(
          "Dipende da",
          dependencyChoices(row),
          rowSignal(row).map(_.dependsOn),
          (depId, isChecked) =>
            updateTaskRow(row.key)(current =>
              if isChecked then current.copy(dependsOn = current.dependsOn + depId) else current.copy(dependsOn = current.dependsOn - depId),
            ),
        ),
      )
    end renderTaskRow

    // The "no task picked" error sits under the block heading; it shows once focus left the block (focusout bubbles) or after a submit attempt.
    val tasksError: Signal[Option[String]] = touch.visible(ManufacturingCatalogForm.tasksId, errorOf(Field.Tasks))

    val formCard = card(
      // Mobile: the list comes first (finding a record must not mean scrolling past the form); "+ Nuova lavorazione" reaches the form.
      cls := "order-last self-start p-4 lg:order-none",
      formAnchor.bind,
      draft.bind,
      child <-- form.signal
        .map(_.editingId.isDefined)
        .distinct
        .map(editing => sectionTitle(if editing then "Modifica lavorazione" else "Nuova lavorazione")),
      requiredLegend.amend(cls := "mt-0.5"),
      div(
        cls := "mt-4 space-y-5",
        fieldGroup("Dati")(
          Components.field(touch.spec("Codice", ManufacturingCatalogForm.fieldId(Field.Code), true, errorOf(Field.Code)))(bind =>
            textInput(form.signal.map(_.code), Observer[String](v => form.update(_.copy(code = v))), "MFG-2026-001")
              .amend(bind, onInput.mapToValue --> (_ => form.update(_.copy(codeManuallyEdited = true)))),
          ),
          Components.field(touch.spec("Nome", ManufacturingCatalogForm.fieldId(Field.Name), true, errorOf(Field.Name)))(bind =>
            textInput(form.signal.map(_.name), Observer[String](v => form.update(_.copy(name = v))), "Tendalino prua").amend(bind),
          ),
          Components.field(FieldSpec("Descrizione", "mfg-description"))(bind =>
            textInput(form.signal.map(_.description), Observer[String](v => form.update(_.copy(description = v))), "").amend(bind),
          ),
        ),
        div(
          idAttr := ManufacturingCatalogForm.tasksId,
          cls := "space-y-2",
          onFocusOut --> (_ => touch.touch(ManufacturingCatalogForm.tasksId)),
          div(
            cls := subheadingCls,
            "Task",
            span(aria.hidden := true, cls := "ml-0.5 text-rose-600", "*"),
            span(cls := "sr-only", " (almeno uno obbligatorio)"),
          ),
          div(
            aria.live := "polite",
            child.maybe <-- tasksError.map(_.map(message => div(cls := "text-xs font-medium text-rose-600", message))),
          ),
          children <-- form.signal.map(_.taskRows).split(_.key)((_, initial, _) => renderTaskRow(initial)),
          div(
            cls := "flex items-center justify-between gap-2",
            button(
              tpe := "button",
              cls := btnSmall,
              "+ Task",
              onClick --> { _ =>
                val row = newTaskRow()
                form.update(state => state.copy(taskRows = state.taskRows :+ row))
                FormFocus.focus(ManufacturingCatalogForm.rowTaskId(row.key))
              },
            ),
            div(
              cls := "text-xs text-slate-500",
              "Durata totale ",
              span(cls := "font-medium tabular-nums text-slate-800", child.text <-- form.signal.map(s => s"${Formats.duration(s.totalMinutes)}h")),
            ),
          ),
        ),
      ),
      div(
        idAttr := ManufacturingCatalogForm.materialsId,
        tabIndex := -1,
        cls := "mt-5 space-y-2",
        materialsContext --> currentMaterialsContext.writer,
        subheading("Materiali per unità"),
        hint("Quanto consuma una unità della lavorazione: negli ordini le quantità si moltiplicano per ×N."),
        child <-- materialRows.signal.map(_.isEmpty).distinct.map { empty =>
          if empty then
            div(
              button(
                tpe := "button",
                cls := btnSmall,
                "+ Materiale",
                onClick --> (_ => materialRows.set(List(MaterialDrafts.Row.empty(nextKey())))),
              ),
            )
          else
            div(
              MaterialsEditor.materialsStatus(warehouseMaterials),
              MaterialsEditor.quantitiesEditor(materialRows, materialsContext, () => nextKey()),
            )
        },
      ),
      child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      div(
        cls := "mt-4",
        child <-- form.signal.map(_.editingId.isDefined).distinct.map { editing =>
          formActions(
            if editing then "Salva" else "Crea",
            "Salvataggio…",
            submitting.signal,
            Option.when(editing)(() => draft.discardThen(() => resetForm())),
            () => submit(),
          )
        },
      ),
    )

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[22rem_1fr]"),
      manufacturingsData --> {
        case Some(Right(list)) =>
          manufacturingsSnapshot.set(list)
          val current = form.now()
          // The regenerated code is not a user edit: the clean content follows it.
          if current.editingId.isEmpty && !current.codeManuallyEdited then
            val regenerate: ManufacturingCatalogForm.State => ManufacturingCatalogForm.State =
              _.copy(code = GeneratedCodes.next("MFG", list.map(_.code)))
            form.update(regenerate)
            draft.rebase(regenerate)
        case _ => ()
      },
      tasksData --> {
        case Some(Right(list)) => tasksSnapshot.set(list.map(task => task.id.toString -> task).toMap)
        case _ => ()
      },
      roleGated(Role.Admin)(formCard),
      card(
        cls := "overflow-hidden",
        div(
          cls := "space-y-2 border-b border-slate-100 p-3",
          div(
            cls := "flex items-center gap-2",
            div(cls := "min-w-0 flex-1", searchBox(query, "Cerca per codice, nome, task…", pager)),
            roleGated(Role.Admin)(
              button(tpe := "button", cls := s"$btnSmall min-h-10 shrink-0 lg:hidden", "+ Nuova lavorazione", onClick --> (_ => startNew())),
            ),
          ),
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
      confirmDeleteModal,
      draft.dialog,
    )
  end apply
end ManufacturingsPage
