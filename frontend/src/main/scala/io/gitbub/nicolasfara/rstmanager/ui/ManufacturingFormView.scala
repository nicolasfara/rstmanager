package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, FieldIssue }
import org.scalajs.dom

/** What the catalog-manufacturing form page shows: a blank new template, or the stored one the user clicked "Modifica" on. */
enum ManufacturingTarget derives CanEqual:
  case New
  case Edit(manufacturing: ManufacturingCatalogResponse)

/**
 * The "Nuova lavorazione" / "Modifica lavorazione «X»" page, shown in place of the manufacturing catalog: a "Dati" section, a "Task" section with the
 * task rows, a "Materiali per unità" section (the price-less [[MaterialsEditor.quantitiesEditor]]), then the sticky action bar with the live total
 * duration.
 */
object ManufacturingFormView:

  /**
   * What the page reads from the catalog page: the manufacturing catalog load (an edited template deleted meanwhile blocks the save; a new one's
   * proposed code follows it), the codes already used when the page opens, and the task and employee lookups.
   */
  final case class Deps(
      manufacturings: Signal[Option[ApiClient.Result[List[ManufacturingCatalogResponse]]]],
      existingCodes: () => List[String],
      tasks: Signal[Option[ApiClient.Result[List[TaskResponse]]]],
      employeeOptions: Signal[List[(String, String)]],
  )

  private type TaskRowState = ManufacturingCatalogForm.TaskRow
  private type Field = ManufacturingCatalogForm.Field
  private val Field = ManufacturingCatalogForm.Field

  private val submitErrorId = "mfg-submit-error"

  /** `focusout` (bubbling, unlike `blur`), which Laminar does not predefine: tracks leaving the whole task block. */
  private val onFocusOut: EventProp[dom.FocusEvent] = eventProp("focusout")

  def apply(target: ManufacturingTarget, deps: Deps, onExit: () => Unit): HtmlElement =
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1; keyCounter
    def newTaskRow(): TaskRowState = ManufacturingCatalogForm.emptyRow(nextKey())

    val initial = target match
      case ManufacturingTarget.New => ManufacturingCatalogForm.fresh(GeneratedCodes.next("MFG", deps.existingCodes()), nextKey())
      case ManufacturingTarget.Edit(manufacturing) => ManufacturingCatalogForm.fromResponse(manufacturing, () => nextKey())
    val editingName = target match
      case ManufacturingTarget.New => None
      case ManufacturingTarget.Edit(manufacturing) => Some(manufacturing.code)
    val form = Var(initial)
    // The per-unit material rows of the form, checked against the active warehouse materials plus the stored lines of the edited template (an
    // archived or unknown material may stay on its line).
    val materialRows: Var[List[MaterialDrafts.Row]] = form.zoomLazy(_.materials)((state, rows) => state.copy(materials = rows))
    val storedMaterials = target match
      case ManufacturingTarget.New => Nil
      case ManufacturingTarget.Edit(manufacturing) => manufacturing.materials
    val warehouseMaterials = MaterialsEditor.activeMaterials()
    val materialsContext: Signal[MaterialDrafts.Context] = warehouseMaterials.map {
      case Some(Right(list)) => MaterialDrafts.Context.ofCatalog(list, storedMaterials)
      case _ => MaterialDrafts.Context.ofCatalog(Nil, storedMaterials)
    }
    val materialsLoadedSignal: Signal[Boolean] = warehouseMaterials.map {
      case Some(Right(_)) => true
      case _ => false
    }
    // Synchronous mirrors of the context and of whether the warehouse materials are loaded, read on submit.
    val currentMaterialsContext = Var(MaterialDrafts.Context.ofCatalog(Nil, storedMaterials))
    val materialsLoaded = Var(false)
    val touch = FormTouch()
    val submitting = Var(false)
    val formError = Var(Option.empty[ApiError])
    val gone = Var(false)
    // Synchronous view of catalog tasks by id, used to propose a task's default employee and duration when it is picked in a row.
    val tasksSnapshot = Var(Map.empty[String, TaskResponse])
    // Once the record is gone the draft cannot be saved, so it no longer counts as unsaved work (`None`): leaving does not ask.
    val draft = DraftGuard[Option[ManufacturingCatalogForm.State]](
      () => Option.unless(gone.now())(form.now()),
      _ => DraftGuard.discardTitle(editingName, "la nuova lavorazione"),
    )
    val leave = () => draft.discardThen(onExit)
    val isDirty = form.signal.combineWith(gone.signal).map(_ => draft.isDirtyNow)

    val taskOptions: Signal[List[(String, String)]] = deps.tasks.map {
      case Some(Right(list)) => list.map(t => t.id.toString -> s"${t.name} (${Formats.duration(t.requiredMinutes)}h)")
      case _ => Nil
    }
    val tasksById: Signal[Map[String, TaskResponse]] = deps.tasks.map {
      case Some(Right(list)) => list.map(task => task.id.toString -> task).toMap
      case _ => Map.empty
    }

    /** Where to move the focus for `field`: the "no task" issue points at the first row's task select. */
    def focusTarget(field: Field): String = field match
      case Field.Tasks => form.now().taskRows.headOption.fold(ManufacturingCatalogForm.tasksId)(row => ManufacturingCatalogForm.rowTaskId(row.key))
      case Field.Materials => ManufacturingCatalogForm.materialsId
      case other => ManufacturingCatalogForm.fieldId(other)

    // Always clickable: an invalid form reveals every error and focuses the first one instead of sending; re-entrant clicks are ignored. Saving an
    // untouched existing template just goes back.
    def submit(): Unit =
      val current = form.now()
      if !submitting.now() && !gone.now() then
        if current.editingId.isDefined && !draft.isDirtyNow then onExit()
        else
          val context = currentMaterialsContext.now()
          val built = current.request(tasksSnapshot.now().view.mapValues(_.requiredMinutes).toMap, context)
          ((current.issues ++ current.materialIssues(context, materialsLoaded.now())).headOption, built) match
            case (Some(first), _) =>
              touch.attemptSubmit()
              FormFocus.focus(focusTarget(first.field))
            case (None, Left(_)) =>
              // Not reachable: invalid lines are issues. Kept so a failed check can never send the template without its materials.
              touch.attemptSubmit()
              FormFocus.focus(ManufacturingCatalogForm.materialsId)
            case (None, Right(request)) =>
              submitting.set(true)
              formError.set(None)
              val effect = current.editingId match
                case Some(id) => ApiClient.updateManufacturingCatalog(id, request)
                case None => ApiClient.createManufacturingCatalog(request)
              effect.foreach { result =>
                submitting.set(false)
                result match
                  case Right(_) =>
                    Notifications.success(s"Lavorazione ${request.code} ${if current.editingId.isDefined then "salvata" else "creata"}")
                    AppBus.mutatedManufacturings()
                    onExit()
                  case Left(err) =>
                    showError(formError, "Salvataggio lavorazione")(err)
                    FormFocus.focus(submitErrorId)
              }
          end match
      end if
    end submit

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
        deps.employeeOptions,
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
        cls := "space-y-3 rounded-lg border border-slate-200 p-3",
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
            cls := s"$btnDanger mt-5 min-h-10 md:min-h-0",
            aria.label <-- position.map(n => s"Rimuovi task $n"),
            "Rimuovi",
            disabled <-- form.signal.map(_.taskRows.size <= 1),
            onClick --> (_ =>
              if form.now().taskRows.size > 1 then form.update(state => state.copy(taskRows = state.taskRows.filterNot(_.key == row.key)))
            ),
          ),
        ),
        div(
          cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
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

    // The "no task picked" error sits at the top of the block; it shows once focus left the block (focusout bubbles) or after a submit attempt.
    val tasksError: Signal[Option[String]] = touch.visible(ManufacturingCatalogForm.tasksId, errorOf(Field.Tasks))

    val dati = formSection("Dati", "")(
      div(
        cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
        Components.field(touch.spec("Codice", ManufacturingCatalogForm.fieldId(Field.Code), true, errorOf(Field.Code)))(bind =>
          textInput(form.signal.map(_.code), Observer[String](v => form.update(_.copy(code = v))), "MFG-2026-001")
            .amend(bind, onInput.mapToValue --> (_ => form.update(_.copy(codeManuallyEdited = true)))),
        ),
        Components.field(touch.spec("Nome", ManufacturingCatalogForm.fieldId(Field.Name), true, errorOf(Field.Name)))(bind =>
          textInput(form.signal.map(_.name), Observer[String](v => form.update(_.copy(name = v))), "Tendalino prua").amend(bind),
        ),
        Components
          .field(FieldSpec("Descrizione", "mfg-description"))(bind =>
            textInput(form.signal.map(_.description), Observer[String](v => form.update(_.copy(description = v))), "").amend(bind),
          )
          .amend(cls := "sm:col-span-2"),
      ),
    )

    val taskSection = formSection("Task", "Almeno un task obbligatorio. Durata e dipendente proposti dal catalogo restano modificabili.")(
      div(
        idAttr := ManufacturingCatalogForm.tasksId,
        cls := "space-y-3",
        onFocusOut --> (_ => touch.touch(ManufacturingCatalogForm.tasksId)),
        div(
          aria.live := "polite",
          child.maybe <-- tasksError.map(_.map(message => div(cls := "text-xs font-medium text-rose-600", message))),
        ),
        children <-- form.signal.map(_.taskRows).split(_.key)((_, initialRow, _) => renderTaskRow(initialRow)),
        button(
          tpe := "button",
          cls := s"$btnSmall min-h-10 md:min-h-0",
          "+ Task",
          onClick --> { _ =>
            val row = newTaskRow()
            form.update(state => state.copy(taskRows = state.taskRows :+ row))
            FormFocus.focus(ManufacturingCatalogForm.rowTaskId(row.key))
          },
        ),
      ),
    )

    val materialsSection = formSection(
      "Materiali per unità",
      "Quanto consuma una unità della lavorazione: negli ordini le quantità si moltiplicano per ×N.",
    )(
      div(
        idAttr := ManufacturingCatalogForm.materialsId,
        tabIndex := -1,
        cls := "space-y-2",
        materialsContext --> currentMaterialsContext.writer,
        materialsLoadedSignal --> materialsLoaded.writer,
        // The "not loaded" issue has no line to sit under: shown here once a submit was attempted.
        child.maybe <-- touch.submittedSignal.combineWith(form.signal, materialsContext, materialsLoadedSignal).map {
          (attempted, state, ctx, loaded) =>
            Option.when(attempted && !loaded)(state.materialIssues(ctx, loaded).headOption).flatten.map { issue =>
              div(cls := "text-xs font-medium text-rose-600", role := "alert", issue.message)
            }
        },
        child <-- materialRows.signal.map(_.isEmpty).distinct.map { empty =>
          if empty then
            div(
              p(cls := "mb-2 text-sm text-slate-500", "Nessun materiale."),
              button(
                tpe := "button",
                cls := s"$btnSmall min-h-10 md:min-h-0",
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
      RegistryPage.submitError(formError.signal, submitErrorId),
    )

    val totalDuration = div(
      cls := "flex flex-wrap items-center gap-x-3 gap-y-1",
      div(
        aria.live := "polite",
        "Durata totale ",
        span(cls := "font-semibold tabular-nums text-slate-800", child.text <-- form.signal.map(s => s"${Formats.duration(s.totalMinutes)}h")),
      ),
      dirtyNote(isDirty),
    )

    val goneReason = editingName.fold("")(code => s"La lavorazione «$code» non esiste più")

    formPage(
      PageHeader(
        "← Torna al catalogo lavorazioni",
        leave,
        Signal.fromValue(editingName.fold("Nuova lavorazione")(code => s"Modifica lavorazione «$code»")),
        context = "Catalogo lavorazioni" :: editingName.toList,
        notes = child.maybe <-- gone.signal.map(
          Option.when(_)(
            warningNote(s"$goneReason: potrebbe essere stata eliminata da un altro utente. Le modifiche non possono essere salvate."),
          ),
        ),
        // A new template opens with a proposed code: the name is what the user types first.
        initialFocus = if editingName.isDefined then PageFocus.Title else PageFocus.Element(ManufacturingCatalogForm.fieldId(Field.Name)),
      ),
    )(
      draft.bind,
      draft.dialog,
      DraftGuard.trackGone(deps.manufacturings, initial.editingId, _.id, gone)(() => draft.markClean()),
      deps.manufacturings --> {
        case Some(Right(list)) =>
          val current = form.now()
          // The regenerated code is not a user edit: the clean content follows it.
          if current.editingId.isEmpty && !current.codeManuallyEdited then
            val regenerate: ManufacturingCatalogForm.State => ManufacturingCatalogForm.State =
              _.copy(code = GeneratedCodes.next("MFG", list.map(_.code)))
            form.update(regenerate)
            draft.rebase(_.map(regenerate))
        case _ => ()
      },
      deps.tasks --> {
        case Some(Right(list)) => tasksSnapshot.set(list.map(task => task.id.toString -> task).toMap)
        case _ => ()
      },
      formColumn(
        dati,
        taskSection,
        materialsSection,
        stickyActionBar(
          totalDuration,
          formActionsInline(
            if editingName.isDefined then "Salva modifiche" else "Crea lavorazione",
            if editingName.isDefined then "Salvataggio…" else "Creazione…",
            submitting.signal,
            gone.signal.map(Option.when(_)(goneReason)),
            Some(leave),
            () => submit(),
          ),
        ),
      ),
    )
  end apply
end ManufacturingFormView
