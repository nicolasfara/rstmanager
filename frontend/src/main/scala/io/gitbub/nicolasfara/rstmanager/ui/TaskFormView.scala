package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.messageFor

/** What the catalog-task form page shows: a blank new task, or the stored task the user clicked "Modifica" on. */
enum TaskTarget derives CanEqual:
  case New
  case Edit(task: TaskResponse)

/**
 * The "Nuovo task" / "Modifica task «X»" page, shown in place of the task catalog: one "Dati task" section, then the sticky action bar. `tasks` is
 * the catalog's load (an edited task deleted meanwhile blocks the save), `employeeOptions` the "Dipendente predefinito" choices; `onExit` goes back.
 */
object TaskFormView:

  private val submitErrorId = "task-submit-error"

  def apply(
      target: TaskTarget,
      tasks: Signal[Option[ApiClient.Result[List[TaskResponse]]]],
      employeeOptions: Signal[List[(String, String)]],
      onExit: () => Unit,
  ): HtmlElement =
    val initial = target match
      case TaskTarget.New => TaskForm.empty
      case TaskTarget.Edit(task) => TaskForm.fromResponse(task)
    val editingName = Option.when(initial.editingId.isDefined)(initial.displayName)
    val form = Var(initial)
    val touch = FormTouch()
    val submitting = Var(false)
    val formError = Var(Option.empty[ApiError])
    val gone = Var(false)
    // Once the record is gone the draft cannot be saved, so it no longer counts as unsaved work (`None`): leaving does not ask.
    val draft = DraftGuard[Option[TaskForm.State]](
      () => Option.unless(gone.now())(form.now()),
      _ => DraftGuard.discardTitle(editingName, "il nuovo task"),
    )
    val leave = () => draft.discardThen(onExit)
    val isDirty = form.signal.combineWith(gone.signal).map(_ => draft.isDirtyNow)

    // Always clickable: an invalid form reveals every error and focuses the first one instead of sending; re-entrant clicks are ignored. Saving an
    // untouched existing task just goes back.
    def submit(): Unit =
      val current = form.now()
      if !submitting.now() && !gone.now() then
        if current.editingId.isDefined && !draft.isDirtyNow then onExit()
        else
          (current.issues.headOption, current.request) match
            case (Some(first), _) =>
              touch.attemptSubmit()
              FormFocus.focus(TaskForm.fieldId(first.field))
            case (None, None) =>
              touch.attemptSubmit()
              FormFocus.focus(TaskForm.fieldId(TaskForm.Field.Duration))
            case (None, Some(request)) =>
              submitting.set(true)
              formError.set(None)
              val effect = current.editingId match
                case Some(id) => ApiClient.updateTask(id, request)
                case None => ApiClient.createTask(request)
              effect.foreach { result =>
                submitting.set(false)
                result match
                  case Right(_) =>
                    Notifications.success(s"Task ${request.name} ${if current.editingId.isDefined then "salvato" else "creato"}")
                    AppBus.mutatedTasks()
                    onExit()
                  case Left(err) =>
                    showError(formError, "Salvataggio task")(err)
                    FormFocus.focus(submitErrorId)
              }
      end if
    end submit

    def errorOf(field: TaskForm.Field): Signal[Option[String]] = form.signal.map(_.issues.messageFor(field)).distinct

    def textField(
        spec: FieldSpec,
        get: TaskForm.State => String,
        set: (TaskForm.State, String) => TaskForm.State,
        placeholderText: String,
    ): HtmlElement =
      Components.field(spec)(bind => textInput(form.signal.map(get), Observer[String](v => form.update(set(_, v))), placeholderText).amend(bind))

    val datiTask = formSection("Dati task", "")(
      div(
        cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
        textField(
          touch.spec("Nome", TaskForm.fieldId(TaskForm.Field.Name), true, errorOf(TaskForm.Field.Name)),
          _.name,
          (s, v) => s.copy(name = v),
          "Es. Taglio",
        ).amend(cls := "sm:col-span-2"),
        textField(FieldSpec("Descrizione", "task-description"), _.description, (s, v) => s.copy(description = v), "Opzionale")
          .amend(cls := "sm:col-span-2"),
        textField(
          touch.spec("Durata richiesta (h:mm)", TaskForm.fieldId(TaskForm.Field.Duration), true, errorOf(TaskForm.Field.Duration)),
          _.hours,
          (s, v) => s.copy(hours = v),
          "1:35",
        ),
        Components.field(
          FieldSpec(
            "Dipendente predefinito",
            "task-default-employee",
            hintText = Some("Proposto quando il task viene aggiunto a una lavorazione o a un ordine."),
          ),
        )(bind =>
          selectInput(form.signal.map(_.defaultEmployee), Observer[String](v => form.update(_.copy(defaultEmployee = v))), employeeOptions)
            .amend(bind),
        ),
      ),
      RegistryPage.submitError(formError.signal, submitErrorId),
    )

    val goneReason = editingName.fold("")(name => s"Il task «$name» non esiste più")

    formPage(
      PageHeader(
        "← Torna al catalogo task",
        leave,
        Signal.fromValue(editingName.fold("Nuovo task")(name => s"Modifica task «$name»")),
        context = "Catalogo task" :: editingName.toList,
        notes = child.maybe <-- gone.signal.map(
          Option.when(_)(
            warningNote(s"$goneReason: potrebbe essere stato eliminato da un altro utente. Le modifiche non possono essere salvate."),
          ),
        ),
        initialFocus = if editingName.isDefined then PageFocus.Title else PageFocus.FirstField,
      ),
    )(
      draft.bind,
      draft.dialog,
      DraftGuard.trackGone(tasks, initial.editingId, _.id, gone)(() => draft.markClean()),
      formColumn(
        datiTask,
        stickyActionBar(
          dirtyNote(isDirty),
          formActionsInline(
            if editingName.isDefined then "Salva modifiche" else "Crea task",
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
end TaskFormView
