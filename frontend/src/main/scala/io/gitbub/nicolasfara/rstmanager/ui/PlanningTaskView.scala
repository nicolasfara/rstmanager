package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.messageFor
import io.gitbub.nicolasfara.rstmanager.ui.TaskProgressForm.{ Field, Mode }

/**
 * Full page of one planning task, shown in place of the board (see [[SubView]]): the progress of a planned task (hours done and total, the
 * manufacturing's preferred employee, "Segna completato"), or the reactivation of a completed one. Built for the workshop: large inputs and buttons,
 * the order / manufacturing / deadline context at arm's length, quick "+0:30 / +1:00 / +2:00" chips that only fill "Ore svolte".
 *
 * The form is built once from the target snapshot: a plan refresh while the page is open does not rebuild it. Validation is inline
 * ([[TaskProgressForm]]); only a failed request shows an error banner.
 */
object PlanningTaskView:

  /**
   * What the page shows about the task, resolved by the board when the user opens it (a snapshot, not refreshed). `quantity` is the multiplier ×N of
   * the manufacturing; `materialShortage` the "Materiale da ordinare: …" text of its slices short of material.
   */
  final case class TaskContext(
      orderId: UUID,
      orderNumber: String,
      customerName: Option[String],
      manufacturingId: UUID,
      manufacturingCode: String,
      manufacturingDescription: Option[String],
      deadline: String,
      task: ScheduledTaskDto,
      taskName: String,
      quantity: Int = 1,
      materialShortage: Option[String] = None,
  ) derives CanEqual

  /** The page to open: the progress of a planned task (from a slice card of `sliceDay`), or the reactivation of a completed task. */
  enum TaskTarget derives CanEqual:
    case Progress(context: TaskContext, preferred: Option[UUID], sliceDay: String)
    case Reactivate(context: TaskContext)

  private val submitErrorId = "pt-submit-error"
  private val employeeId = "pt-employee"

  /** Workshop size of the controls: 48px tall, 16px text (also avoids the iOS zoom on focus). */
  private val workshopInputCls = "min-h-12 text-base!"

  /**
   * The cancel / submit pair of the sticky bar, 48px tall at all widths (workshop touch target), the primary one rightmost and widest. Same contract
   * as [[Components.formActionsInline]], whose buttons shrink back above `md`. Candidate for promotion to `Components` if another workshop page needs
   * it.
   */
  private def workshopActions(submitLabel: String, submitting: Signal[Boolean], onCancel: () => Unit, onSubmit: () => Unit): HtmlElement =
    div(
      cls := "ml-auto flex items-center gap-2",
      button(tpe := "button", cls := s"$btnGhost min-h-12 min-w-24 justify-center", "Annulla", onClick --> (_ => onCancel())),
      button(
        tpe := "button",
        cls := s"$btnPrimary min-h-12 min-w-32 justify-center",
        disabled <-- submitting,
        aria.busy <-- submitting,
        child.text <-- submitting.map(busy => if busy then "Salvataggio…" else submitLabel),
        onClick --> (_ => onSubmit()),
      ),
    )

  /** The task's progress as a large bar with "h:mm / h:mm h", green once the estimate is covered. */
  private def progressBar(task: ScheduledTaskDto): HtmlElement =
    val done = task.completedMinutes.getOrElse(0)
    val total = task.expectedMinutes
    val percent = if total > 0 then math.min(100, math.max(0, done * 100 / total)) else 0
    div(
      cls := "space-y-1.5",
      div(
        cls := "flex items-baseline justify-between gap-2 text-sm",
        span(cls := "text-xs font-medium text-slate-500", "Avanzamento registrato"),
        span(
          cls := "font-semibold tabular-nums text-slate-800",
          s"${Formats.duration(done)} / ${Formats.duration(total)} h",
          span(cls := "ml-2 text-xs font-normal text-slate-500", s"$percent%"),
        ),
      ),
      div(
        role := "progressbar",
        aria.label := "Ore svolte sulle ore totali",
        aria.valueMin := 0,
        aria.valueMax := 100,
        aria.valueNow := percent,
        cls := "h-3 w-full overflow-hidden rounded-full bg-slate-100",
        div(cls := s"h-full rounded-full ${if percent >= 100 then "bg-emerald-500" else "bg-slate-600"}", width := s"$percent%"),
      ),
    )
  end progressBar

  /** The read-only context card: manufacturing, order, customer, deadline (with the board's urgency badge) and the recorded progress. */
  private def contextSection(target: TaskTarget, context: TaskContext): HtmlElement =
    val urgency = target match
      case TaskTarget.Progress(_, _, sliceDay) =>
        Some(PlanningPage.urgencyFor(Formats.daysUntil(sliceDay, context.deadline))).filter(_.label.nonEmpty)
      case TaskTarget.Reactivate(_) => None
    val dayField = target match
      case TaskTarget.Progress(_, _, sliceDay) => readField("Pianificato il", Formats.date(sliceDay))
      case TaskTarget.Reactivate(ctx) => readField("Completato il", ctx.task.completionDate.map(Formats.date).getOrElse("—"))
    formSection("Lavorazione", "")(
      div(
        div(
          cls := "flex flex-wrap items-center gap-2",
          span(cls := "text-lg font-bold leading-tight text-slate-900 break-words", context.manufacturingCode),
          OrderFormView.quantityBadge(context.quantity),
        ),
        context.manufacturingDescription.fold[Modifier[HtmlElement]](emptyNode)(desc => p(cls := "mt-0.5 text-sm text-slate-600", desc)),
      ),
      div(
        cls := "grid grid-cols-1 gap-x-6 gap-y-3 sm:grid-cols-2",
        readField("Ordine", context.orderNumber),
        readField("Cliente", context.customerName.getOrElse("—")),
        readField(
          "Consegna lavorazione",
          span(
            cls := "inline-flex flex-wrap items-center gap-2",
            Formats.date(context.deadline),
            urgency.fold[Modifier[HtmlElement]](emptyNode)(u => span(cls := s"rounded-full px-2 py-0.5 text-xs font-semibold ${u.labelCls}", u.label)),
          ),
        ),
        dayField,
      ),
      progressBar(context.task),
      context.materialShortage.map(warningNote),
    )
  end contextSection

  /**
   * The page of `target`. `employeeOptions` feeds "Dipendente preferito" (value = employee id, "" = automatic); `onExit` goes back to the board
   * WITHOUT asking (the page runs the dirty guard itself before calling it).
   */
  def apply(target: TaskTarget, employeeOptions: Signal[List[(String, String)]], onExit: () => Unit): HtmlElement =
    val (context, mode, preferred) = target match
      case TaskTarget.Progress(ctx, pref, _) => (ctx, Mode.Progress, pref)
      case TaskTarget.Reactivate(ctx) => (ctx, Mode.Reactivate, None)
    val baseline = TaskProgressForm.baseline(context.task, preferred)
    val state = Var(TaskProgressForm.initial(mode, baseline))
    val touch = FormTouch()
    val submitting = Var(false)
    val submitError = Var(Option.empty[ApiError])
    val draft = DraftGuard[TaskProgressForm.State](() => state.now(), _ => DraftGuard.discardTitle(Some(context.taskName), "il task"))
    val leave = () => draft.discardThen(onExit)

    val issues = state.signal.map(TaskProgressForm.issues(mode, _, baseline))
    def rawError(field: Field): Signal[Option[String]] = issues.map(_.messageFor(field))

    def submit(): Unit =
      if !submitting.now() then
        touch.attemptSubmit()
        val current = state.now()
        TaskProgressForm.issues(mode, current, baseline) match
          case first :: _ => FormFocus.focus(TaskProgressForm.fieldId(first.field))
          case Nil =>
            val submission = TaskProgressForm.submission(mode, current, baseline)
            // Nothing changed (e.g. "1" typed over "1:00"): back to the board, no request and no toast.
            if submission.isEmpty then onExit()
            else
              submitting.set(true)
              submitError.set(None)
              val taskSaved: Future[ApiClient.Result[Boolean]] = submission.task.fold(Future.successful(Right(false))) { request =>
                ApiClient.updateScheduledTask(context.orderId, context.manufacturingId, context.task.id, request).map(_.map(_ => true))
              }
              taskSaved.flatMap {
                case Left(err) => Future.successful(Left(err))
                case Right(taskChanged) =>
                  submission.employee
                    .fold(Future.successful(Right(()))) { employee =>
                      ApiClient.setPreferredEmployee(context.orderId, context.manufacturingId, employee).map(_.map(_ => ()))
                    }
                    .map {
                      case Left(err) =>
                        // The hours were saved even though the employee was not: refresh the board anyway.
                        if taskChanged then AppBus.mutatedOrders()
                        Left(err)
                      case Right(()) => Right(())
                    }
              }.foreach {
                case Right(()) =>
                  Notifications.success(TaskProgressForm.successMessage(TaskProgressForm.outcome(mode, current, baseline), context.taskName))
                  AppBus.mutatedOrders()
                  onExit()
                case Left(err) =>
                  submitting.set(false)
                  showError(submitError, "Aggiornamento task")(err)
                  FormFocus.focus(submitErrorId)
              }
            end if
        end match

    def markCompleted(): Unit =
      state.update(TaskProgressForm.markCompleted(_, baseline))
      submit()

    def hoursInput(field: Field, labelText: String, valueOf: TaskProgressForm.State => String, write: String => Unit): HtmlElement =
      Components.field(touch.spec(labelText, TaskProgressForm.fieldId(field), rawError(field))) { bind =>
        textInput(state.signal.map(valueOf), Observer[String](write), "1:35").amend(bind, cls := workshopInputCls, autoComplete := "off")
      }

    val quickChips: HtmlElement =
      div(
        cls := "mt-2",
        div(
          role := "group",
          aria.label := "Aggiunta rapida alle ore svolte",
          cls := "flex flex-wrap gap-2",
          TaskProgressForm.quickIncrements.map { (minutes, labelText) =>
            button(
              tpe := "button",
              cls := s"$btnGhost min-h-12 min-w-16 justify-center tabular-nums disabled:opacity-40 disabled:cursor-not-allowed",
              aria.label := s"Aggiungi ${labelText.drop(1)} alle ore svolte",
              disabled <-- state.signal.map(s => TaskProgressForm.addMinutes(s.completed, 0).isEmpty).combineWith(submitting.signal).map(_ || _),
              labelText,
              onClick --> (_ => state.update(s => TaskProgressForm.addMinutes(s.completed, minutes).fold(s)(v => s.copy(completed = v)))),
            )
          },
        ),
        p(cls := "mt-1 text-xs text-slate-400", "Aggiornano solo il campo: premi «Salva» per registrare."),
      )

    val (titleVerb, submitLabel, hintText) = mode match
      case Mode.Progress =>
        ("Aggiorna", "Salva", "Le ore svolte pari o superiori alle ore totali completano il task. La pianificazione si aggiorna in automatico.")
      case Mode.Reactivate =>
        (
          "Riattiva",
          "Riattiva",
          "Indica le ore effettivamente svolte: se inferiori alle ore totali il task torna attivo e rientra nella pianificazione.",
        )

    val header = PageHeader(
      "← Torna alla pianificazione",
      leave,
      Signal.fromValue(s"$titleVerb task «${context.taskName}»"),
      context = List("Pianificazione", (s"Ordine ${context.orderNumber}" :: context.customerName.toList).mkString(" · ")),
      requiredLegend = false,
    )

    formPage(header)(
      draft.bind,
      draft.dialog,
      formColumn("max-w-2xl")(
        contextSection(target, context),
        formSection("Avanzamento", hintText)(
          div(
            cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
            div(
              hoursInput(Field.Completed, "Ore svolte (h:mm)", _.completed, v => state.update(_.copy(completed = v))),
              quickChips,
            ),
            hoursInput(Field.Expected, "Ore totali (h:mm)", _.expected, v => state.update(_.copy(expected = v))),
            if mode == Mode.Progress then
              div(
                cls := "sm:col-span-2",
                Components.field(
                  FieldSpec(
                    "Dipendente preferito",
                    employeeId,
                    hintText = Some(
                      s"Vale per l'intera lavorazione ${context.manufacturingCode}: quando possibile la pianificazione assegna i suoi task a questo dipendente.",
                    ),
                  ),
                ) { bind =>
                  selectInput(state.signal.map(_.employeeId), Observer[String](v => state.update(_.copy(employeeId = v))), employeeOptions)
                    .amend(bind, cls := workshopInputCls)
                },
              )
            else emptyNode,
          ),
          child.maybe <-- submitError.signal.map(_.map(err => div(idAttr := submitErrorId, errorBanner(err)))),
        ),
        stickyActionBar(
          div(
            cls := "flex flex-wrap items-center gap-3",
            if mode == Mode.Progress then
              button(
                tpe := "button",
                cls := s"$btnGhost min-h-12 justify-center",
                disabled <-- submitting.signal,
                "Segna completato",
                onClick --> (_ => markCompleted()),
              )
            else emptyNode,
            dirtyNote(state.signal.map(_ => draft.isDirtyNow)),
          ),
          workshopActions(submitLabel, submitting.signal, leave, () => submit()),
        ),
      ),
    )
  end apply
end PlanningTaskView
