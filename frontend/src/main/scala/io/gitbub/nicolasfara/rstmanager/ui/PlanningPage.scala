package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.setTimeout
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Work-planning board: the current planning attempt shown as day columns of task→employee slices,
  * plus a form to trigger a new planning attempt. This is the focus screen of the app.
  */
object PlanningPage:

  private val triggerOptions = List(
    "daily_planning" -> "Pianificazione giornaliera",
    "workforce_capacity_changed" -> "Capacità forza lavoro",
    "manual_recovery" -> "Recupero manuale",
    "order_changed" -> "Ordine modificato",
  )

  private def toIso(day: String): String = if day.isEmpty then "" else s"${day}T00:00:00.000Z"
  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** Groups every slice of the attempt by day, whether the attempt is in progress or completed. */
  private def slicesByDay(state: PlanningStateDto): List[(String, List[ScheduledTaskSliceDto])] =
    state.result match
      case Some(result) => result.schedules.sortBy(_.day).map(schedule => schedule.day -> schedule.slices)
      case None =>
        state.inProgress match
          case Some(inProgress) => inProgress.slices.groupBy(_.day).toList.sortBy(_._1)
          case None             => Nil

  private def diagnostics(
      state: PlanningStateDto,
  ): (List[DelayedOrderDto], List[DelayedManufacturingDto], List[UnplannedOrderDto], List[PlanningWarningDto]) =
    state.result
      .map(r => (r.delayedOrders, r.delayedManufacturings, r.unplannedOrders, r.warnings))
      .orElse(state.inProgress.map(ip => (ip.delayedOrders, ip.delayedManufacturings, ip.unplannedOrders, ip.warnings)))
      .getOrElse((Nil, Nil, Nil, Nil))

  def apply(): HtmlElement =
    val tick = Var(0)
    val pageError = Var(Option.empty[ApiError])

    // ---- Task-update modal state -----------------------------------------------------------------
    // The scheduled task instance being edited: (orderId, manufacturingId, taskInstanceId).
    val editing = Var(Option.empty[(UUID, UUID, UUID)])
    val editName = Var("")
    val editCompleted = Var("")
    val editExpected = Var("")
    val origCompleted = Var(0)
    val origExpected = Var(0)
    val modalOpen = Var(false)
    val modalError = Var(Option.empty[ApiError])
    val saving = Var(false)

    val planningData = loadable(tick.signal)(() => ApiClient.currentPlanning())
    val ordersData = loadable(tick.signal)(() => ApiClient.listOrders())
    val employeesData = loadable(tick.signal)(() => ApiClient.listEmployees())
    val tasksData = loadable(tick.signal)(() => ApiClient.listTasks())

    val orderNames: Signal[Map[UUID, String]] = ordersData.map {
      case Some(Right(list)) => list.map(o => o.id -> o.number).toMap
      case _                 => Map.empty
    }
    val employeeNames: Signal[Map[UUID, String]] = employeesData.map {
      case Some(Right(list)) => list.map(e => e.id -> s"${e.name} ${e.surname}").toMap
      case _                 => Map.empty
    }
    val ordersList: Signal[List[OrderResponse]] = ordersData.map {
      case Some(Right(list)) => list
      case _                 => Nil
    }
    // Catalog task id -> task name (from the task catalog).
    val catalogNames: Signal[Map[UUID, String]] = tasksData.map {
      case Some(Right(list)) => list.map(t => t.id -> t.name).toMap
      case _                 => Map.empty
    }
    // A slice's `manufacturingId` is a scheduled-manufacturing id; its human label is the manufacturing code.
    val manufacturingCodes: Signal[Map[UUID, String]] =
      ordersList.map(orders => orders.flatMap(_.manufacturings.map(m => m.id -> m.code)).toMap)
    // A slice's `taskId` is a scheduled-task instance id; resolve it to the catalog task name.
    val scheduledTaskNames: Signal[Map[UUID, String]] =
      Signal.combine(ordersList, catalogNames).map { case (orders, names) =>
        orders.flatMap(_.manufacturings.flatMap(_.tasks.map(t => t.id -> names.getOrElse(t.taskId, Formats.shortId(t.taskId))))).toMap
      }
    val orderOptions: Signal[List[(String, String)]] = ordersData.map {
      case Some(Right(list)) => ("" -> "— seleziona ordine —") :: list.map(o => o.id.toString -> o.number)
      case _                 => List("" -> "—")
    }
    // Resolve a slice's scheduled-task instance id to its current progress/estimate, so the modal opens pre-filled.
    val scheduledTaskById: Signal[Map[UUID, ScheduledTaskDto]] =
      ordersList.map(_.flatMap(_.manufacturings.flatMap(_.tasks.map(t => t.id -> t))).toMap)
    val editLookup: Signal[(Map[UUID, ScheduledTaskDto], Map[UUID, String])] =
      scheduledTaskById.combineWith(scheduledTaskNames)

    // ---- Attempt form ----------------------------------------------------------------------------
    val startOn = Var("")
    val triggerKind = Var("daily_planning")
    val triggerOrderId = Var("")

    def launchAttempt(): Unit =
      val trigger = triggerKind.now() match
        case "order_changed" => PlanningTriggerDto("order_changed", parseUuid(triggerOrderId.now()), None, None)
        case other           => PlanningTriggerDto(other, None, None, None)
      val request = PlanningAttemptRequest(None, toIso(startOn.now()), trigger, None, None, None)
      ApiClient.createPlanningAttempt(request).foreach {
        case Right(_)  => pageError.set(None); tick.update(_ + 1)
        case Left(err) => pageError.set(Some(err))
      }

    val attemptForm =
      card(
        cls := "p-4",
        div(
          cls := "flex flex-wrap items-end gap-3",
          div(cls := "w-44", field("Data inizio", textInput(startOn, inputType = "date"))),
          div(cls := "w-56", field("Trigger", staticSelect(triggerKind, triggerOptions))),
          child <-- triggerKind.signal.map {
            case "order_changed" => div(cls := "w-56", field("Ordine", selectInput(triggerOrderId, orderOptions)))
            case _               => emptyNode
          },
          button(tpe := "button", cls := btnPrimary, "Calcola pianificazione", onClick --> (_ => launchAttempt())),
          button(tpe := "button", cls := btnGhost, "Aggiorna", onClick --> (_ => tick.update(_ + 1))),
        ),
        child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      )

    // ---- Task-update modal -----------------------------------------------------------------------
    def openEdit(slice: ScheduledTaskSliceDto, task: ScheduledTaskDto, name: String): Unit =
      val completed = task.completedHours.getOrElse(0)
      editing.set(Some((slice.orderId, slice.manufacturingId, slice.taskId)))
      editName.set(name)
      editCompleted.set(completed.toString)
      editExpected.set(task.expectedHours.toString)
      origCompleted.set(completed)
      origExpected.set(task.expectedHours)
      modalError.set(None)
      modalOpen.set(true)

    def closeEdit(): Unit =
      modalOpen.set(false); editing.set(None); saving.set(false)

    def save(): Unit =
      editing.now().foreach { case (orderId, manufacturingId, taskId) =>
        val completed = editCompleted.now().trim.nn.toIntOption
        val expected = editExpected.now().trim.nn.toIntOption
        if completed.exists(_ < 0) then modalError.set(Some(ApiError("invalid", "Le ore svolte non possono essere negative.", Nil)))
        else if expected.exists(_ < 1) then modalError.set(Some(ApiError("invalid", "Le ore totali devono essere almeno 1.", Nil)))
        else
          // Send only the fields the user actually changed, so we don't emit redundant events.
          val request = TaskProgressUpdateRequest(
            completed.filter(_ != origCompleted.now()),
            expected.filter(_ != origExpected.now()),
          )
          if request.completedHours.isEmpty && request.expectedHours.isEmpty then closeEdit()
          else
            saving.set(true)
            modalError.set(None)
            ApiClient.updateScheduledTask(orderId, manufacturingId, taskId, request).foreach {
              case Right(_) =>
                closeEdit()
                tick.update(_ + 1)
                // Recalculation runs asynchronously on the backend; refresh again shortly to pick up the new plan.
                setTimeout(1200)(tick.update(_ + 1))
              case Left(err) => saving.set(false); modalError.set(Some(err))
            }
      }

    val taskModal =
      modal(modalOpen, "Aggiorna task")(
        div(
          cls := "space-y-3",
          div(cls := "text-sm font-semibold text-slate-800", child.text <-- editName.signal),
          div(
            cls := "grid grid-cols-2 gap-3",
            field("Ore svolte", textInput(editCompleted, inputType = "number")),
            field("Ore totali", textInput(editExpected, inputType = "number")),
          ),
          p(cls := "text-xs text-slate-400", "Le ore svolte pari o superiori alle ore totali completano il task. La pianificazione si aggiorna in automatico."),
          child.maybe <-- modalError.signal.map(_.map(errorBanner)),
          div(
            cls := "flex justify-end gap-2 pt-1",
            button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => closeEdit())),
            button(
              tpe := "button",
              cls := btnPrimary,
              disabled <-- saving.signal,
              child.text <-- saving.signal.map(s => if s then "Salvataggio…" else "Salva"),
              onClick --> (_ => save()),
            ),
          ),
        ),
      )

    // ---- Board -----------------------------------------------------------------------------------
    def sliceCard(slice: ScheduledTaskSliceDto): HtmlElement =
      div(
        cls := s"rounded-lg border p-2.5 ${Formats.colorFor(slice.orderId)}",
        cls := "cursor-pointer transition hover:shadow-md hover:ring-1 hover:ring-slate-300",
        onClick.compose(_.sample(editLookup)) --> { case (byId, names) =>
          byId.get(slice.taskId).foreach(task => openEdit(slice, task, names.getOrElse(slice.taskId, Formats.shortId(slice.taskId))))
        },
        // Order number + hours assigned to this slice
        div(
          cls := "flex items-center justify-between gap-2",
          span(cls := "truncate text-xs font-semibold", child.text <-- orderNames.map(_.getOrElse(slice.orderId, Formats.shortId(slice.orderId)))),
          span(cls := "shrink-0 rounded bg-white/70 px-1.5 py-0.5 text-[10px] font-semibold tabular-nums text-slate-600", s"${slice.candidateEmployee.assignedHours}h"),
        ),
        // Task name (prominent)
        div(cls := "mt-1.5 text-sm font-semibold leading-snug text-slate-800", child.text <-- scheduledTaskNames.map(_.getOrElse(slice.taskId, Formats.shortId(slice.taskId)))),
        // Associated manufacturing (its code)
        div(
          cls := "mt-0.5 flex items-baseline gap-1 text-xs",
          span(cls := "text-slate-400", "Lavorazione"),
          span(cls := "truncate font-medium text-slate-600", child.text <-- manufacturingCodes.map(_.getOrElse(slice.manufacturingId, Formats.shortId(slice.manufacturingId)))),
        ),
        // Employee assignment + projected remaining hours
        div(
          cls := "mt-2 flex items-center justify-between gap-2 border-t border-white/60 pt-1.5 text-xs",
          span(cls := "flex min-w-0 items-center gap-1 text-slate-600", span(cls := "text-slate-400", "◍"), span(cls := "truncate", child.text <-- employeeNames.map(_.getOrElse(slice.candidateEmployee.employeeId, Formats.shortId(slice.candidateEmployee.employeeId))))),
          span(cls := "shrink-0 tabular-nums text-slate-400", s"resta ${slice.remainingHoursAfterSlice}h"),
        ),
      )

    def dayColumn(day: String, slices: List[ScheduledTaskSliceDto]): HtmlElement =
      div(
        cls := "w-64 shrink-0",
        div(
          cls := "mb-2 flex items-baseline justify-between border-b border-slate-200 pb-1",
          span(cls := "text-sm font-semibold text-slate-700", Formats.date(day)),
          span(cls := "text-xs text-slate-400", s"${slices.size}"),
        ),
        div(cls := "space-y-2", slices.map(sliceCard)),
      )

    def board(state: PlanningStateDto): HtmlElement =
      val days = slicesByDay(state)
      if days.isEmpty then emptyState("Nessuno slice pianificato in questo attempt.")
      else div(cls := "flex gap-4 overflow-x-auto pb-2", days.map((day, slices) => dayColumn(day, slices)))

    def panel(title: String, items: List[String], tone: String): com.raquo.laminar.nodes.ChildNode.Base =
      if items.isEmpty then emptyNode
      else
        card(
          cls := "p-3",
          div(cls := s"text-xs font-semibold uppercase tracking-wide $tone", s"$title (${items.size})"),
          ul(cls := "mt-1 space-y-0.5 text-xs text-slate-600", items.map(item => li(item))),
        )

    def statusHeader(state: PlanningStateDto): HtmlElement =
      val when = state.completedOn.orElse(state.rejectedOn).map(Formats.dateTime).getOrElse("—")
      div(
        cls := "flex flex-wrap items-center gap-3",
        statusBadge(state.status),
        span(cls := "text-xs text-slate-400", s"versione ${state.version}"),
        span(cls := "text-xs text-slate-400", when),
      )

    def diagnosticsSection(state: PlanningStateDto): HtmlElement =
      val (delayedOrders, delayedManufacturings, unplannedOrders, warnings) = diagnostics(state)
      val unplannedItems = unplannedOrders.flatMap(o => o.blockedTasks.map(t => s"${Formats.shortId(o.orderId)} · ${t.reason.message}"))
      val delayedOrderItems = delayedOrders.map(d => s"${Formats.shortId(d.orderId)}: previsto ${Formats.date(d.expectedDeliveryDate)} → promesso ${Formats.date(d.promisedDeliveryDate)}")
      val delayedMfgItems = delayedManufacturings.map(d => s"${Formats.shortId(d.manufacturingId)}: ${Formats.date(d.expectedCompletionDate)} → ${Formats.date(d.computedCompletionDate)}")
      div(
        cls := "grid gap-3 md:grid-cols-2 xl:grid-cols-4",
        panel("Ordini in ritardo", delayedOrderItems, "text-amber-600"),
        panel("Lavorazioni in ritardo", delayedMfgItems, "text-amber-600"),
        panel("Task non pianificabili", unplannedItems, "text-rose-600"),
        panel("Avvisi", warnings.map(_.message), "text-slate-500"),
      )

    div(
      cls := "space-y-4",
      taskModal,
      sectionTitle("Pianificazione dei lavori"),
      attemptForm,
      renderResult(planningData) { state =>
        div(
          cls := "space-y-4",
          statusHeader(state),
          panel("Errori di dominio", state.errors.map(e => e.message), "text-rose-600"),
          card(cls := "p-4", board(state)),
          diagnosticsSection(state),
        )
      },
    )
end PlanningPage
