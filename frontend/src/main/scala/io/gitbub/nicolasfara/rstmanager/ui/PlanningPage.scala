package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.{ clearInterval, setInterval, SetIntervalHandle }

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/**
 * Work-planning board: the current plan shown as day columns of task→employee slices, plus a section of completed tasks that can be brought back into
 * planning. This is the focus screen of the app.
 *
 * There is no manual "recalculate" control: the backend recomputes the plan on every order/workforce change, and this page converges on the new plan
 * through the global [[AppBus]] refreshes and a silent poll while it stays mounted.
 */
object PlanningPage:

  /** Interval of the silent background poll that keeps the plan fresh while the page is open. */
  private val pollIntervalMs = 15000

  /** What the task modal is doing: adjusting the progress of a planned task, or reactivating a completed one. */
  private enum EditKind derives CanEqual:
    case Progress, Reactivate

  /** Editable task-modal fields, tracked against the values loaded when the modal opens. */
  private final case class TaskModalState(completed: String, expected: String, employeeId: String) derives CanEqual

  /** Visual treatment for a planned task slice based on how close its day is to the manufacturing deadline. */
  private final case class Urgency(cardCls: String, label: String, labelCls: String)

  /**
   * Maps the days between a slice's scheduled day and its manufacturing deadline to a colour scale: red = late, orange = due today, yellow = due
   * tomorrow, sky = due soon, blue = on time.
   */
  private def urgencyFor(daysToDeadline: Option[Int]): Urgency = daysToDeadline match
    case Some(d) if d < 0 => Urgency("border-rose-300 bg-rose-50", "In ritardo", "bg-rose-100 text-rose-700")
    case Some(0) => Urgency("border-orange-300 bg-orange-50", "Scade oggi", "bg-orange-100 text-orange-700")
    case Some(1) => Urgency("border-amber-300 bg-amber-50", "Scade domani", "bg-amber-100 text-amber-700")
    case Some(d) if d <= 3 => Urgency("border-sky-300 bg-sky-50", s"Tra $d giorni", "bg-sky-100 text-sky-700")
    case Some(_) => Urgency("border-blue-300 bg-blue-50", "In tempo", "bg-blue-100 text-blue-700")
    case None => Urgency("border-slate-200 bg-white", "", "bg-slate-100 text-slate-600")

  /** Groups every slice of the attempt by day, whether the attempt is in progress or completed. */
  private def slicesByDay(state: PlanningStateDto): List[(String, List[ScheduledTaskSliceDto])] =
    state.result match
      case Some(result) => result.schedules.sortBy(_.day).map(schedule => schedule.day -> schedule.slices)
      case None =>
        state.inProgress match
          case Some(inProgress) => inProgress.slices.groupBy(_.day).toList.sortBy(_._1)
          case None => Nil

  private def diagnostics(
      state: PlanningStateDto,
  ): (List[DelayedOrderDto], List[DelayedManufacturingDto], List[UnplannedOrderDto], List[PlanningWarningDto]) =
    state.result
      .map(r => (r.delayedOrders, r.delayedManufacturings, r.unplannedOrders, r.warnings))
      .orElse(state.inProgress.map(ip => (ip.delayedOrders, ip.delayedManufacturings, ip.unplannedOrders, ip.warnings)))
      .getOrElse((Nil, Nil, Nil, Nil))

  /** A completed scheduled task flattened with its order/manufacturing context, for the completed-tasks section. */
  private final case class CompletedRow(order: OrderResponse, manufacturing: ManufacturingResponse, task: ScheduledTaskDto)

  /** Human context for diagnostics that only carry ids. */
  private final case class ManufacturingContext(order: OrderResponse, manufacturing: ManufacturingResponse)

  /** Completed tasks across all non-cancelled orders, most recently completed first. */
  private def completedRows(orders: List[OrderResponse]): List[CompletedRow] =
    val rows =
      for
        order <- orders if order.status != "cancelled"
        manufacturing <- order.manufacturings
        task <- manufacturing.tasks if task.status == "completed"
      yield CompletedRow(order, manufacturing, task)
    rows.sortBy(_.task.completionDate.getOrElse(""))(using Ordering[String].reverse)

  /** Only in-progress and suspended orders accept task edits (see the domain `Order` aggregate). */
  private def isOrderEditable(status: String): Boolean = status == "in_progress" || status == "suspended"

  private def statusLabel(status: String): String = status match
    case "pending" => "In attesa"
    case "in_progress" => "In corso"
    case "suspended" => "Sospeso"
    case "completed" => "Completato"
    case "delivered" => "Consegnato"
    case "cancelled" => "Annullato"
    case "not_started" => "Non iniziata"
    case "paused" => "In pausa"
    case other => other

  private def delayAmount(fromIso: String, toIso: String): String =
    Formats.daysUntil(fromIso, toIso) match
      case Some(1) => "Slitta di 1 giorno"
      case Some(days) if days > 1 => s"Slitta di $days giorni"
      case Some(0) => "Data invariata"
      case Some(days) => s"Anticipa di ${math.abs(days)} giorni"
      case None => "Date da verificare"

  def apply(): HtmlElement =
    // ---- Task modal state (shared by progress editing and reactivation) ---------------------------
    // The scheduled task instance being edited: (orderId, manufacturingId, taskInstanceId).
    val editing = Var(Option.empty[(UUID, UUID, UUID)])
    val editKind = Var(EditKind.Progress)
    val editName = Var("")
    val editState = Var(TaskModalState("", "", ""))
    val editTracker = Var(DirtyTracker(TaskModalState("", "", ""), editState))
    val modalOpen = Var(false)
    val modalError = Var(Option.empty[ApiError])
    val saving = Var(false)

    val weekIndex = Var(0)

    // Private poll tick: only planningData re-fetches on the background poll.
    // ordersData/employeesData/tasksData use their own domain ticks so they react to mutations
    // from other pages, but they are NOT re-fetched on every silent planning poll.
    val pollTick = Var(0)
    val planningTick: Signal[Any] = AppBus.planningTicks.combineWith(pollTick.signal)

    val planningData = loadable(planningTick)(() => ApiClient.currentPlanning())
    val ordersData = loadable(AppBus.ordersTicks)(() => ApiClient.listOrders())
    val employeesData = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val tasksData = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())

    val orderNames: Signal[Map[UUID, String]] = ordersData.map {
      case Some(Right(list)) => list.map(o => o.id -> o.number).toMap
      case _ => Map.empty
    }
    val employeeNames: Signal[Map[UUID, String]] = employeesData.map {
      case Some(Right(list)) => list.map(e => e.id -> s"${e.name} ${e.surname}").toMap
      case _ => Map.empty
    }
    val employeeOptions: Signal[List[(String, String)]] = employeesData.map {
      case Some(Right(list)) => ("" -> "— auto (pianificazione sceglie) —") :: list.map(e => e.id.toString -> s"${e.name} ${e.surname}")
      case _ => List("" -> "—")
    }
    val ordersList: Signal[List[OrderResponse]] = ordersData.map {
      case Some(Right(list)) => list
      case _ => Nil
    }
    val ordersById: Signal[Map[UUID, OrderResponse]] =
      ordersList.map(_.map(order => order.id -> order).toMap)
    val manufacturingContexts: Signal[Map[UUID, ManufacturingContext]] =
      ordersList.map { orders =>
        orders.flatMap(order => order.manufacturings.map(manufacturing => manufacturing.id -> ManufacturingContext(order, manufacturing))).toMap
      }
    // Maps scheduledManufacturingId -> preferredEmployeeId (from the order data).
    val preferredByMfg: Signal[Map[UUID, Option[UUID]]] = ordersList.map { orders =>
      orders.flatMap(_.manufacturings.map(m => m.id -> m.preferredEmployeeId)).toMap
    }
    // Catalog task id -> task name (from the task catalog).
    val catalogNames: Signal[Map[UUID, String]] = tasksData.map {
      case Some(Right(list)) => list.map(t => t.id -> t.name).toMap
      case _ => Map.empty
    }
    // A slice's `manufacturingId` is a scheduled-manufacturing id; its human label is the manufacturing code.
    val manufacturingCodes: Signal[Map[UUID, String]] =
      ordersList.map(orders => orders.flatMap(_.manufacturings.map(m => m.id -> m.code)).toMap)
    // Target completion date per scheduled manufacturing, used to colour its task slices by urgency.
    val manufacturingDeadlines: Signal[Map[UUID, String]] =
      ordersList.map(orders => orders.flatMap(_.manufacturings.map(m => m.id -> m.completionDate)).toMap)
    // A slice's `taskId` is a scheduled-task instance id; resolve it to the catalog task name.
    val scheduledTaskNames: Signal[Map[UUID, String]] =
      Signal.combine(ordersList, catalogNames).map { case (orders, names) =>
        orders.flatMap(_.manufacturings.flatMap(_.tasks.map(t => t.id -> names.getOrElse(t.taskId, Formats.shortId(t.taskId))))).toMap
      }
    // Resolve a slice's scheduled-task instance id to its current progress/estimate, so the modal opens pre-filled.
    val scheduledTaskById: Signal[Map[UUID, ScheduledTaskDto]] =
      ordersList.map(_.flatMap(_.manufacturings.flatMap(_.tasks.map(t => t.id -> t))).toMap)
    val editLookup: Signal[(Map[UUID, ScheduledTaskDto], Map[UUID, String])] =
      scheduledTaskById.combineWith(scheduledTaskNames)
    val editAndPref = editLookup.flatMapSwitch(el => preferredByMfg.map((el, _)))

    // ---- Task modal ------------------------------------------------------------------------------
    def openEdit(slice: ScheduledTaskSliceDto, task: ScheduledTaskDto, name: String, preferred: Option[UUID]): Unit =
      val completed = task.completedHours.getOrElse(0)
      val prefStr = preferred.map(_.toString).getOrElse("")
      val state = TaskModalState(completed.toString, task.expectedHours.toString, prefStr)
      editing.set(Some((slice.orderId, slice.manufacturingId, slice.taskId)))
      editKind.set(EditKind.Progress)
      editName.set(name)
      editState.set(state)
      editTracker.set(DirtyTracker(state, editState))
      modalError.set(None)
      modalOpen.set(true)

    def openReactivate(row: CompletedRow, name: String): Unit =
      val initial = TaskModalState(row.task.completedHours.getOrElse(0).toString, row.task.expectedHours.toString, "")
      editing.set(Some((row.order.id, row.manufacturing.id, row.task.id)))
      editKind.set(EditKind.Reactivate)
      editName.set(name)
      // The task is completed, so its recorded hours cover the estimate: ask for the hours actually done.
      editState.set(initial.copy(completed = ""))
      editTracker.set(DirtyTracker(initial, editState))
      modalError.set(None)
      modalOpen.set(true)

    def closeEdit(): Unit =
      modalOpen.set(false); editing.set(None); saving.set(false)

    def save(): Unit =
      editing.now().foreach { case (orderId, manufacturingId, taskId) =>
        val state = editState.now()
        val tracker = editTracker.now()
        val completed = state.completed.trim.nn.toIntOption
        val expected = state.expected.trim.nn.toIntOption
        val reactivating = editKind.now() == EditKind.Reactivate
        if completed.exists(_ < 0) then showError(modalError, "Aggiornamento task")(ApiError("invalid", "Le ore svolte non possono essere negative.", Nil))
        else if expected.exists(_ < 1) then showError(modalError, "Aggiornamento task")(ApiError("invalid", "Le ore totali devono essere almeno 1.", Nil))
        else if reactivating && !completed.zip(expected).exists((done, total) => done < total) then
          showError(modalError, "Aggiornamento task")(
            ApiError("invalid", "Per riportare il task in lavorazione le ore svolte devono essere inferiori alle ore totali.", Nil),
          )
        else
          val taskRequest = TaskProgressUpdateRequest(
            completed.filter(_ => tracker.changed(_.completed.trim.nn.toIntOption)),
            expected.filter(_ => tracker.changed(_.expected.trim.nn.toIntOption)),
          )
          val employeeChanged = !reactivating && tracker.changed(_.employeeId)
          val hasTaskChanges = taskRequest.completedHours.isDefined || taskRequest.expectedHours.isDefined

          if !hasTaskChanges && !employeeChanged then closeEdit()
          else
            saving.set(true)
            modalError.set(None)
            val taskF: Future[ApiClient.Result[Unit]] =
              if hasTaskChanges then ApiClient.updateScheduledTask(orderId, manufacturingId, taskId, taskRequest).map(_.map(_ => ()))
              else Future.successful(Right(()))
            taskF.flatMap {
              case Left(err) => Future.successful(Left(err))
              case Right(()) =>
                if employeeChanged then
                  val empId = scala.util.Try(UUID.fromString(state.employeeId).nn).toOption
                  ApiClient.setPreferredEmployee(orderId, manufacturingId, empId).map(_.map(_ => ()))
                else Future.successful(Right(()))
            }.foreach {
              case Right(_) => closeEdit(); AppBus.mutatedOrders()
              case Left(err) => saving.set(false); showError(modalError, "Aggiornamento task")(err)
            }
        end if
      }

    /** Quick action: completes the task by aligning the hours done to the total. */
    def markCompleted(): Unit =
      editState.update(state => state.copy(completed = state.expected))
      save()

    val modalTitle = editKind.signal.map {
      case EditKind.Progress => "Aggiorna task"
      case EditKind.Reactivate => "Riattiva task"
    }

    val taskModal =
      modal(modalOpen, modalTitle)(
        div(
          cls := "space-y-3",
          div(cls := "text-sm font-semibold text-slate-800", child.text <-- editName.signal),
          div(
            cls := "grid grid-cols-2 gap-3",
            field("Ore svolte", textInput(editState.signal.map(_.completed), Observer[String](v => editState.update(_.copy(completed = v))), "", "number")),
            field("Ore totali", textInput(editState.signal.map(_.expected), Observer[String](v => editState.update(_.copy(expected = v))), "", "number")),
          ),
          child <-- editKind.signal.map {
            case EditKind.Progress =>
              field("Dipendente preferito", selectInput(editState.signal.map(_.employeeId), Observer[String](v => editState.update(_.copy(employeeId = v))), employeeOptions))
            case EditKind.Reactivate => emptyNode
          },
          p(
            cls := "text-xs text-slate-400",
            child.text <-- editKind.signal.map {
              case EditKind.Progress =>
                "Le ore svolte pari o superiori alle ore totali completano il task. La pianificazione si aggiorna in automatico."
              case EditKind.Reactivate =>
                "Indica le ore effettivamente svolte: se inferiori alle ore totali il task torna attivo e rientra nella pianificazione."
            },
          ),
          child.maybe <-- modalError.signal.map(_.map(errorBanner)),
          div(
            cls := "flex items-center justify-between gap-2 pt-1",
            child <-- editKind.signal.map {
              case EditKind.Progress =>
                button(
                  tpe := "button",
                  cls := btnGhost,
                  disabled <-- saving.signal,
                  "Segna completato",
                  onClick --> (_ => markCompleted()),
                )
              case EditKind.Reactivate => emptyNode
            },
            div(
              cls := "flex gap-2",
              button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => closeEdit())),
              button(
                tpe := "button",
                cls := btnPrimary,
                disabled <-- saving.signal,
                child.text <-- saving.signal.combineWith(editKind.signal).map {
                  case (true, _) => "Salvataggio…"
                  case (false, EditKind.Progress) => "Salva"
                  case (false, EditKind.Reactivate) => "Riattiva"
                },
                onClick --> (_ => save()),
              ),
            ),
          ),
        ),
      )

    // ---- Board -----------------------------------------------------------------------------------
    def sliceCard(slice: ScheduledTaskSliceDto): HtmlElement =
      // Urgency (card colour + badge) derived from the slice day vs its manufacturing deadline.
      val urgency: Signal[Urgency] =
        manufacturingDeadlines.map(deadlines =>
          urgencyFor(deadlines.get(slice.manufacturingId).flatMap(deadline => Formats.daysUntil(slice.day, deadline))),
        )
      // The task's overall progress: (completed, expected) hours.
      val progress: Signal[Option[(Int, Int)]] =
        scheduledTaskById.map(_.get(slice.taskId).map(task => (task.completedHours.getOrElse(0), task.expectedHours)))
      div(
        cls := "rounded-lg border p-2.5 cursor-pointer transition hover:shadow-md hover:ring-1 hover:ring-slate-300",
        cls <-- urgency.map(_.cardCls),
        onClick.compose(_.sample(editAndPref)) --> { case ((byId, names), pref) =>
          byId
            .get(slice.taskId)
            .foreach(task =>
              openEdit(slice, task, names.getOrElse(slice.taskId, Formats.shortId(slice.taskId)), pref.get(slice.manufacturingId).flatten),
            )
        },
        // Order number + hours assigned to this slice
        div(
          cls := "flex items-center justify-between gap-2",
          span(
            cls := "truncate text-xs font-semibold text-slate-700",
            child.text <-- orderNames.map(_.getOrElse(slice.orderId, Formats.shortId(slice.orderId))),
          ),
          span(
            cls := "shrink-0 rounded bg-white/70 px-1.5 py-0.5 text-[10px] font-semibold tabular-nums text-slate-600",
            s"${slice.candidateEmployee.assignedHours}h",
          ),
        ),
        // Task name (prominent) + urgency badge
        div(
          cls := "mt-1.5 flex items-start justify-between gap-2",
          div(
            cls := "text-sm font-semibold leading-snug text-slate-800",
            child.text <-- scheduledTaskNames.map(_.getOrElse(slice.taskId, Formats.shortId(slice.taskId))),
          ),
          child <-- urgency.map { u =>
            if u.label.isEmpty then emptyNode
            else span(cls := s"shrink-0 rounded-full px-1.5 py-0.5 text-[10px] font-semibold ${u.labelCls}", u.label)
          },
        ),
        // Associated manufacturing (its code)
        div(
          cls := "mt-0.5 flex items-baseline gap-1 text-xs",
          span(cls := "text-slate-400", "Lavorazione"),
          span(
            cls := "truncate font-medium text-slate-600",
            child.text <-- manufacturingCodes.map(_.getOrElse(slice.manufacturingId, Formats.shortId(slice.manufacturingId))),
          ),
        ),
        // Task progress: completed/total hours + a small bar
        div(
          cls := "mt-2",
          div(
            cls := "flex items-center justify-between text-xs",
            span(cls := "text-slate-400", "Ore"),
            span(
              cls := "font-semibold tabular-nums text-slate-700",
              child.text <-- progress.map {
                case Some((completed, expected)) => s"$completed/${expected}h"
                case None => "—"
              },
            ),
          ),
          div(
            cls := "mt-1 h-1.5 w-full overflow-hidden rounded-full bg-white/70",
            div(
              cls := "h-full rounded-full bg-slate-500/70",
              width <-- progress.map {
                case Some((completed, expected)) if expected > 0 => s"${math.min(100, math.max(0, completed * 100 / expected))}%"
                case _ => "0%"
              },
            ),
          ),
        ),
        // Employee assignment + projected remaining hours
        div(
          cls := "mt-2 flex items-center justify-between gap-2 border-t border-white/60 pt-1.5 text-xs",
          span(
            cls := "flex min-w-0 items-center gap-1 text-slate-600",
            span(cls := "text-slate-400", "◍"),
            span(
              cls := "truncate",
              child.text <-- employeeNames.map(_.getOrElse(slice.candidateEmployee.employeeId, Formats.shortId(slice.candidateEmployee.employeeId))),
            ),
          ),
          span(cls := "shrink-0 tabular-nums text-slate-400", s"resta ${slice.remainingHoursAfterSlice}h"),
        ),
      )
    end sliceCard

    def dayColumn(day: String, slices: List[ScheduledTaskSliceDto]): HtmlElement =
      div(
        cls := "min-w-0",
        div(
          cls := "mb-2 flex items-baseline justify-between border-b border-slate-200 pb-1",
          span(cls := "text-sm font-semibold text-slate-700", Formats.date(day)),
          if slices.nonEmpty then span(cls := "text-xs text-slate-400", s"${slices.size}") else emptyNode,
        ),
        if slices.isEmpty then div(cls := "py-4 text-center text-xs text-slate-300", "—")
        else div(cls := "space-y-2", slices.map(sliceCard)),
      )

    def board(state: PlanningStateDto): HtmlElement =
      val days = slicesByDay(state)
      if days.isEmpty then
        emptyState("Nessun task pianificato. La pianificazione si ricalcola automaticamente quando ordini, task o forza lavoro cambiano.")
      else
        // Normalize each day to a YYYY-MM-DD key so that weekKey and weekDays align regardless of the
        // timezone or time component in the backend-produced datetime strings.
        val byWeek: List[(String, Map[String, List[ScheduledTaskSliceDto]])] =
          days
            .map { case (day, slices) => Formats.dateKey(day) -> slices }
            .groupBy { case (dk, _) => Formats.weekKey(dk) }
            .toList
            .sortBy(_._1)
            .map { case (wk, pairs) => wk -> pairs.toMap }

        def renderWeekGrid(wk: String, slicesByDate: Map[String, List[ScheduledTaskSliceDto]]): HtmlElement =
          div(
            cls := "grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 gap-4",
            Formats.weekDays(wk).map(dk => dayColumn(dk, slicesByDate.getOrElse(dk, Nil))),
          )

        if byWeek.size <= 1 then
          val (wk, slicesByDate) = byWeek.head
          renderWeekGrid(wk, slicesByDate)
        else
          val totalWeeks = byWeek.size
          val clampedIdx: Signal[Int] = weekIndex.signal.map(i => math.min(i, totalWeeks - 1))
          val weekRangeLabel: Signal[String] = clampedIdx.map { i =>
            val (wk, _) = byWeek(i)
            val wd = Formats.weekDays(wk)
            s"${Formats.date(wd.head)} – ${Formats.date(wd.last)}"
          }
          div(
            cls := "space-y-3",
            div(
              cls := "flex items-center justify-between gap-4",
              button(
                tpe := "button",
                cls := s"$btnSmall disabled:opacity-40 disabled:cursor-not-allowed",
                "← Settimana prec.",
                disabled <-- clampedIdx.map(_ <= 0),
                onClick --> (_ => weekIndex.update(i => math.max(0, i - 1))),
              ),
              div(
                cls := "flex flex-col items-center gap-0.5",
                span(cls := "text-sm font-semibold text-slate-700", child.text <-- weekRangeLabel),
                span(
                  cls := "text-xs text-slate-400",
                  child.text <-- clampedIdx.map(i => s"Settimana ${i + 1} di $totalWeeks"),
                ),
              ),
              button(
                tpe := "button",
                cls := s"$btnSmall disabled:opacity-40 disabled:cursor-not-allowed",
                "Settimana succ. →",
                disabled <-- clampedIdx.map(_ >= totalWeeks - 1),
                onClick --> (_ => weekIndex.update(i => math.min(totalWeeks - 1, i + 1))),
              ),
            ),
            child <-- clampedIdx.map { i =>
              val (wk, slicesByDate) = byWeek(i)
              renderWeekGrid(wk, slicesByDate)
            },
          )
        end if
      end if
    end board

    // ---- Completed tasks -------------------------------------------------------------------------
    def completedRow(row: CompletedRow): HtmlElement =
      tr(
        cls := "border-b border-slate-100 last:border-0",
        td(
          cls := "px-4 py-2 font-medium text-slate-800",
          child.text <-- catalogNames.map(_.getOrElse(row.task.taskId, Formats.shortId(row.task.taskId))),
        ),
        td(cls := "px-4 py-2 text-slate-600", row.order.number),
        td(cls := "px-4 py-2 text-slate-500", row.manufacturing.code),
        td(cls := "px-4 py-2 tabular-nums text-slate-600", s"${row.task.completedHours.getOrElse(0)}/${row.task.expectedHours}h"),
        td(cls := "px-4 py-2 text-slate-500", row.task.completionDate.map(Formats.date).getOrElse("—")),
        td(
          cls := "px-4 py-2 text-right",
          if isOrderEditable(row.order.status) then
            button(
              tpe := "button",
              cls := btnSmall,
              "Riattiva",
              onClick.compose(_.sample(catalogNames)) --> { names =>
                openReactivate(row, names.getOrElse(row.task.taskId, Formats.shortId(row.task.taskId)))
              },
            )
          else span(cls := "text-xs text-slate-400", "ordine non modificabile"),
        ),
      )

    def completedCard(row: CompletedRow): HtmlElement =
      div(
        cls := "border-b border-slate-100 last:border-0 px-4 py-3 space-y-1.5",
        div(
          cls := "text-sm font-medium text-slate-800",
          child.text <-- catalogNames.map(_.getOrElse(row.task.taskId, Formats.shortId(row.task.taskId))),
        ),
        div(
          cls := "flex flex-wrap gap-x-4 gap-y-0.5 text-xs",
          span(span(cls := "text-slate-400", "Ordine "), span(cls := "text-slate-600", row.order.number)),
          span(span(cls := "text-slate-400", "Lavorazione "), span(cls := "text-slate-600", row.manufacturing.code)),
        ),
        div(
          cls := "flex flex-wrap gap-x-4 gap-y-0.5 text-xs",
          span(
            span(cls := "text-slate-400", "Ore "),
            span(cls := "tabular-nums text-slate-600", s"${row.task.completedHours.getOrElse(0)}/${row.task.expectedHours}h"),
          ),
          span(
            span(cls := "text-slate-400", "Completato "),
            span(cls := "text-slate-600", row.task.completionDate.map(Formats.date).getOrElse("—")),
          ),
        ),
        if isOrderEditable(row.order.status) then
          button(
            tpe := "button",
            cls := btnSmall,
            "Riattiva",
            onClick.compose(_.sample(catalogNames)) --> { names =>
              openReactivate(row, names.getOrElse(row.task.taskId, Formats.shortId(row.task.taskId)))
            },
          )
        else span(cls := "text-xs text-slate-400", "ordine non modificabile"),
      )

    def completedSection(orders: List[OrderResponse]): HtmlElement =
      val rows = completedRows(orders)
      card(
        div(
          cls := "flex items-center justify-between border-b border-slate-100 px-4 py-3",
          div(
            div(cls := "text-sm font-semibold text-slate-800", s"Task completati (${rows.size})"),
            div(cls := "text-xs text-slate-400", "Un task completato per errore può essere riattivato: torna tra i task pianificati."),
          ),
        ),
        if rows.isEmpty then div(cls := "p-4", emptyState("Nessun task completato."))
        else
          div(
            // Mobile: card per ogni riga
            div(cls := "sm:hidden", rows.map(completedCard)),
            // Desktop: tabella
            div(
              cls := "hidden sm:block overflow-x-auto",
              table(
                cls := "w-full text-sm",
                thead(
                  cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                  tr(
                    th(cls := "px-4 py-2", "Task"),
                    th(cls := "px-4 py-2", "Ordine"),
                    th(cls := "px-4 py-2", "Lavorazione"),
                    th(cls := "px-4 py-2", "Ore"),
                    th(cls := "px-4 py-2", "Completato il"),
                    th(cls := "px-4 py-2"),
                  ),
                ),
                tbody(rows.map(completedRow)),
              ),
            ),
          ),
      )
    end completedSection

    // ---- Diagnostics -----------------------------------------------------------------------------
    def delayMetric(label: String, value: String): HtmlElement =
      span(
        cls := "inline-flex min-w-0 items-baseline gap-1 rounded-md bg-white/70 px-1.5 py-0.5 text-[11px] leading-tight",
        span(cls := "shrink-0 text-slate-400", label),
        span(cls := "truncate font-medium text-slate-700", value),
      )

    def datePair(fromLabel: String, fromIso: String, toLabel: String, toIso: String): HtmlElement =
      div(
        cls := "flex flex-wrap items-center gap-x-2 gap-y-1 rounded-md bg-white/75 px-2 py-1 text-[11px] leading-tight",
        span(cls := "font-medium text-slate-500", fromLabel),
        span(cls := "tabular-nums text-slate-700", Formats.date(fromIso)),
        span(cls := "text-slate-300", "->"),
        span(cls := "font-medium text-slate-500", toLabel),
        span(cls := "tabular-nums font-semibold text-slate-900", Formats.date(toIso)),
      )

    def delayedOrderCard(delay: DelayedOrderDto, orders: Map[UUID, OrderResponse]): HtmlElement =
      val order = orders.get(delay.orderId)
      div(
        cls := "rounded-md border border-amber-200 bg-white/80 p-2 shadow-sm",
        div(
          cls := "flex items-start justify-between gap-2",
          div(
            cls := "min-w-0 flex-1",
            div(
              cls := "flex flex-wrap items-baseline gap-x-2 gap-y-0.5",
              span(cls := "text-[11px] font-semibold uppercase tracking-wide text-amber-700", "Ordine"),
              span(
                cls := "min-w-0 break-words text-sm font-semibold leading-snug text-slate-900",
                order.map(_.number).getOrElse(Formats.shortId(delay.orderId)),
              ),
            ),
            order.flatMap(_.description).map(description => div(cls := "mt-0.5 break-words text-xs leading-snug text-slate-600", description)).getOrElse(emptyNode),
          ),
          div(
            cls := "shrink-0 rounded-full bg-amber-100 px-2 py-0.5 text-[11px] font-semibold text-amber-800",
            delayAmount(delay.expectedDeliveryDate, delay.promisedDeliveryDate),
          ),
        ),
        div(
          cls := "mt-2 flex flex-wrap gap-1.5",
          delayMetric("Stato", order.map(o => statusLabel(o.status)).getOrElse("Non trovato")),
          delayMetric("Priorità", order.map(_.priority).getOrElse("—")),
          delayMetric("Lavorazioni", order.map(_.manufacturings.size.toString).getOrElse("—")),
        ),
        div(cls := "mt-2", datePair("Deadline", delay.expectedDeliveryDate, "Pianificata", delay.promisedDeliveryDate)),
      )
    end delayedOrderCard

    def delayedManufacturingCard(delay: DelayedManufacturingDto, contexts: Map[UUID, ManufacturingContext]): HtmlElement =
      val context = contexts.get(delay.manufacturingId)
      val manufacturing = context.map(_.manufacturing)
      val order = context.map(_.order)
      val taskCount = manufacturing.map(_.tasks.size)
      val expectedHours = manufacturing.map(_.tasks.map(_.expectedHours).sum)
      div(
        cls := "rounded-md border border-orange-200 bg-white/80 p-2 shadow-sm",
        div(
          cls := "flex items-start justify-between gap-2",
          div(
            cls := "min-w-0 flex-1",
            div(
              cls := "flex flex-wrap items-baseline gap-x-2 gap-y-0.5",
              span(cls := "text-[11px] font-semibold uppercase tracking-wide text-orange-700", "Lavorazione"),
              span(
                cls := "min-w-0 break-words text-sm font-semibold leading-snug text-slate-900",
                manufacturing.map(_.code).getOrElse(Formats.shortId(delay.manufacturingId)),
              ),
            ),
            div(cls := "mt-0.5 text-xs leading-snug text-slate-600", order.map(o => s"Ordine ${o.number}").getOrElse(s"Ordine ${Formats.shortId(delay.orderId)}")),
            manufacturing
              .flatMap(_.description)
              .map(description => div(cls := "mt-0.5 break-words text-xs leading-snug text-slate-600", description))
              .getOrElse(emptyNode),
          ),
          div(
            cls := "shrink-0 rounded-full bg-orange-100 px-2 py-0.5 text-[11px] font-semibold text-orange-800",
            delayAmount(delay.expectedCompletionDate, delay.computedCompletionDate),
          ),
        ),
        div(
          cls := "mt-2 flex flex-wrap gap-1.5",
          delayMetric("Stato", manufacturing.map(m => statusLabel(m.status)).getOrElse("Non trovata")),
          delayMetric("Task", taskCount.map(_.toString).getOrElse("—")),
          delayMetric("Ore", expectedHours.map(hours => s"${hours}h").getOrElse("—")),
        ),
        div(cls := "mt-2", datePair("Deadline", delay.expectedCompletionDate, "Stima", delay.computedCompletionDate)),
      )
    end delayedManufacturingCard

    def delaysSection(state: PlanningStateDto): HtmlElement =
      val (delayedOrders, delayedManufacturings, _, _) = diagnostics(state)
      div(
        child <-- ordersById.combineWith(manufacturingContexts).map { case (orders, contexts) =>
          val cards = delayedOrders.map(delayedOrderCard(_, orders)) ++ delayedManufacturings.map(delayedManufacturingCard(_, contexts))
          if cards.isEmpty then emptyNode
          else
            card(
              cls := "overflow-hidden border-amber-200 bg-amber-50/50",
              div(
                cls := "flex flex-wrap items-center justify-between gap-2 border-b border-amber-200/70 bg-amber-100/50 px-3 py-2",
                div(cls := "text-sm font-semibold text-amber-900", "Ritardi da verificare"),
                div(
                  cls := "flex items-center gap-2 text-xs text-amber-800",
                  span(cls := "rounded-full bg-amber-200/70 px-2 py-0.5 font-semibold text-amber-900", cards.size.toString),
                  span("Oltre deadline o stima corrente"),
                ),
              ),
              div(cls := "grid grid-cols-1 gap-2 p-2 lg:grid-cols-2 2xl:grid-cols-3", cards),
            )
        },
      )

    def panel(title: String, items: List[String], tone: String): com.raquo.laminar.nodes.ChildNode.Base =
      if items.isEmpty then emptyNode
      else
        card(
          cls := "p-3",
          div(cls := s"text-xs font-semibold uppercase tracking-wide $tone", s"$title (${items.size})"),
          ul(cls := "mt-1 space-y-0.5 text-xs text-slate-600", items.map(item => li(item))),
        )

    def diagnosticsSection(state: PlanningStateDto): HtmlElement =
      val (_, _, unplannedOrders, warnings) = diagnostics(state)
      val unplannedItems = unplannedOrders.flatMap(o => o.blockedTasks.map(t => s"${Formats.shortId(o.orderId)} · ${t.reason.message}"))
      div(
        cls := "grid gap-3 md:grid-cols-2",
        panel("Task non pianificabili", unplannedItems, "text-rose-600"),
        panel("Avvisi", warnings.map(_.message), "text-slate-500"),
      )

    // Silent background poll: only planningData is refreshed (via pollTick, not AppBus.refresh),
    // so the poll does not cascade to orders/employees/tasks on other mounted pages.
    var pollHandle = Option.empty[SetIntervalHandle]

    div(
      cls := "space-y-4",
      onMountUnmountCallback(
        mount = _ => pollHandle = Some(setInterval(pollIntervalMs.toDouble)(pollTick.update(_ + 1))),
        unmount = _ =>
          pollHandle.foreach(clearInterval); pollHandle = None,
      ),
      taskModal,
      sectionTitle("Pianificazione dei lavori"),
      renderResult(planningData) { state =>
        div(
          cls := "space-y-4",
          delaysSection(state),
          panel("Errori di dominio", state.errors.map(e => e.message), "text-rose-600"),
          card(
            div(
              cls := "border-b border-slate-100 px-4 py-3",
              div(cls := "text-sm font-semibold text-slate-800", "Pianificazione attuale"),
              div(cls := "text-xs text-slate-400", "Clicca un task per aggiornare ore svolte e ore totali, o per segnarlo completato."),
            ),
            div(cls := "p-4", board(state)),
          ),
          diagnosticsSection(state),
        )
      },
      child <-- ordersList.map(completedSection),
    )
  end apply
end PlanningPage
