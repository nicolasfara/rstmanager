package io.github.nicolasfara.rstmanager.planning

import scala.annotation.tailrec

import io.github.nicolasfara.rstmanager.hr.domain.{ DailyHours, Employee, EmployeeId }
import io.github.nicolasfara.rstmanager.planning.PlanningError.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencyError.CycleDetected
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.NonEmptyList
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime

/**
 * Facts produced by one scheduling run.
 *
 * The outcome is pure planner output: it carries the day-by-day task slices plus the delays and warnings derived from the same run. It is meant to be
 * recorded on the [[Planning]] aggregate, which derives the final [[PlanningResult]] from these accepted facts.
 */
final case class SchedulingOutcome(
    slices: List[ScheduledTaskSlice],
    delayedOrders: List[DelayedOrder],
    delayedManufacturings: List[DelayedManufacturing],
    warnings: List[PlanningWarning],
)

/**
 * Domain service computing when each task must be performed inside a planning window.
 *
 * The scheduler combines work demand with HR capacity, week by week:
 *   - production days are the Monday-to-Friday days inside the planning window;
 *   - each employee contributes, for every production day, the daily hours derived from the weekly budget and calendar overrides
 *     ([[io.github.nicolasfara.rstmanager.hr.domain.BudgetHours.getWorkingHoursForDay]]), skipping employees whose contract is not active on that
 *     day;
 *   - orders are visited in [[PlanningPriorityPolicy]] sequence, so urgent and earlier-due orders consume capacity first;
 *   - inside a manufacturing, tasks are visited in dependency order and a dependent task may start only on the day after all its prerequisites are
 *     fully scheduled (day granularity keeps intra-day ordering out of the model);
 *   - a task requiring more hours than one day offers is split into multiple [[ScheduledTaskSlice]] values over consecutive days and, when useful,
 *     over multiple employees on the same day.
 *
 * The algorithm is greedy and deterministic: given the same request, orders, and employees it always produces the same schedule. When the remaining
 * work does not fit the window capacity the whole run fails with [[PlanningError.InsufficientCapacity]]; work that fits the window but overruns an
 * expected date is reported as a [[DelayedOrder]] or [[DelayedManufacturing]] instead.
 */
object SchedulingService:
  /** Share of window capacity that, once required, triggers a tight-capacity warning. */
  private val tightCapacityRatio: Double = 0.9

  /**
   * Computes the schedule for a planning request.
   *
   * @param request
   *   Planning request bounding the window and selecting the orders to plan.
   * @param orders
   *   Candidate orders; only in-progress orders listed in `request.openOrderIds` are planned.
   * @param employees
   *   Workforce providing daily capacity inside the window.
   * @return
   *   The computed [[SchedulingOutcome]], or every [[PlanningError]] that made the request infeasible.
   */
  def computeSchedule(
      request: PlanningRequest,
      orders: List[Order],
      employees: List[Employee],
  ): Either[NonEmptyList[PlanningError], SchedulingOutcome] =
    val openOrders = PlanningPriorityPolicy.sortOpenOrders(orders).filter(order => request.openOrderIds.contains(order.data.id))
    val days = productionDays(request.window)
    val baseCapacity = days.map(dailyCapacity(_, employees))
    val requiredHours = openOrders.flatMap(_.data.setOfManufacturing.toList).map(_.remainingHours.value).sum
    val availableHours = baseCapacity.map(_.values.map(_.value).sum).sum

    val initialState = PlannerState(baseCapacity.map(_.view.mapValues(_.value).toMap), Vector.empty, Vector.empty, Vector.empty)
    val planned = openOrders.foldLeft(initialState)(planOrder(_, _, request.window, days, baseCapacity))

    val capacityError = Option.when(planned.unplaced.nonEmpty) {
      InsufficientCapacity(
        request.window,
        TaskHours.applyUnsafe(requiredHours),
        TaskHours.applyUnsafe(availableHours),
        planned.unplaced.map(_._1).distinct.toList,
        planned.unplaced.map(_._2).distinct.toList,
      )
    }

    NonEmptyList.fromList((planned.errors ++ capacityError).toList) match
      case Some(errors) => errors.asLeft
      case None if planned.slices.isEmpty => NonEmptyList.one(EmptyPlanningResult).asLeft
      case None =>
        SchedulingOutcome(
          planned.slices.toList,
          orderDelays(openOrders, planned.slices),
          manufacturingDelays(openOrders, planned.slices),
          skippedOrderWarnings(request, openOrders) ++ capacityWarnings(requiredHours, availableHours),
        ).asRight
  end computeSchedule

  /** Returns the Monday-to-Friday production days inside the window, normalized at start of day. */
  def productionDays(window: PlanningWindow): Vector[DateTime] =
    val start = window.start.withTimeAtStartOfDay().nn
    val end = window.end.withTimeAtStartOfDay().nn
    Iterator.iterate(start)(_.plusDays(1).nn).takeWhile(!_.isAfter(end)).filter(isWorkingDay).toVector

  /** Returns the daily capacity of every employee active on the given day, excluding zero-hour days. */
  def dailyCapacity(day: DateTime, employees: List[Employee]): Map[EmployeeId, DailyHours] =
    employees.view
      .filter(_.isActiveAt(day))
      .map(employee => employee.id -> employee.budgetHours.getWorkingHoursForDay(day))
      .filter { case (_, hours) => hours.value > 0 }
      .toMap

  private def isWorkingDay(day: DateTime): Boolean = day.getDayOfWeek <= 5

  /**
   * Mutable-free planner state threaded through the allocation.
   *
   * @param remaining
   *   Remaining employee hours per production-day index.
   * @param slices
   *   Slices produced so far, in allocation order.
   * @param unplaced
   *   Order and manufacturing pairs owning at least one task that does not fit the window.
   * @param errors
   *   Structural errors, such as dependency cycles, discovered while planning.
   */
  private final case class PlannerState(
      remaining: Vector[Map[EmployeeId, Int]],
      slices: Vector[ScheduledTaskSlice],
      unplaced: Vector[(OrderId, ScheduledManufacturingId)],
      errors: Vector[PlanningError],
  )

  private def planOrder(
      state: PlannerState,
      order: InProgressOrder,
      window: PlanningWindow,
      days: Vector[DateTime],
      base: Vector[Map[EmployeeId, DailyHours]],
  ): PlannerState =
    order.data.setOfManufacturing.toList
      .filter(_.remainingHours.value > 0)
      .sortBy(manufacturing => (manufacturing.info.completionDate.getMillis, manufacturing.info.id.toString))
      .foldLeft(state)(planManufacturing(_, order.data.id, _, window, days, base))

  private def planManufacturing(
      state: PlannerState,
      orderId: OrderId,
      manufacturing: ScheduledManufacturing,
      window: PlanningWindow,
      days: Vector[DateTime],
      base: Vector[Map[EmployeeId, DailyHours]],
  ): PlannerState =
    manufacturing.info.dependencies.sort match
      case Left(CycleDetected(cycle)) =>
        val blockedTaskId = manufacturing.info.tasks.find(task => cycle.contains(task.taskId)).map(_.id).getOrElse(manufacturing.info.tasks.head.id)
        state.copy(errors =
          state.errors :+ TaskCannotBeScheduled(
            orderId,
            manufacturing.info.id,
            blockedTaskId,
            window.start,
            s"Dependency cycle between tasks: ${cycle.mkString(", ")}",
          ),
        )
      case Right(topologicalOrder) =>
        // `sort` puts dependents before their prerequisites (edges point from task to dependency), so execution order is the reverse.
        val executionRank = topologicalOrder.reverse.zipWithIndex.toMap
        val orderedTasks = manufacturing.info.tasks.toList.sortBy(task => executionRank.getOrElse(task.taskId, Int.MaxValue))
        orderedTasks
          .foldLeft(ManufacturingPlan(state, Map.empty, Set.empty)) { (plan, task) =>
            val dependencies = manufacturing.info.dependencies.dependenciesOf(task.taskId)
            if dependencies.exists(plan.blocked.contains) then plan.block(orderId, manufacturing.info.id, task.taskId)
            else if task.remainingHours.value == 0 then plan.markCompleted(task.taskId, dayIndex = -1)
            else
              val earliestStart = dependencies.foldLeft(0)((idx, dependency) => math.max(idx, plan.completionIdx.getOrElse(dependency, -1) + 1))
              allocateTask(plan.state, orderId, manufacturing.info.id, task.id, task.remainingHours.value, earliestStart, days, base) match
                case Some((allocated, lastDayIdx)) => plan.copy(state = allocated).markCompleted(task.taskId, lastDayIdx)
                case None => plan.block(orderId, manufacturing.info.id, task.taskId)
          }
          .state

  /**
   * Per-manufacturing planning progress.
   *
   * @param completionIdx
   *   Day index on which all scheduled work for a template task finishes; `-1` marks work already completed before planning.
   * @param blocked
   *   Template tasks that could not be scheduled, blocking their dependents.
   */
  private final case class ManufacturingPlan(state: PlannerState, completionIdx: Map[TaskId, Int], blocked: Set[TaskId]):
    def markCompleted(taskId: TaskId, dayIndex: Int): ManufacturingPlan =
      copy(completionIdx = completionIdx.updated(taskId, math.max(completionIdx.getOrElse(taskId, -1), dayIndex)))

    def block(orderId: OrderId, manufacturingId: ScheduledManufacturingId, taskId: TaskId): ManufacturingPlan =
      copy(state = state.copy(unplaced = state.unplaced :+ (orderId, manufacturingId)), blocked = blocked + taskId)

  /**
   * Assigns the required hours of one task starting from `startIdx`, splitting over days and employees as needed.
   *
   * Employees with the most remaining hours are picked first, with the employee id as a deterministic tie-break. Returns the updated state and the
   * day index of the last slice, or `None` when the window capacity cannot absorb the task.
   */
  private def allocateTask(
      state: PlannerState,
      orderId: OrderId,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      hours: Int,
      startIdx: Int,
      days: Vector[DateTime],
      base: Vector[Map[EmployeeId, DailyHours]],
  ): Option[(PlannerState, Int)] =
    @tailrec
    def go(
        idx: Int,
        needed: Int,
        remaining: Vector[Map[EmployeeId, Int]],
        slices: Vector[ScheduledTaskSlice],
        lastIdx: Int,
    ): Option[(PlannerState, Int)] =
      if needed <= 0 then Some((state.copy(remaining = remaining, slices = state.slices ++ slices), lastIdx))
      else if idx >= days.length then None
      else
        val candidates = remaining(idx).toList.filter { case (_, left) => left > 0 }.sortBy { case (id, left) => (-left, id.toString) }
        val (dayCapacity, daySlices, neededAfter) = candidates.foldLeft((remaining(idx), Vector.empty[ScheduledTaskSlice], needed)) {
          case ((capacity, acc, toPlace), (employeeId, employeeHours)) =>
            if toPlace <= 0 then (capacity, acc, toPlace)
            else
              val assigned = math.min(toPlace, employeeHours)
              val slice = ScheduledTaskSlice(
                orderId,
                manufacturingId,
                taskId,
                days(idx),
                CandidateEmployee(employeeId, base(idx)(employeeId), TaskHours.applyUnsafe(assigned)),
                TaskHours.applyUnsafe(toPlace - assigned),
              )
              (capacity.updated(employeeId, employeeHours - assigned), acc :+ slice, toPlace - assigned)
        }
        val progressed = if daySlices.nonEmpty then idx else lastIdx
        go(idx + 1, neededAfter, remaining.updated(idx, dayCapacity), slices ++ daySlices, progressed)

    go(startIdx, hours, state.remaining, Vector.empty, startIdx)
  end allocateTask

  private def orderDelays(openOrders: List[InProgressOrder], slices: Vector[ScheduledTaskSlice]): List[DelayedOrder] =
    val lastDayByOrder = lastPlannedDay(slices)(_.orderId)
    openOrders.flatMap { order =>
      lastDayByOrder
        .get(order.data.id)
        .filter(_.isAfter(order.data.deliveryDate))
        .map(DelayedOrder(order.data.id, order.data.deliveryDate, _))
    }

  private def manufacturingDelays(openOrders: List[InProgressOrder], slices: Vector[ScheduledTaskSlice]): List[DelayedManufacturing] =
    val lastDayByManufacturing = lastPlannedDay(slices)(_.manufacturingId)
    for
      order <- openOrders
      manufacturing <- order.data.setOfManufacturing.toList
      computedCompletion <- lastDayByManufacturing.get(manufacturing.info.id).toList
      if computedCompletion.isAfter(manufacturing.info.completionDate)
    yield DelayedManufacturing(order.data.id, manufacturing.info.id, manufacturing.info.completionDate, computedCompletion)

  private def lastPlannedDay[K](slices: Vector[ScheduledTaskSlice])(key: ScheduledTaskSlice => K): Map[K, DateTime] =
    slices.groupBy(key).view.mapValues(_.map(_.day).maxBy(_.getMillis)).toMap

  private def skippedOrderWarnings(request: PlanningRequest, openOrders: List[InProgressOrder]): List[PlanningWarning] =
    val plannedIds = openOrders.map(_.data.id).toSet
    request.openOrderIds
      .filterNot(plannedIds.contains)
      .map(orderId => PlanningWarning(s"Order $orderId was requested for planning but is not an in-progress order and was skipped"))

  private def capacityWarnings(requiredHours: Int, availableHours: Int): List[PlanningWarning] =
    if availableHours > 0 && requiredHours.toDouble / availableHours.toDouble >= tightCapacityRatio then
      List(PlanningWarning(s"Tight capacity: $requiredHours of $availableHours available hours are required in the planning window"))
    else Nil
end SchedulingService
