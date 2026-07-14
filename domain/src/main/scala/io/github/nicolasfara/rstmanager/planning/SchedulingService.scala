package io.github.nicolasfara.rstmanager.planning

import scala.annotation.tailrec

import io.github.nicolasfara.rstmanager.hr.domain.{ Contract, DailyHours, Employee, EmployeeId, VacationOverride, WorkingDayOverride }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencyError.CycleDetected
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

import cats.data.NonEmptyList
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime

/**
 * Facts produced by one scheduling run.
 *
 * The outcome is pure planner output: it carries the day-by-day task slices plus the delays, unplanned orders, and warnings derived from the same
 * run. It is meant to be recorded on the [[Planning]] aggregate, which derives the final [[PlanningResult]] from these accepted facts.
 */
final case class SchedulingOutcome(
    slices: List[ScheduledTaskSlice],
    delayedOrders: List[DelayedOrder],
    delayedManufacturings: List[DelayedManufacturing],
    unplannedOrders: List[UnplannedOrder],
    warnings: List[PlanningWarning],
)

/**
 * Domain service computing when each task must be performed from a starting day onward.
 *
 * The scheduler combines work demand with HR capacity:
 *   - production days are Monday-to-Friday days on or after `PlanningRequest.startOn`;
 *   - each employee contributes, for every production day, the daily hours derived from the weekly budget and calendar overrides
 *     ([[io.github.nicolasfara.rstmanager.hr.domain.BudgetHours.getWorkingHoursForDay]]), skipping employees whose contract is not active on that
 *     day;
 *   - orders are visited in [[PlanningPriorityPolicy]] sequence, so urgent and earlier-due orders consume capacity first;
 *   - each order is planned atomically: if any of its remaining work cannot be placed, no capacity is consumed for that order and it is returned as
 *     an [[UnplannedOrder]];
 *   - inside a manufacturing, tasks are visited in dependency order and a dependent task may start only on the next usable production day after all
 *     its prerequisites are fully scheduled;
 *   - a task requiring more hours than one day offers is split into multiple [[ScheduledTaskSlice]] values over future production days and, when
 *     useful, over multiple employees on the same day.
 *
 * The algorithm is greedy and deterministic. There is no business end date: allocation stops only when work is complete or when the known workforce
 * proves that no future positive-capacity production day exists.
 */
object SchedulingService:
  /**
   * Computes the schedule for a planning request.
   *
   * @param request
   *   Planning request selecting the start day and the orders to plan.
   * @param orders
   *   Candidate orders; only in-progress orders listed in `request.openOrderIds` are planned.
   * @param employees
   *   Workforce providing daily capacity from the request start onward.
   * @return
   *   The computed [[SchedulingOutcome]], or terminal planning errors unrelated to normal capacity exhaustion.
   */
  def computeSchedule(
      request: PlanningRequest,
      orders: List[Order],
      employees: List[Employee],
  ): Either[NonEmptyList[PlanningError], SchedulingOutcome] =
    val startOn = startOfDay(request.startOn)
    val openOrders = PlanningPriorityPolicy.sortOpenOrders(orders).filter(order => request.openOrderIds.contains(order.data.id))
    val initialState = PlannerState.empty(startOn)

    val (planned, unplanned) = openOrders.foldLeft((initialState, Vector.empty[UnplannedOrder])) { case ((state, unplannedOrders), order) =>
      planOrder(state, order, employees) match
        case Right(updatedState) => (updatedState, unplannedOrders)
        case Left(unplannedOrder) => (state, unplannedOrders :+ unplannedOrder)
    }

    val unplannedOrderIds = unplanned.map(_.orderId).toSet
    val plannedOrders = openOrders.filterNot(order => unplannedOrderIds.contains(order.data.id))

    SchedulingOutcome(
      planned.slices.toList,
      orderDelays(plannedOrders, planned.slices),
      manufacturingDelays(plannedOrders, planned.slices),
      unplanned.toList,
      skippedOrderWarnings(request, openOrders),
    ).asRight
  end computeSchedule

  /** Returns the Monday-to-Friday production days from the given start day onward, normalized at start of day. */
  def productionDays(startOn: DateTime): LazyList[DateTime] =
    LazyList.iterate(startOfDay(startOn))(_.plusDays(1).nn).filter(isWorkingDay)

  /** Returns the daily capacity of every employee active on the given day, excluding zero-hour days. */
  def dailyCapacity(day: DateTime, employees: List[Employee]): Map[EmployeeId, DailyHours] =
    employees.view
      .filter(_.isActiveAt(day))
      .map(employee => employee.id -> employee.budgetHours.getWorkingHoursForDay(day))
      .filter { case (_, hours) => hours.value > 0 }
      .toMap

  private def isWorkingDay(day: DateTime): Boolean = day.getDayOfWeek <= 5

  private def startOfDay(day: DateTime): DateTime = day.withTimeAtStartOfDay().nn

  private def nextDay(day: DateTime): DateTime = startOfDay(day.plusDays(1).nn)

  private def maxDate(first: DateTime, second: DateTime): DateTime =
    if first.isAfter(second) then first else second

  private def minDate(first: DateTime, second: DateTime): DateTime =
    if first.isBefore(second) then first else second

  /**
   * Mutable-free planner state threaded through successful allocations.
   *
   * `days`, `base`, and `remaining` share indexes. The vectors grow only when an allocation needs another future positive-capacity production day.
   */
  private final case class PlannerState(
      days: Vector[DateTime],
      base: Vector[Map[EmployeeId, DailyHours]],
      remaining: Vector[Map[EmployeeId, Int]],
      slices: Vector[ScheduledTaskSlice],
      nextSearchFrom: DateTime,
  )

  private object PlannerState:
    def empty(startOn: DateTime): PlannerState =
      PlannerState(Vector.empty, Vector.empty, Vector.empty, Vector.empty, startOn)

  private def planOrder(
      state: PlannerState,
      order: InProgressOrder,
      employees: List[Employee],
  ): Either[UnplannedOrder, PlannerState] =
    val remainingManufacturings = order.data.setOfManufacturing.toList
      .filter(_.remainingHours.value > 0)
      .sortBy(manufacturing => (manufacturing.info.completionDate.getMillis, manufacturing.info.id.toString))

    remainingManufacturings
      .foldLeft(state.asRight[NonEmptyList[UnplannedTask]]) {
        case (Right(currentState), manufacturing) => planManufacturing(currentState, order.data.id, manufacturing, employees)
        case (left @ Left(_), _) => left
      }
      .leftMap(blockedTasks => UnplannedOrder(order.data.id, blockedTasks))
  end planOrder

  private def planManufacturing(
      state: PlannerState,
      orderId: OrderId,
      manufacturing: ScheduledManufacturing,
      employees: List[Employee],
  ): Either[NonEmptyList[UnplannedTask], PlannerState] =
    val tasks = manufacturing.info.tasks.toList
    val taskTemplates = tasks.map(_.taskId).toSet
    val missingDependencies = tasks.flatMap { task =>
      manufacturing.info.dependencies
        .dependenciesOf(task.taskId)
        .filterNot(taskTemplates.contains)
        .toList
        .sortBy(_.toString)
        .map { missingDependency =>
          UnplannedTask(manufacturing.info.id, task.id, UnplannedReason.MissingDependency(missingDependency))
        }
    }

    NonEmptyList.fromList(missingDependencies) match
      case Some(blockedTasks) => blockedTasks.asLeft
      case None =>
        manufacturing.info.dependencies.sort match
          case Left(CycleDetected(cycle)) =>
            val cycleTemplates = if cycle.nonEmpty then cycle else tasks.map(_.taskId).toSet
            val cycleTasks = tasks
              .filter(task => cycleTemplates.contains(task.taskId))
              .map(task => UnplannedTask(manufacturing.info.id, task.id, UnplannedReason.DependencyCycle(cycleTemplates)))
            NonEmptyList
              .fromList(cycleTasks)
              .getOrElse(NonEmptyList.one(UnplannedTask(manufacturing.info.id, tasks.head.id, UnplannedReason.DependencyCycle(cycleTemplates))))
              .asLeft

          case Right(topologicalOrder) =>
            // `sort` puts dependents before prerequisites because edges point from task to dependency, so execution order is the reverse.
            val executionRank = topologicalOrder.reverse.zipWithIndex.toMap
            val orderedTasks = tasks.zipWithIndex.sortBy { case (task, originalIndex) =>
              (executionRank.getOrElse(task.taskId, Int.MaxValue), originalIndex)
            }
              .map(_._1)

            val planned = orderedTasks.foldLeft(ManufacturingPlan(state, Map.empty, Set.empty, Vector.empty)) { (plan, task) =>
              val dependencies = manufacturing.info.dependencies.dependenciesOf(task.taskId)
              dependencies.find(plan.blocked.contains) match
                case Some(blockedDependency) =>
                  plan.block(manufacturing.info.id, task, UnplannedReason.BlockedByDependency(blockedDependency))
                case None if task.remainingHours.value == 0 =>
                  plan.markCompleted(task.taskId, dayIndex = -1)
                case None =>
                  val earliestStart = dependencies.foldLeft(0) { (idx, dependency) =>
                    math.max(idx, plan.completionIdx.getOrElse(dependency, -1) + 1)
                  }
                  allocateTask(
                    plan.state,
                    orderId,
                    manufacturing.info.id,
                    task.id,
                    task.remainingHours.value,
                    earliestStart,
                    employees,
                    manufacturing.info.preferredEmployeeId,
                  ) match
                    case Right((updatedState, lastDayIdx)) => plan.copy(state = updatedState).markCompleted(task.taskId, lastDayIdx)
                    case Left(reason) => plan.block(manufacturing.info.id, task, reason)
              end match
            }

            NonEmptyList.fromList(planned.blockedTasks.toList).fold(planned.state.asRight)(_.asLeft)
    end match
  end planManufacturing

  /**
   * Per-manufacturing planning progress.
   *
   * @param completionIdx
   *   Day index on which all scheduled work for a template task finishes; `-1` marks work already completed before planning.
   * @param blocked
   *   Template tasks that could not be scheduled, blocking their dependents.
   */
  private final case class ManufacturingPlan(
      state: PlannerState,
      completionIdx: Map[TaskId, Int],
      blocked: Set[TaskId],
      blockedTasks: Vector[UnplannedTask],
  ):
    def markCompleted(taskId: TaskId, dayIndex: Int): ManufacturingPlan =
      copy(completionIdx = completionIdx.updated(taskId, math.max(completionIdx.getOrElse(taskId, -1), dayIndex)))

    def block(manufacturingId: ScheduledManufacturingId, task: ScheduledTask, reason: UnplannedReason): ManufacturingPlan =
      copy(
        blocked = blocked + task.taskId,
        blockedTasks = blockedTasks :+ UnplannedTask(manufacturingId, task.id, reason),
      )

  /**
   * Assigns the required hours of one task starting from `startIdx`, splitting over days and employees as needed.
   *
   * Employees with the most remaining hours are picked first, with the employee id as a deterministic tie-break. Returns the updated state and the
   * day index of the last slice, or [[UnplannedReason.NoFutureCapacity]] when no future positive-capacity production day exists.
   */
  private def allocateTask(
      state: PlannerState,
      orderId: OrderId,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      hours: Int,
      startIdx: Int,
      employees: List[Employee],
      preferredEmployeeId: Option[EmployeeId],
  ): Either[UnplannedReason, (PlannerState, Int)] =
    val requiredHours = TaskHours.applyUnsafe(hours)

    @tailrec
    def go(
        current: PlannerState,
        idx: Int,
        needed: Int,
        slices: Vector[ScheduledTaskSlice],
        lastIdx: Int,
    ): Either[UnplannedReason, (PlannerState, Int)] =
      if needed <= 0 then Right((current.copy(slices = current.slices ++ slices), lastIdx))
      else
        ensureDay(current, idx, employees) match
          case None => Left(UnplannedReason.NoFutureCapacity(requiredHours))
          case Some(expanded) =>
            val allCandidates = expanded.remaining(idx).toList.filter { case (_, left) => left > 0 }.sortBy { case (id, left) =>
              (-left, id.toString)
            }
            val candidates = preferredEmployeeId.fold(allCandidates) { preferred =>
              val pref = allCandidates.filter(_._1.equals(preferred))
              if pref.nonEmpty then pref else allCandidates
            }
            val (dayCapacity, daySlices, neededAfter) = candidates.foldLeft((expanded.remaining(idx), Vector.empty[ScheduledTaskSlice], needed)) {
              case ((capacity, acc, toPlace), (employeeId, employeeHours)) =>
                if toPlace <= 0 then (capacity, acc, toPlace)
                else
                  val assigned = math.min(toPlace, employeeHours)
                  val slice = ScheduledTaskSlice(
                    orderId,
                    manufacturingId,
                    taskId,
                    expanded.days(idx),
                    CandidateEmployee(employeeId, expanded.base(idx)(employeeId), TaskHours.applyUnsafe(assigned)),
                    TaskHours.applyUnsafe(toPlace - assigned),
                  )
                  (capacity.updated(employeeId, employeeHours - assigned), acc :+ slice, toPlace - assigned)
            }
            val progressed = if daySlices.nonEmpty then idx else lastIdx
            go(expanded.copy(remaining = expanded.remaining.updated(idx, dayCapacity)), idx + 1, neededAfter, slices ++ daySlices, progressed)

    go(state, startIdx, hours, Vector.empty, startIdx - 1)
  end allocateTask

  @tailrec
  private def ensureDay(state: PlannerState, idx: Int, employees: List[Employee]): Option[PlannerState] =
    if idx < state.days.length then Some(state)
    else
      nextCapacityDay(state.nextSearchFrom, employees) match
        case None => None
        case Some((day, capacity)) =>
          val remaining = capacity.map { case (employeeId, hours) => employeeId -> hours.value }
          ensureDay(
            state.copy(
              days = state.days :+ day,
              base = state.base :+ capacity,
              remaining = state.remaining :+ remaining,
              nextSearchFrom = nextDay(day),
            ),
            idx,
            employees,
          )

  @tailrec
  private def nextCapacityDay(from: DateTime, employees: List[Employee]): Option[(DateTime, Map[EmployeeId, DailyHours])] =
    val candidate = employees.flatMap(nextCapacityDayForEmployee(from, _)).sortBy(_.getMillis).headOption
    candidate match
      case None => None
      case Some(day) =>
        val capacity = dailyCapacity(day, employees)
        if capacity.nonEmpty then Some(day -> capacity)
        else nextCapacityDay(nextDay(day), employees)

  private def nextCapacityDayForEmployee(from: DateTime, employee: Employee): Option[DateTime] =
    val firstCandidate = maxDate(startOfDay(from), startOfDay(contractStart(employee)))
    if defaultDailyHours(employee) > 0 then nextDefaultCapacityDay(firstCandidate, employee)
    else nextPositiveOverrideDay(firstCandidate, employee)

  @tailrec
  private def nextDefaultCapacityDay(candidate: DateTime, employee: Employee): Option[DateTime] =
    if isBeforeContractStart(candidate, employee) then nextDefaultCapacityDay(nextDay(candidate), employee)
    else if isAtOrAfterContractEnd(candidate, employee) then None
    else if !isWorkingDay(candidate) then nextDefaultCapacityDay(nextDay(candidate), employee)
    else if employee.budgetHours.getWorkingHoursForDay(candidate).value > 0 then Some(candidate)
    else
      vacationEndCovering(candidate, employee) match
        case Some(jumpTo) => nextDefaultCapacityDay(jumpTo, employee)
        case None => nextDefaultCapacityDay(nextDay(candidate), employee)

  private def nextPositiveOverrideDay(from: DateTime, employee: Employee): Option[DateTime] =
    employee.budgetHours.overrides.collect {
      case WorkingDayOverride(hours, _, day) if hours.value > 0 =>
        startOfDay(day)
    }.filter { day =>
      !day.isBefore(from) &&
      isWorkingDay(day) &&
      !isBeforeContractStart(day, employee) &&
      !isAtOrAfterContractEnd(day, employee) &&
      employee.budgetHours.getWorkingHoursForDay(day).value > 0
    }.reduceOption(minDate)

  private def vacationEndCovering(day: DateTime, employee: Employee): Option[DateTime] =
    employee.budgetHours.overrides.collect {
      case VacationOverride(interval) if interval.contains(day) =>
        val endDay = startOfDay(interval.getEnd.nn)
        if endDay.isAfter(day) then endDay else nextDay(day)
    }.reduceOption(minDate)

  private def contractStart(employee: Employee): DateTime = employee.contract match
    case Contract.FullTime(startDate) => startDate
    case Contract.FixedTerm(startDate, _) => startDate
    case Contract.PartTime(startDate, _) => startDate

  private def contractEnd(employee: Employee): Option[DateTime] = employee.contract match
    case Contract.FixedTerm(_, endDate) => Some(endDate)
    case Contract.FullTime(_) | Contract.PartTime(_, _) => None

  private def isBeforeContractStart(day: DateTime, employee: Employee): Boolean =
    day.isBefore(contractStart(employee))

  private def isAtOrAfterContractEnd(day: DateTime, employee: Employee): Boolean =
    contractEnd(employee).exists(endDate => !endDate.isAfter(day))

  private def defaultDailyHours(employee: Employee): Int = employee.budgetHours.default.value / 5

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
end SchedulingService
