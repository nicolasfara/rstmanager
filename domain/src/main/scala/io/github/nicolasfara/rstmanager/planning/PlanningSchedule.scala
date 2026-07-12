package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.hr.domain.{ DailyHours, EmployeeId }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.{ NonEmptyList, Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime

/**
 * Employee selected for one scheduled task slice.
 *
 * The assignment is deliberately slice-level, not task-level: a task may span multiple days and the employee may change between slices.
 * `availableHours` is the employee capacity for the production day after weekly capacity and calendar overrides have been applied. `assignedHours` is
 * the portion of that availability consumed by this specific slice.
 *
 * The smart constructor checks the per-slice invariant `0 < assignedHours <= availableHours`. It does not validate the employee's total load across
 * all slices on the same day; that belongs to the future scheduling algorithm.
 *
 * @param employeeId
 *   Employee selected for the work slice.
 * @param availableHours
 *   Employee hours available on the slice day.
 * @param assignedHours
 *   Hours assigned to this slice.
 */
final case class CandidateEmployee(employeeId: EmployeeId, availableHours: DailyHours, assignedHours: TaskHours)

object CandidateEmployee:
  /** Creates an employee assignment for a single task slice. */
  def create(employeeId: EmployeeId, availableHours: DailyHours, assignedHours: TaskHours): ValidatedNec[PlanningError, CandidateEmployee] =
    Validated.condNec(
      assignedHours.value > 0 && assignedHours.value <= availableHours.value,
      CandidateEmployee(employeeId, availableHours, assignedHours),
      PlanningError.InvalidEmployeeAssignment(availableHours, assignedHours),
    )

/**
 * Amount of task work planned for one production day.
 *
 * A slice is the smallest planning output currently modeled. It says that, on a given day, one employee should spend
 * `candidateEmployee.assignedHours` on one scheduled task belonging to one scheduled manufacturing and order. Task effort remains hour-based through
 * [[TaskHours]], so a task that needs more work than a day allows is represented by multiple slices over multiple days.
 *
 * `remainingHoursAfterSlice` records the projected task remainder after this slice is applied. It is informational domain output for users and audit
 * trails; actual task progress remains owned by the work execution model.
 *
 * @param orderId
 *   Order receiving the planned work.
 * @param manufacturingId
 *   Scheduled manufacturing receiving the planned work.
 * @param taskId
 *   Scheduled task receiving the planned work.
 * @param day
 *   Production day for this slice.
 * @param candidateEmployee
 *   Employee assignment for this slice.
 * @param remainingHoursAfterSlice
 *   Projected remaining task hours after the assigned work is completed.
 */
final case class ScheduledTaskSlice(
    orderId: OrderId,
    manufacturingId: ScheduledManufacturingId,
    taskId: ScheduledTaskId,
    day: DateTime,
    candidateEmployee: CandidateEmployee,
    remainingHoursAfterSlice: TaskHours,
)

/**
 * Planned work for one production day.
 *
 * A daily schedule groups all task slices assigned to the same production day. Use [[DailySchedule.create]] when building a schedule from planner
 * output so the model rejects empty days and slices that point to a different day. Empty production days should simply be omitted from a
 * [[PlanningResult]].
 *
 * @param day
 *   Production day represented by this schedule.
 * @param slices
 *   Non-empty task slices planned for `day`.
 */
final case class DailySchedule(day: DateTime, slices: NonEmptyList[ScheduledTaskSlice])

object DailySchedule:
  /** Creates a daily schedule when it has at least one slice and every slice belongs to the same day. */
  def create(day: DateTime, slices: List[ScheduledTaskSlice]): ValidatedNec[PlanningError, DailySchedule] =
    NonEmptyList
      .fromList(slices)
      .toValidNec(PlanningError.EmptyDailySchedule(day))
      .andThen { nonEmptySlices =>
        val scheduleDay = day.withTimeAtStartOfDay().nn
        nonEmptySlices.find(slice => slice.day.withTimeAtStartOfDay().nn.getMillis != scheduleDay.getMillis) match
          case Some(slice) => PlanningError.TaskSliceOutsideScheduleDay(day, slice.day).invalidNec
          case None => DailySchedule(scheduleDay, nonEmptySlices).validNec
      }

/**
 * Order whose promised delivery date moved beyond its expected delivery date.
 *
 * The promised delivery date should be the first admissible date computed by the new schedule.
 */
final case class DelayedOrder(orderId: OrderId, expectedDeliveryDate: DateTime, promisedDeliveryDate: DateTime)

object DelayedOrder:
  /** Creates an order delay when the promised delivery date is after the expected date. */
  def create(orderId: OrderId, expectedDeliveryDate: DateTime, promisedDeliveryDate: DateTime): ValidatedNec[PlanningError, DelayedOrder] =
    Validated.condNec(
      promisedDeliveryDate.isAfter(expectedDeliveryDate),
      DelayedOrder(orderId, expectedDeliveryDate, promisedDeliveryDate),
      PlanningError.InvalidOrderDelay(orderId, expectedDeliveryDate, promisedDeliveryDate),
    )

/**
 * Manufacturing whose computed completion date moved beyond its expected completion date.
 *
 * A delayed manufacturing means the manufacturing does not fit the current planning constraints before its expected completion date.
 */
final case class DelayedManufacturing(
    orderId: OrderId,
    manufacturingId: ScheduledManufacturingId,
    expectedCompletionDate: DateTime,
    computedCompletionDate: DateTime,
)

object DelayedManufacturing:
  /** Creates a manufacturing delay when the computed completion date is after the expected date. */
  def create(
      orderId: OrderId,
      manufacturingId: ScheduledManufacturingId,
      expectedCompletionDate: DateTime,
      computedCompletionDate: DateTime,
  ): ValidatedNec[PlanningError, DelayedManufacturing] =
    Validated.condNec(
      computedCompletionDate.isAfter(expectedCompletionDate),
      DelayedManufacturing(orderId, manufacturingId, expectedCompletionDate, computedCompletionDate),
      PlanningError.InvalidManufacturingDelay(orderId, manufacturingId, expectedCompletionDate, computedCompletionDate),
    )

/**
 * Structured reason why a task could not be planned.
 *
 * These reasons describe impossible work, not delayed work: orders that can be scheduled after their expected date are still represented through
 * [[DelayedOrder]] and [[DelayedManufacturing]].
 */
enum UnplannedReason derives CanEqual:
  /** No future production day with enough remaining workforce capacity exists for the task. */
  case NoFutureCapacity(requiredHours: TaskHours)

  /** The manufacturing dependency graph contains a cycle involving the listed task templates. */
  case DependencyCycle(cycle: Set[TaskId])

  /** The task depends on a template task that is not present in the manufacturing. */
  case MissingDependency(dependency: TaskId)

  /** The task cannot start because one of its prerequisites is itself unplanned. */
  case BlockedByDependency(dependency: TaskId)

/**
 * One task-level planning failure inside an unplanned order.
 *
 * @param manufacturingId
 *   Manufacturing containing the blocked task.
 * @param taskId
 *   Scheduled task instance that could not be placed.
 * @param reason
 *   Structured reason explaining the blocking condition.
 */
final case class UnplannedTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, reason: UnplannedReason)

/**
 * Order that cannot be planned with the known workforce and structural constraints.
 *
 * The list is non-empty because an order is marked unplanned only when at least one task makes the order impossible to schedule atomically.
 */
final case class UnplannedOrder(orderId: OrderId, blockedTasks: NonEmptyList[UnplannedTask])

/**
 * Non-fatal issue detected while planning.
 *
 * Warnings should be shown to users or kept for audit when planning succeeds but some condition is noteworthy, such as tight capacity or a
 * lower-priority order moving close to its deadline.
 */
final case class PlanningWarning(message: String)

/**
 * Result of one planning attempt.
 *
 * A planning result may contain no planned production days: when no work can be placed, the result still carries the structured list of unplanned
 * orders and warnings. Delays and warnings are explicit side information derived from the same planning run. This type does not mutate orders or task
 * progress by itself; it describes the schedule that was computed.
 *
 * @param schedules
 *   Day-by-day schedules. Empty production days are omitted.
 * @param delayedOrders
 *   Orders whose promised delivery dates moved beyond their expected delivery dates.
 * @param delayedManufacturings
 *   Manufacturings whose computed completion dates exceed their expected completion dates.
 * @param unplannedOrders
 *   Orders that could not be planned at all with the known workforce and constraints.
 * @param warnings
 *   Non-fatal planning notes that should remain visible to users or auditors.
 */
final case class PlanningResult(
    schedules: List[DailySchedule],
    delayedOrders: List[DelayedOrder],
    delayedManufacturings: List[DelayedManufacturing],
    unplannedOrders: List[UnplannedOrder],
    warnings: List[PlanningWarning],
)

object PlanningResult:
  /** Creates a planning result. Daily schedules, when present, are expected to have been validated through [[DailySchedule.create]]. */
  def create(
      schedules: List[DailySchedule],
      delayedOrders: List[DelayedOrder],
      delayedManufacturings: List[DelayedManufacturing],
      unplannedOrders: List[UnplannedOrder],
      warnings: List[PlanningWarning],
  ): ValidatedNec[PlanningError, PlanningResult] =
    PlanningResult(schedules, delayedOrders, delayedManufacturings, unplannedOrders, warnings).validNec

  /** Builds the final planning result from accepted task-slice, delay, unplanned-order, and warning events. */
  def fromSlices(
      slices: List[ScheduledTaskSlice],
      delayedOrders: List[DelayedOrder],
      delayedManufacturings: List[DelayedManufacturing],
      unplannedOrders: List[UnplannedOrder],
      warnings: List[PlanningWarning],
  ): ValidatedNec[PlanningError, PlanningResult] =
    val scheduleInputs = slices
      .groupBy(_.day.withTimeAtStartOfDay().nn.getMillis)
      .toList
      .sortBy(_._1)
      .map { case (_, daySlices) =>
        val scheduleDay = daySlices.head.day.withTimeAtStartOfDay().nn
        DailySchedule.create(scheduleDay, daySlices)
      }

    scheduleInputs.sequence.andThen(create(_, delayedOrders, delayedManufacturings, unplannedOrders, warnings))
end PlanningResult
