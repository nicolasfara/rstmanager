package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.{ NonEmptyList, Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime

/**
 * Inclusive date range used as input for a planning attempt.
 *
 * A planning window bounds the days that the planner is allowed to inspect and populate. The domain treats both `start` and `end` as included in the
 * window. Use [[PlanningWindow.create]] when accepting external input so the invariant `end >= start` is checked and reported as a
 * [[PlanningError.InvalidPlanningWindow]].
 *
 * @param start
 *   First production day that may be considered by the planning attempt.
 * @param end
 *   Last production day that may be considered by the planning attempt.
 */
final case class PlanningWindow(start: DateTime, end: DateTime)

object PlanningWindow:
  /** Creates a planning window when the end is on or after the start. */
  def create(start: DateTime, end: DateTime): ValidatedNec[PlanningError, PlanningWindow] =
    Validated.condNec(!end.isBefore(start), PlanningWindow(start, end), PlanningError.InvalidPlanningWindow(start, end))

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
final case class CandidateEmployee(employeeId: EmployeeId, availableHours: TaskHours, assignedHours: TaskHours)

object CandidateEmployee:
  /** Creates an employee assignment for a single task slice. */
  def create(employeeId: EmployeeId, availableHours: TaskHours, assignedHours: TaskHours): ValidatedNec[PlanningError, CandidateEmployee] =
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
        nonEmptySlices.find(slice => slice.day.getMillis != day.getMillis) match
          case Some(slice) => PlanningError.TaskSliceOutsideScheduleDay(day, slice.day).invalidNec
          case None => DailySchedule(day, nonEmptySlices).validNec
      }

/**
 * Order whose planned delivery date moved beyond its expected delivery date.
 *
 * The promised delivery date should be the first admissible date computed by the new schedule.
 */
final case class DelayedOrder(orderId: OrderId, expectedDeliveryDate: DateTime, promisedDeliveryDate: DateTime)

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
 * A planning result is produced only when at least one production day has planned work. Delays and warnings are explicit side information derived
 * from the same planning run. This type does not mutate orders or task progress by itself; it describes the schedule that was computed.
 *
 * @param schedules
 *   Non-empty day-by-day schedule.
 * @param delayedOrders
 *   Orders whose promised delivery dates moved beyond their expected delivery dates.
 * @param delayedManufacturings
 *   Manufacturings whose computed completion dates exceed their expected completion dates.
 * @param warnings
 *   Non-fatal planning notes that should remain visible to users or auditors.
 */
final case class PlanningResult(
    schedules: NonEmptyList[DailySchedule],
    delayedOrders: List[DelayedOrder],
    delayedManufacturings: List[DelayedManufacturing],
    warnings: List[PlanningWarning],
)

object PlanningResult:
  /** Creates a planning result when at least one daily schedule exists. */
  def create(
      schedules: List[DailySchedule],
      delayedOrders: List[DelayedOrder],
      delayedManufacturings: List[DelayedManufacturing],
      warnings: List[PlanningWarning],
  ): ValidatedNec[PlanningError, PlanningResult] =
    NonEmptyList
      .fromList(schedules)
      .toValidNec(PlanningError.EmptyPlanningResult)
      .map(PlanningResult(_, delayedOrders, delayedManufacturings, warnings))
