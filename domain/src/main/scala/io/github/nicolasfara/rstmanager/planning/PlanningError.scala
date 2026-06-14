package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import com.github.nscala_time.time.Imports.DateTime

/**
 * Errors produced while computing, deciding, or applying a production plan.
 *
 * Planning errors are structured so the application can build clear user-facing messages without looking up every piece of context again. They
 * include the relevant order, manufacturing, task, date, and capacity values whenever those values are known.
 */
enum PlanningError derives CanEqual:
  /** Returned when the planning window end is before its start. */
  case InvalidPlanningWindow(start: DateTime, end: DateTime)

  /** Returned when an employee assignment is empty or exceeds the employee's available hours. */
  case InvalidEmployeeAssignment(availableHours: TaskHours, assignedHours: TaskHours)

  /** Returned when a daily schedule contains no task slices. */
  case EmptyDailySchedule(day: DateTime)

  /** Returned when a task slice belongs to a different day than its containing daily schedule. */
  case TaskSliceOutsideScheduleDay(scheduleDay: DateTime, sliceDay: DateTime)

  /**
   * Returned when no feasible capacity is available for the requested planning window.
   *
   * `requiredHours` and `availableHours` summarize the capacity gap. The affected identifiers make it possible to explain which orders and
   * manufacturings could not fit the window.
   */
  case InsufficientCapacity(
      window: PlanningWindow,
      requiredHours: TaskHours,
      availableHours: TaskHours,
      affectedOrders: List[OrderId],
      affectedManufacturings: List[ScheduledManufacturingId],
  )

  /** Returned when a manufacturing cannot complete by its expected date. */
  case ManufacturingDeadlineExceeded(
      orderId: OrderId,
      manufacturingId: ScheduledManufacturingId,
      expectedDate: DateTime,
      computedDate: DateTime,
  )

  /**
   * Returned when a task cannot be scheduled on a candidate day because of a blocking constraint.
   *
   * The blocking constraint is intentionally textual for now because the scheduler implementation does not yet expose a closed set of constraint
   * types.
   */
  case TaskCannotBeScheduled(
      orderId: OrderId,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      day: DateTime,
      blockingConstraint: String,
  )

  /** Returned when planning is requested while another planning request is already in progress. */
  case PlanningAlreadyInProgress(requestId: PlanningRequestId)

  /** Returned when an event requires an active planning request. */
  case PlanningMustBeInProgress

  /** Returned when no daily schedule can be computed. */
  case EmptyPlanningResult
end PlanningError
