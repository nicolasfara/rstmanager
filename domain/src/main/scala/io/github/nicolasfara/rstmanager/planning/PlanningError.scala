package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.hr.domain.DailyHours
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
  /** Returned when an employee assignment is empty or exceeds the employee's available hours. */
  case InvalidEmployeeAssignment(availableHours: DailyHours, assignedHours: TaskHours)

  /** Returned when a planned order delay does not actually move the promised date after the expected date. */
  case InvalidOrderDelay(orderId: OrderId, expectedDeliveryDate: DateTime, promisedDeliveryDate: DateTime)

  /** Returned when a planned manufacturing delay does not actually move completion after the expected date. */
  case InvalidManufacturingDelay(
      orderId: OrderId,
      manufacturingId: ScheduledManufacturingId,
      expectedCompletionDate: DateTime,
      computedCompletionDate: DateTime,
  )

  /** Returned when a daily schedule contains no task slices. */
  case EmptyDailySchedule(day: DateTime)

  /** Returned when a task slice belongs to a different day than its containing daily schedule. */
  case TaskSliceOutsideScheduleDay(scheduleDay: DateTime, sliceDay: DateTime)

  /** Returned when a manufacturing cannot complete by its expected date. */
  case ManufacturingDeadlineExceeded(
      orderId: OrderId,
      manufacturingId: ScheduledManufacturingId,
      expectedDate: DateTime,
      computedDate: DateTime,
  )

  /**
   * Returned when a terminal validation path needs to reject a task-level planning fact.
   *
   * Normal scheduler output uses [[UnplannedReason]] instead of this error so the planning attempt can still complete.
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
end PlanningError
