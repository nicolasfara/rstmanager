package io.github.nicolasfara.rstmanager.work.domain.order.events

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ManufacturingStatus, ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.order.{ CancellationReason, OrderData, OrderDependencies, OrderPriority, SuspensionReason }
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*

/**
 * Domain events emitted by the `Order` aggregate.
 *
 * The enum includes both order-level lifecycle events and task/manufacturing events that are persisted through the aggregate root.
 */
enum OrderEvent:
  /** The order has been created with its initial data. */
  case OrderCreated(orderData: OrderData, promisedDeliveryDate: DateTime)

  /** The order has been cancelled. */
  case OrderCancelled(cancelledOn: DateTime, reason: Option[String :| CancellationReason])

  /** The order has been temporarily suspended. */
  case OrderSuspended(suspendedOn: DateTime, reason: Option[String :| SuspensionReason])

  /** A suspended or cancelled order has been reactivated. */
  case OrderReactivated(reactivatedOn: DateTime)

  /** The order has been completed. */
  case OrderCompleted(completionDate: DateTime)

  /** The order has been delivered to the customer. */
  case OrderDelivered(deliveredOn: DateTime)

  /** The work-completion deadline has changed. */
  case OrderPromisedDeliveryDateChanged(newPromisedDeliveryDate: DateTime, changedOn: DateTime)

  /** The order priority has changed. */
  case OrderPriorityChanged(newPriority: OrderPriority, changedOn: DateTime)

  /** The order description has changed. */
  case OrderDescriptionChanged(newDescription: Option[String], changedOn: DateTime)

  /** A manufacturing has been added to the order. */
  case ManufacturingAdded(manufacturing: ScheduledManufacturing, addedOn: DateTime)

  /** A manufacturing has been removed from the order. */
  case ManufacturingRemoved(manufacturingId: ScheduledManufacturingId, removedOn: DateTime)

  /** A manufacturing's description has changed. */
  case ManufacturingDescriptionChanged(manufacturingId: ScheduledManufacturingId, newDescription: Option[String], changedOn: DateTime)

  /** A manufacturing's work deadline has changed. */
  case ManufacturingCompletionDateChanged(manufacturingId: ScheduledManufacturingId, newCompletionDate: DateTime, changedOn: DateTime)

  /** The preferred employee for a manufacturing has been set or cleared. */
  case ManufacturingPreferredEmployeeChanged(manufacturingId: ScheduledManufacturingId, employeeId: Option[UUID], changedOn: DateTime)

  /** The preferred employee of a task within a manufacturing has been set or cleared. */
  case ManufacturingTaskPreferredEmployeeChanged(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      employeeId: Option[UUID],
      changedOn: DateTime,
  )

  /** A manufacturing has been manually moved to a new lifecycle status. */
  case ManufacturingStatusChanged(
      manufacturingId: ScheduledManufacturingId,
      newStatus: ManufacturingStatus,
      reason: Option[String],
      changedOn: DateTime,
  )

  /** A task has been added to a manufacturing, optionally with a preferred employee for the planner. */
  case ManufacturingTaskAdded(
      manufacturingId: ScheduledManufacturingId,
      task: ScheduledTask,
      dependsOn: List[TaskId],
      addedOn: DateTime,
      preferredEmployeeId: Option[UUID] = None,
  )

  /** A task has been removed from a manufacturing. */
  case ManufacturingTaskRemoved(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, removedOn: DateTime)

  /** Progress on a task within a manufacturing has been advanced. */
  case ManufacturingTaskAdvanced(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, advancedBy: TaskHours)

  /** Progress on a task within a manufacturing has been rolled back. */
  case ManufacturingTaskRolledBack(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, deAdvancedBy: TaskHours)

  /** The absolute progress (completed hours) of a task within a manufacturing has been set. */
  case ManufacturingTaskProgressSet(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, completedHours: TaskHours)

  /** The total expected hours of a task within a manufacturing has been changed. */
  case ManufacturingTaskExpectedHoursChanged(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, expectedHours: TaskHours)

  /** A task within a manufacturing has been completed. */
  case ManufacturingTaskCompleted(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, withHours: TaskHours)

  /** A completed task within a manufacturing has been reopened. */
  case ManufacturingTaskReverted(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId)

  /** The dependency graph between the order manufacturings has been replaced. */
  case ManufacturingDependenciesChanged(dependencies: OrderDependencies, changedOn: DateTime)

  /** The task dependency graph of a manufacturing has been replaced. */
  case TaskDependenciesChanged(manufacturingId: ScheduledManufacturingId, dependencies: ManufacturingDependencies, changedOn: DateTime)
end OrderEvent
