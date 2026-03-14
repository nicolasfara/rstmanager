package io.github.nicolasfara.rstmanager.work.domain.order.events

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.order.{ CancellationReason, OrderData, OrderPriority, SuspensionReason }
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*

/** Domain events emitted by the `Order` aggregate.
  *
  * The enum includes both order-level lifecycle events and task/manufacturing events that are
  * persisted through the aggregate root.
  */
enum OrderEvent:
  /** The order has been created with its initial data. */
  case OrderCreated(orderData: OrderData, deliveryDate: DateTime)

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

  /** The planned delivery date has changed. */
  case OrderDeliveryDateChanged(newDeliveryDate: DateTime, changedOn: DateTime)

  /** The order priority has changed. */
  case OrderPriorityChanged(newPriority: OrderPriority, changedOn: DateTime)

  /** A manufacturing has been added to the order. */
  case ManufacturingAdded(manufacturing: ScheduledManufacturing, addedOn: DateTime)

  /** A manufacturing has been removed from the order. */
  case ManufacturingRemoved(manufacturingId: ScheduledManufacturingId, removedOn: DateTime)

  /** Progress on a task within a manufacturing has been advanced. */
  case ManufacturingTaskAdvanced(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, advancedBy: TaskHours)

  /** Progress on a task within a manufacturing has been rolled back. */
  case ManufacturingTaskRolledBack(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, deAdvancedBy: TaskHours)

  /** A task within a manufacturing has been completed. */
  case ManufacturingTaskCompleted(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, withHours: TaskHours)

  /** A completed task within a manufacturing has been reopened. */
  case ManufacturingTaskReverted(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId)
end OrderEvent
