package io.github.nicolasfara.rstmanager.work.domain.order.events

import io.github.iltotore.iron.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.order.{OrderData, OrderPriority, SuspensionReason}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ScheduledManufacturing, ScheduledManufacturingId}
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId
import io.github.nicolasfara.rstmanager.work.domain.order.CancellationReason

/** Events representing state changes at the Order level within the Order aggregate.
  *
  * These events capture changes to the Order entity itself, including adding/removing manufacturings from the order. Events affecting the
  * ScheduledManufacturing entities within the aggregate are defined in ManufacturingEvent.
  *
  * Note: ScheduledManufacturing is an entity within the Order aggregate boundary. Both OrderEvent and ManufacturingEvent are emitted through the
  * Order aggregate root, but are separated for clarity and organization.
  */
enum OrderEvent:
  /** Order has been created with initial data */
  case OrderCreated(orderData: OrderData, deliveryDate: DateTime)

  /** Order has been canceled */
  case OrderCancelled(cancelledOn: DateTime, reason: Option[String :| CancellationReason])

  /** Order has been temporarily suspended */
  case OrderSuspended(suspendedOn: DateTime, reason: Option[String :| SuspensionReason])

  /** Previously suspended order has been reactivated */
  case OrderReactivated(reactivatedOn: DateTime)

  /** Order has been completed (all work done) */
  case OrderCompleted(completionDate: DateTime)

  /** Order has been delivered to customer */
  case OrderDelivered(deliveredOn: DateTime)

  /** Order delivery date has been changed */
  case OrderDeliveryDateChanged(newDeliveryDate: DateTime, changedOn: DateTime)

  /** Order priority has been changed */
  case OrderPriorityChanged(newPriority: OrderPriority, changedOn: DateTime)

  /** A manufacturing has been added to the order */
  case ManufacturingAdded(manufacturing: ScheduledManufacturing, addedOn: DateTime)

  /** A manufacturing has been removed from the order */
  case ManufacturingRemoved(manufacturingId: ScheduledManufacturingId, removedOn: DateTime)

  /** A task within a manufacturing has been advanced by a certain number of hours */
  case ManufacturingTaskAdvanced(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, advancedBy: TaskHours)

  /** A task within a manufacturing has been de-advanced by a certain number of hours */
  case ManufacturingTaskRolledBack(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, deAdvancedBy: TaskHours)

  /** A task within a manufacturing has been completed */
  case ManufacturingTaskCompleted(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, withHours: TaskHours)

  /** A task within a manufacturing has been reverted to in-progress */
  case ManufacturingTaskReverted(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId)
