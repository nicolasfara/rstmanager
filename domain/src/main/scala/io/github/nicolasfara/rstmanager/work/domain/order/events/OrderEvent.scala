package io.github.nicolasfara.rstmanager.work.domain.order.events

import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.order.{OrderData, OrderId, OrderPriority, SuspensionReason}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.{ScheduledManufacturing, ScheduledManufacturingId}

/** Events representing state changes at the Order level within the Order aggregate.
  *
  * These events capture changes to the Order entity itself, including adding/removing
  * manufacturings from the order. Events affecting the ScheduledManufacturing entities
  * within the aggregate are defined in ManufacturingEvent.
  *
  * Note: ScheduledManufacturing is an entity within the Order aggregate boundary.
  * Both OrderEvent and ManufacturingEvent are emitted through the Order aggregate root,
  * but are separated for clarity and organization.
  */
enum OrderEvent:
  /** Order has been created with initial data */
  case OrderCreated(orderData: OrderData, timestamp: DateTime)
  
  /** Order has been cancelled */
  case OrderCancelled(orderId: OrderId, cancelledOn: DateTime, reason: Option[String])
  
  /** Order data has been updated (e.g., delivery date changed) */
  case OrderUpdated(updatedData: OrderData, updatedOn: DateTime)
  
  /** Order has been temporarily suspended */
  case OrderSuspended(reason: Option[SuspensionReason], suspendedOn: DateTime)
  
  /** Previously suspended order has been reactivated */
  case OrderReactivated(reactivatedOn: DateTime)
  
  /** Order has been completed (all work done) */
  case OrderCompleted(completionDate: DateTime)
  
  /** Order has been delivered to customer */
  case OrderDelivered(deliveredOn: DateTime)
  
  /** Order priority has been changed */
  case OrderPriorityChanged(newPriority: OrderPriority, changedOn: DateTime)
  
  /** A manufacturing has been added to the order */
  case ManufacturingAdded(manufacturing: ScheduledManufacturing, addedOn: DateTime)
  
  /** A manufacturing has been removed from the order */
  case ManufacturingRemoved(manufacturingId: ScheduledManufacturingId, removedOn: DateTime)
