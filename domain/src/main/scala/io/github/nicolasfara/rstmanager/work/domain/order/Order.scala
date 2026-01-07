package io.github.nicolasfara.rstmanager.work.domain.order

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import edomata.core.DomainModel
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.{InvalidTransition, OrderAlreadyCreated}
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.OrderCreated

/** Aggregate root representing an order in different states.
  *
  * An order can be in one of the following states:
  *   - [[NewOrder]]: Initial state, order not yet created
  *   - [[InProgressOrder]]: The order is currently being processed and has a delivery date
  *   - [[SuspendedOrder]]: The order is temporarily on hold with a reason
  *   - [[CompletedOrder]]: The order has been completed, with completion date
  *   - [[DeliveredOrder]]: The order has been delivered to the customer
  *   - [[CancelledOrder]]: The order has been cancelled
  *
  * State transitions are driven by OrderEvents and validated to ensure business rules are respected.
  */
enum Order derives CanEqual:
  case NewOrder
  case InProgressOrder(data: OrderData, plannedDelivery: DateTime)
  case SuspendedOrder(data: OrderData, plannedDelivery: DateTime, pausedOn: DateTime, reason: Option[SuspensionReason])
  case CompletedOrder(data: OrderData, completionDate: DateTime)
  case DeliveredOrder(data: OrderData, completionDate: DateTime, deliveredOn: DateTime)
  case CancelledOrder(data: OrderData, cancelledOn: DateTime, reason: Option[String])

object Order extends DomainModel[Order, OrderEvent, OrderError]:
  override def initial: Order = NewOrder

  override def transition: OrderEvent => Order => ValidatedNec[OrderError, Order] = event =>
    state =>
      (event, state) match
        case (OrderCreated(data, timestamp), NewOrder) => InProgressOrder(data, data.deliveryDate).validNec
        case (_, NewOrder)                             => InvalidTransition("Order created event expected").invalidNec
        case (OrderCreated(_, _), _)                   => OrderAlreadyCreated.invalidNec
        case _                                         => InvalidTransition("TODO").invalidNec
//
//      // Order Cancellation
//      case (OrderEvent.OrderCancelled(_, cancelledOn, reason), InProgressOrder(data, _)) =>
//        CancelledOrder(data, cancelledOn, reason).validNec
//
//      case (OrderEvent.OrderCancelled(_, cancelledOn, reason), SuspendedOrder(data, _, _, _)) =>
//        CancelledOrder(data, cancelledOn, reason).validNec
//
//      case (OrderEvent.OrderCancelled(_, _, _), _) =>
//        OrderError.CannotCancelInCurrentState.invalidNec
//
//      // Order Update
//      case (OrderEvent.OrderUpdated(updatedData, _), InProgressOrder(_, plannedDelivery)) =>
//        InProgressOrder(updatedData, updatedData.deliveryDate).validNec
//
//      case (OrderEvent.OrderUpdated(updatedData, _), SuspendedOrder(_, plannedDelivery, pausedOn, reason)) =>
//        SuspendedOrder(updatedData, updatedData.deliveryDate, pausedOn, reason).validNec
//
//      case (OrderEvent.OrderUpdated(_, _), _) =>
//        OrderError.CannotUpdateInCurrentState.invalidNec
//
//      // Order Suspension
//      case (OrderEvent.OrderSuspended(reason, suspendedOn), InProgressOrder(data, plannedDelivery)) =>
//        SuspendedOrder(data, plannedDelivery, suspendedOn, reason).validNec
//
//      case (OrderEvent.OrderSuspended(_, _), _) =>
//        OrderError.CannotSuspendInCurrentState.invalidNec
//
//      // Order Reactivation
//      case (OrderEvent.OrderReactivated(_), SuspendedOrder(data, plannedDelivery, _, _)) =>
//        InProgressOrder(data, plannedDelivery).validNec
//
//      case (OrderEvent.OrderReactivated(_), _) =>
//        OrderError.CannotReactivateInCurrentState.invalidNec
//
//      // Order Completion
//      case (OrderEvent.OrderCompleted(completionDate), InProgressOrder(data, _)) =>
//        // Validate all manufacturings are completed
//        if data.setOfManufacturing.forall(_.isCompleted) then
//          CompletedOrder(data, completionDate).validNec
//        else
//          OrderError.CannotCompleteWithPendingManufacturing.invalidNec
//
//      case (OrderEvent.OrderCompleted(_), _) =>
//        OrderError.CannotCompleteInCurrentState.invalidNec
//
//      // Order Delivery
//      case (OrderEvent.OrderDelivered(deliveredOn), CompletedOrder(data, completionDate)) =>
//        DeliveredOrder(data, completionDate, deliveredOn).validNec
//
//      case (OrderEvent.OrderDelivered(_), _) =>
//        OrderError.CannotDeliverInCurrentState.invalidNec
//
//      // Priority Change
//      case (OrderEvent.OrderPriorityChanged(newPriority, _), InProgressOrder(data, plannedDelivery)) =>
//        val updatedData = data.copy(priority = newPriority)
//        InProgressOrder(updatedData, plannedDelivery).validNec
//
//      case (OrderEvent.OrderPriorityChanged(newPriority, _), SuspendedOrder(data, plannedDelivery, pausedOn, reason)) =>
//        val updatedData = data.copy(priority = newPriority)
//        SuspendedOrder(updatedData, plannedDelivery, pausedOn, reason).validNec
//
//      case (OrderEvent.OrderPriorityChanged(_, _), _) =>
//        OrderError.CannotChangePriorityInCurrentState.invalidNec
//
//      // Manufacturing Management
//      case (OrderEvent.ManufacturingAdded(manufacturing, _), InProgressOrder(data, plannedDelivery)) =>
//        val updatedManufacturings = data.setOfManufacturing.append(manufacturing)
//        val updatedData = data.copy(setOfManufacturing = updatedManufacturings)
//        InProgressOrder(updatedData, plannedDelivery).validNec
//
//      case (OrderEvent.ManufacturingAdded(_, _), _) =>
//        OrderError.CannotAddManufacturingInCurrentState.invalidNec
//
//      case (OrderEvent.ManufacturingRemoved(manufacturingId, _), InProgressOrder(data, plannedDelivery)) =>
//        val filtered = data.setOfManufacturing.filterNot(_.id == manufacturingId)
//        filtered match
//          case head :: tail =>
//            val updatedData = data.copy(setOfManufacturing = cats.data.NonEmptyList(head, tail))
//            InProgressOrder(updatedData, plannedDelivery).validNec
//          case Nil =>
//            OrderError.OrderWithNoManufacturing.invalidNec
//
//      case (OrderEvent.ManufacturingRemoved(_, _), _) =>
//        OrderError.CannotRemoveManufacturingInCurrentState.invalidNec
