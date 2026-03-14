package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderOperations.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.*
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import edomata.core.*
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import monocle.syntax.all.*

type CancellationReason = DescribedAs[Not[Empty], "The reason, if provided, cannot be empty"]
type SuspensionReason = DescribedAs[Not[Empty], "The suspension reason, if provided, cannot be empty"]

/** Event-sourced aggregate root representing an order lifecycle.
  *
  * State model:
  *  - `NewOrder`: initial empty state.
  *  - `InProgressOrder`: active order being executed.
  *  - `SuspendedOrder`: temporarily paused order.
  *  - `CompletedOrder`: work is complete.
  *  - `DeliveredOrder`: completed order already delivered.
  *  - `CancelledOrder`: order cancelled before delivery.
  *
  * Commands on the aggregate emit `OrderEvent` values and validate the resulting state.
  */
enum Order derives CanEqual:
  case NewOrder
  case InProgressOrder(data: OrderData, plannedDelivery: DateTime)
  case SuspendedOrder(data: OrderData, plannedDelivery: DateTime, pausedOn: DateTime, reason: Option[String :| SuspensionReason])
  case CompletedOrder(data: OrderData, completionDate: DateTime)
  case DeliveredOrder(data: OrderData, completionDate: DateTime, deliveredOn: DateTime)
  case CancelledOrder(data: OrderData, cancelledOn: DateTime, reason: Option[String :| CancellationReason])

  /** Creates an order from `NewOrder`.
    *
    * Fails with `OrderAlreadyCreated` for every other state.
    */
  def create(data: OrderData, plannedDelivery: DateTime): Decision[OrderError, OrderEvent, Order] = this.decide {
    case NewOrder => Decision.accept(OrderCreated(data, plannedDelivery))
    case _ => Decision.reject(OrderAlreadyCreated)
  }
    .validate(_.mustBeInProgress)

  /** Suspends an in-progress order. */
  def suspend(reason: Option[String :| SuspensionReason]): Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeInProgress.toDecision *> OrderSuspended(DateTime.now(), reason).accept).validate(_.mustBeSuspended)

  /** Reactivates a suspended order. */
  def reactivate: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeSuspended.toDecision *> OrderReactivated(DateTime.now()).accept).validate(_.mustBeInProgress)

  /** Completes an active or suspended order. */
  def complete: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeInProgressOrSuspended.toDecision *> OrderCompleted(DateTime.now()).accept).validate(_.mustBeCompleted)

  /** Delivers a completed order. */
  def deliver: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeCompleted.toDecision *> OrderDelivered(DateTime.now()).accept).validate(_.mustBeDelivered)

  /** Cancels the order unless it has not been created yet or is already cancelled. */
  def cancel(reason: Option[String :| CancellationReason]): Decision[OrderError, OrderEvent, Order] =
    this.decide {
      case NewOrder => Decision.reject(NoSuchOrder)
      case CancelledOrder(_, _, _) => Decision.reject(OrderAlreadyCancelled)
      case _ => Decision.accept(OrderCancelled(DateTime.now(), reason))
    }
      .validate(_.mustBeCancelled)

  /** Changes the planned delivery date for an active or suspended order. */
  def updateDeliveryDate(newDeliveryDate: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.decide {
      case _: InProgressOrder | _: SuspendedOrder =>
        Decision.accept(OrderDeliveryDateChanged(newDeliveryDate, DateTime.now()))
      case _ =>
        Decision.reject(OrderMustBeInProgressOrPaused)
    }
      .validate(_.mustBeInProgressOrSuspended)

  /** Reopens a cancelled order, returning it to the in-progress state. */
  def reopen: Decision[OrderError, OrderEvent, Order] =
    this.decide {
      case CancelledOrder(_, _, _) => Decision.accept(OrderReactivated(DateTime.now()))
      case _ => Decision.reject(OnlyCancelledOrdersCanBeReactivated)
    }
      .validate(_.mustBeInProgress)

  /** Changes the priority of an active or suspended order. */
  def changePriority(newPriority: OrderPriority): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> OrderPriorityChanged(newPriority, DateTime.now()).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Adds a manufacturing to an active or suspended order. */
  def addManufacturing(manufacturing: ScheduledManufacturing): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingAdded(manufacturing, DateTime.now()).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Removes a manufacturing from an active or suspended order. */
  def removeManufacturing(manufacturingId: ScheduledManufacturingId): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingRemoved(manufacturingId, DateTime.now()).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Completes a task inside one of the order manufacturings. */
  def completeTask(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      withHours: TaskHours,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskCompleted(manufacturingId, taskId, withHours).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Reopens a completed task inside one of the order manufacturings. */
  def revertTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskReverted(manufacturingId, taskId).accept)
      .validate(_.mustBeInProgressOrSuspended)

  transparent inline private def mustBe[O <: Order](onFail: OrderError): ValidatedNec[OrderError, O] = inline this match
    case o: O => o.validNec
    case _ => onFail.invalidNec

  private def mustBeDelivered: ValidatedNec[OrderError, DeliveredOrder] = mustBe[DeliveredOrder](OrderMustBeDelivered)

  private def mustBeCompleted: ValidatedNec[OrderError, CompletedOrder] = mustBe[CompletedOrder](OrderMustBeCompleted)

  private def mustBeSuspended: ValidatedNec[OrderError, SuspendedOrder] = mustBe[SuspendedOrder](OrderMustBeSuspended)

  private def mustBeCancelled: ValidatedNec[OrderError, CancelledOrder] = mustBe[CancelledOrder](OrderMustBeCancelled)

  private def mustBeInProgressOrSuspended: ValidatedNec[OrderError, InProgressOrder | SuspendedOrder] =
    mustBe[InProgressOrder | SuspendedOrder](OrderMustBeInProgressOrPaused)

  private def mustBeInProgress: ValidatedNec[OrderError, InProgressOrder] = mustBe[InProgressOrder](OrderMustBeInProgress)
end Order

/** `DomainModel` instance for the event-sourced `Order` aggregate. */
object Order extends DomainModel[Order, OrderEvent, OrderError]:
  override def initial: Order = NewOrder

  override def transition: OrderEvent => Order => ValidatedNec[OrderError, Order] = {
    case OrderCreated(orderData, deliveryDate) => _ => InProgressOrder(orderData, deliveryDate).validNec
    case OrderCancelled(date, reason) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, _) => CancelledOrder(data, date, reason)
        case SuspendedOrder(data, _, _, _) => CancelledOrder(data, date, reason)
      }
    case OrderSuspended(date, reason) =>
      _.mustBeInProgress.map { case InProgressOrder(orderData, plannedDelivery) => SuspendedOrder(orderData, plannedDelivery, date, reason) }
    case OrderReactivated(_) => {
      case SuspendedOrder(orderData, plannedDelivery, _, _) => InProgressOrder(orderData, plannedDelivery).validNec
      case CancelledOrder(data, _, _) => InProgressOrder(data, data.deliveryDate).validNec
      case _ => OrderMustBeSuspended.invalidNec
    }
    case OrderCompleted(date) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, _) => CompletedOrder(data, date)
        case SuspendedOrder(data, _, _, _) => CompletedOrder(data, date)
      }
    case OrderDelivered(date) =>
      _.mustBeCompleted.map { case CompletedOrder(data, completionDate) => DeliveredOrder(data, completionDate, date) }
    case OrderDeliveryDateChanged(newDate, _) =>
      _.mustBeInProgressOrSuspended.map {
        case order: InProgressOrder => order.focus(_.plannedDelivery).replace(newDate)
        case order: SuspendedOrder => order.focus(_.plannedDelivery).replace(newDate)
      }
    case OrderPriorityChanged(newPriority, _) =>
      _.mustBeInProgressOrSuspended.map {
        case order: InProgressOrder => order.focus(_.data.priority).replace(newPriority)
        case order: SuspendedOrder => order.focus(_.data.priority).replace(newPriority)
      }
    case ManufacturingAdded(manufacturing, _) => _.mustBeInProgressOrSuspended.map(addManufacturing(_, manufacturing))
    case ManufacturingRemoved(manufacturingId, _) => _.mustBeInProgressOrSuspended.map(removeManufacturing(_, manufacturingId))
    case ManufacturingTaskAdvanced(manufacturingId, taskId, advancedBy) =>
      _.mustBeInProgressOrSuspended.andThen(advanceTask(_, manufacturingId, taskId, advancedBy).toValidatedNec)
    case ManufacturingTaskRolledBack(manufacturingId, taskId, deAdvancedBy) =>
      _.mustBeInProgressOrSuspended.andThen(rollbackTask(_, manufacturingId, taskId, deAdvancedBy).toValidatedNec)
    case ManufacturingTaskCompleted(manufacturingId, taskId, withHours) =>
      _.mustBeInProgressOrSuspended.andThen(completeTask(_, manufacturingId, taskId, withHours).toValidatedNec)
    case ManufacturingTaskReverted(manufacturingId, taskId) =>
      _.mustBeInProgressOrSuspended.andThen(revertTaskToInProgress(_, manufacturingId, taskId).toValidatedNec)
  }
end Order
