package io.github.nicolasfara.rstmanager.work.domain.order

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import edomata.core.*
import edomata.syntax.all.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.{ScheduledManufacturing, ScheduledManufacturingId}
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderOperations.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.*
import io.github.nicolasfara.rstmanager.work.domain.task.Hours
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTaskId

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

  /** Create a new order with the given [[data]] and [[plannedDelivery]]. The operation succeeds only if the current state is [[NewOrder]]. If trying
    * to create an order from any other state, it will fail with [[OrderAlreadyCreated]] error.
    */
  def create(data: OrderData, plannedDelivery: DateTime): Decision[OrderError, OrderEvent, Order] = this
    .decide {
      case NewOrder => Decision.accept(OrderCreated(data, plannedDelivery))
      case _        => Decision.reject(OrderAlreadyCreated)
    }
    .validate(_.mustBeInProgress)

  /** Suspend the order for an optional [[reason]]. The operation succeeds only if the current state is [[InProgressOrder]]. If the order is not in
    * progress, it will fail with [[OrderMustBeInProgress]] error.
    */
  def suspend(reason: Option[SuspensionReason]): Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeInProgress.toDecision *> OrderSuspended(DateTime.now(), reason).accept).validate(_.mustBeSuspended)

  /** Reactivate a previously suspended order. The operation succeeds only if the current state is [[SuspendedOrder]]. If the order is not suspended,
    * it will fail with [[OrderMustBeSuspended]] error.
    */
  def reactivate: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeSuspended.toDecision *> OrderReactivated(DateTime.now()).accept).validate(_.mustBeInProgress)

  /** Complete the order. The operation succeeds only if the current state is [[InProgressOrder]] or [[SuspendedOrder]]. If the order is not in
    * progress or paused, it will fail with [[OrderMustBeInProgress]] error.
    */
  def complete: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeInProgressOrSuspended.toDecision *> OrderCompleted(DateTime.now()).accept).validate(_.mustBeCompleted)

  /** Deliver the order. The operation succeeds only if the current state is [[CompletedOrder]]. If the order is not completed, it will fail with
    * [[OrderMustBeCompleted]] error.
    */
  def deliver: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeCompleted.toDecision *> OrderDelivered(DateTime.now()).accept).validate(_.mustBeDelivered)

  /** Cancel the order with an optional [[reason]]. The operation fails if the current state is [[NewOrder]] (no such order) or [[CancelledOrder]]
    * (already canceled). In all other states, the cancellation will succeed.
    */
  def cancel(reason: Option[String]): Decision[OrderError, OrderEvent, Order] =
    this
      .decide {
        case NewOrder                => Decision.reject(NoSuchOrder)
        case CancelledOrder(_, _, _) => Decision.reject(OrderAlreadyCancelled)
        case _                       => Decision.accept(OrderCancelled(DateTime.now(), reason))
      }
      .validate(_.mustBeCancelled)

  /** Update the planned delivery date to [[newDeliveryDate]]. The operation succeeds only if the current state is [[InProgressOrder]] or
    * [[SuspendedOrder]]. If the order is not in progress or paused, it will fail with [[OrderMustBeInProgressOrPaused]] error.
    */
  def updateDeliveryDate(newDeliveryDate: DateTime): Decision[OrderError, OrderEvent, Order] =
    this
      .decide {
        case _: InProgressOrder | _: SuspendedOrder =>
          Decision.accept(OrderDeliveryDateChanged(newDeliveryDate, DateTime.now()))
        case _ =>
          Decision.reject(OrderMustBeInProgressOrPaused)
      }
      .validate(_.mustBeInProgressOrSuspended)

  /** Reopen a previously canceled order. The operation succeeds only if the current state is [[CancelledOrder]]. If the order is not canceled, it
    * will fail with [[OnlyCancelledOrdersCanBeReactivated]] error.
    */
  def reopen: Decision[OrderError, OrderEvent, Order] =
    this
      .decide {
        case CancelledOrder(_, _, _) => Decision.accept(OrderReactivated(DateTime.now()))
        case _                       => Decision.reject(OnlyCancelledOrdersCanBeReactivated)
      }
      .validate(_.mustBeInProgress)

  /** Change the order priority to [[newPriority]]. The operation succeeds only if the current state is [[InProgressOrder]] or [[SuspendedOrder]]. If
    * the order is not in progress or paused, it will fail with [[OrderMustBeInProgressOrPaused]] error.
    */
  def changePriority(newPriority: OrderPriority): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> OrderPriorityChanged(newPriority, DateTime.now()).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Add a new [[manufacturing]] to the order. The operation succeeds only if the current state is [[InProgressOrder]] or [[SuspendedOrder]]. If the
    * order is not in progress or paused, it will fail with [[OrderMustBeInProgressOrPaused]] error.
    */
  def addManufacturing(manufacturing: ScheduledManufacturing): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingAdded(manufacturing, DateTime.now()).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Remove a manufacturing with the given [[manufacturingId]] from the order. The operation succeeds only if the current state is
    * [[InProgressOrder]] or [[SuspendedOrder]]. If the order is not in progress or paused, it will fail with [[OrderMustBeInProgressOrPaused]] error.
    */
  def removeManufacturing(manufacturingId: ScheduledManufacturingId): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingRemoved(manufacturingId, DateTime.now()).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Advance a task within a manufacturing by [[advancedBy]] hours. The operation succeeds only if the current state is [[InProgressOrder]] or
    * [[SuspendedOrder]]. If the order is not in progress or paused, it will fail with [[OrderMustBeInProgressOrPaused]] error.
    */
  def completeTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, withHours: Hours): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskCompleted(manufacturingId, taskId, withHours).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Revert a task within a manufacturing to in-progress state. The operation succeeds only if the current state is [[InProgressOrder]] or
    * [[SuspendedOrder]]. If the order is not in progress or paused, it will fail with [[OrderMustBeInProgressOrPaused]] error.
    */
  def revertTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskReverted(manufacturingId, taskId).accept)
      .validate(_.mustBeInProgressOrSuspended)

  private transparent inline def mustBe[O <: Order](onFail: OrderError): ValidatedNec[OrderError, O] = inline this match
    case o: O => o.validNec
    case _    => onFail.invalidNec

  private def mustBeDelivered: ValidatedNec[OrderError, DeliveredOrder] = mustBe[DeliveredOrder](OrderMustBeDelivered)

  private def mustBeCompleted: ValidatedNec[OrderError, CompletedOrder] = mustBe[CompletedOrder](OrderMustBeCompleted)

  private def mustBeSuspended: ValidatedNec[OrderError, SuspendedOrder] = mustBe[SuspendedOrder](OrderMustBeSuspended)

  private def mustBeCancelled: ValidatedNec[OrderError, CancelledOrder] = mustBe[CancelledOrder](OrderMustBeCancelled)

  private def mustBeInProgressOrSuspended: ValidatedNec[OrderError, InProgressOrder | SuspendedOrder] =
    mustBe[InProgressOrder | SuspendedOrder](OrderMustBeInProgressOrPaused)

  private def mustBeInProgress: ValidatedNec[OrderError, InProgressOrder] = mustBe[InProgressOrder](OrderMustBeInProgress)

object Order extends DomainModel[Order, OrderEvent, OrderError]:
  override def initial: Order = NewOrder

  override def transition: OrderEvent => Order => ValidatedNec[OrderError, Order] = {
    case OrderCreated(orderData, deliveryDate) =>
      _ => InProgressOrder(orderData, deliveryDate).validNec
    case OrderCancelled(date, reason) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, _)      => CancelledOrder(data, date, reason)
        case SuspendedOrder(data, _, _, _) => CancelledOrder(data, date, reason)
      }
    case OrderSuspended(date, reason) =>
      _.mustBeInProgress.map { case InProgressOrder(orderData, plannedDelivery) => SuspendedOrder(orderData, plannedDelivery, date, reason) }
    case OrderReactivated(_) =>
      _.mustBeSuspended.map { case SuspendedOrder(orderData, plannedDelivery, _, _) => InProgressOrder(orderData, plannedDelivery) }
    case OrderCompleted(date) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, _)      => CompletedOrder(data, date)
        case SuspendedOrder(data, _, _, _) => CompletedOrder(data, date)
      }
    case OrderDelivered(date) =>
      _.mustBeCompleted.map { case CompletedOrder(data, completionDate) => DeliveredOrder(data, completionDate, date) }
    case OrderDeliveryDateChanged(newDate, _) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, _)                  => InProgressOrder(data, newDate)
        case SuspendedOrder(data, _, pausedOn, reason) => SuspendedOrder(data, newDate, pausedOn, reason)
      }
    case OrderPriorityChanged(newPriority, _) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, plannedDelivery) => InProgressOrder(data.copy(priority = newPriority), plannedDelivery)
        case SuspendedOrder(data, plannedDelivery, pausedOn, reason) =>
          SuspendedOrder(data.copy(priority = newPriority), plannedDelivery, pausedOn, reason)
      }
    case ManufacturingAdded(manufacturing, _) =>
      _.mustBeInProgressOrSuspended.map(addManufacturing(_, manufacturing))
    case ManufacturingRemoved(manufacturingId, _) =>
      _.mustBeInProgressOrSuspended.map(removeManufacturing(_, manufacturingId))
    case ManufacturingTaskAdvanced(manufacturingId, taskId, advancedBy) =>
      _.mustBeInProgressOrSuspended.andThen(advanceTask(_, manufacturingId, taskId, advancedBy).toValidatedNec)
    case ManufacturingTaskRolledBack(manufacturingId, taskId, deAdvancedBy) =>
      _.mustBeInProgressOrSuspended.andThen(rollbackTask(_, manufacturingId, taskId, deAdvancedBy).toValidatedNec)
    case ManufacturingTaskCompleted(manufacturingId, taskId, withHours) =>
      _.mustBeInProgressOrSuspended.andThen(completeTask(_, manufacturingId, taskId, withHours).toValidatedNec)
    case ManufacturingTaskReverted(manufacturingId, taskId) =>
      _.mustBeInProgressOrSuspended.andThen(revertTaskToInProgress(_, manufacturingId, taskId).toValidatedNec)
  }
