package io.github.nicolasfara.rstmanager.work.domain.order

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import edomata.core.*
import edomata.syntax.all.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.*

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
    this.perform(mustBeInProgress.toDecision *> OrderSuspended(reason, DateTime.now()).accept).validate(_.mustBeSuspended)

  /** Reactivate a previously suspended order. The operation succeeds only if the current state is [[SuspendedOrder]]. If the order is not suspended,
    * it will fail with [[OrderMustBeSuspended]] error.
    */
  def reactivate: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeSuspended.toDecision *> OrderReactivated(DateTime.now()).accept).validate(_.mustBeInProgress)

  /** Complete the order. The operation succeeds only if the current state is [[InProgressOrder]] or [[SuspendedOrder]]. If the order is not in
    * progress or paused, it will fail with [[OrderMustBeInProgress]] error.
    */
  def complete: Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeInProgressOrPaused.toDecision *> OrderCompleted(DateTime.now()).accept).validate(_.mustBeCompleted)

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
      .validate(_.mustBeInProgressOrPaused)

  /** Reopen a previously canceled order. The operation succeeds only if the current state is [[CancelledOrder]]. If the order is not canceled, it
    * will fail with [[OnlyCancelledOrdersCanBeReactivated]] error.
    */
  def reopen: Decision[OrderError, OrderEvent, Order] =
    this
      .decide {
        case CancelledOrder(data, _, _) => Decision.accept(OrderReactivated(DateTime.now()))
        case _                          => Decision.reject(OnlyCancelledOrdersCanBeReactivated)
      }
      .validate(_.mustBeInProgress)

  def changePriority(newPriority: OrderPriority): Decision[OrderError, OrderEvent, Order] =
    this
      .decide {
        case _: InProgressOrder | _: SuspendedOrder =>
          Decision.accept(OrderPriorityChanged(newPriority, DateTime.now()))
        case _ => Decision.reject(OrderMustBeInProgressOrPaused)
      }
      .validate(_.mustBeInProgressOrPaused)

  private transparent inline def mustBe[O <: Order](onFail: OrderError): ValidatedNec[OrderError, O] = this match
    case o: O => o.validNec
    case _    => onFail.invalidNec

  private def mustBeDelivered: ValidatedNec[OrderError, DeliveredOrder] = mustBe[DeliveredOrder](OrderMustBeDelivered)

  private def mustBeCompleted: ValidatedNec[OrderError, CompletedOrder] = mustBe[CompletedOrder](OrderMustBeCompleted)

  private def mustBeSuspended: ValidatedNec[OrderError, SuspendedOrder] = mustBe[SuspendedOrder](OrderMustBeSuspended)

  private def mustBeCancelled: ValidatedNec[OrderError, CancelledOrder] = mustBe[CancelledOrder](OrderMustBeCancelled)

  private def mustBeInProgressOrPaused: ValidatedNec[OrderError, InProgressOrder | SuspendedOrder] =
    mustBe[InProgressOrder | SuspendedOrder](OrderMustBeInProgressOrPaused)

  private def mustBeInProgress: ValidatedNec[OrderError, InProgressOrder] = mustBe[InProgressOrder](OrderMustBeInProgress)

object Order extends DomainModel[Order, OrderEvent, OrderError]:
  override def initial: Order = NewOrder

  override def transition: OrderEvent => Order => ValidatedNec[OrderError, Order] = ???
