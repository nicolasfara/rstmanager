package io.github.nicolasfara.rstmanager.work.domain.order

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderOperations.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

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

/**
 * Event-sourced aggregate root representing an order lifecycle.
 *
 * State model:
 *   - `NewOrder`: initial empty state.
 *   - `InProgressOrder`: active order being executed.
 *   - `SuspendedOrder`: temporarily paused order.
 *   - `CompletedOrder`: work is complete (can still be reopened for further changes until delivered).
 *   - `DeliveredOrder`: completed order already delivered.
 *   - `CancelledOrder`: order cancelled before delivery.
 *
 * Commands on the aggregate emit `OrderEvent` values and validate the resulting state. Decisions never read the clock: the instant recorded on the
 * emitted events (`now`) is supplied by the caller, normally the time of the command being handled.
 */
enum Order derives CanEqual:
  case NewOrder
  case InProgressOrder(data: OrderData, promisedDeliveryDate: DateTime)
  case SuspendedOrder(data: OrderData, promisedDeliveryDate: DateTime, pausedOn: DateTime, reason: Option[String :| SuspensionReason])
  case CompletedOrder(data: OrderData, completionDate: DateTime)
  case DeliveredOrder(data: OrderData, completionDate: DateTime, deliveredOn: DateTime)
  case CancelledOrder(data: OrderData, cancelledOn: DateTime, reason: Option[String :| CancellationReason])

  /**
   * Creates an order from `NewOrder`.
   *
   * Fails with `OrderAlreadyCreated` for every other state, and rejects duplicate manufacturing or task ids and dependency graphs referencing unknown
   * manufacturings/tasks or containing cycles.
   */
  def create(data: OrderData, promisedDeliveryDate: DateTime): Decision[OrderError, OrderEvent, Order] = this.decide {
    case NewOrder =>
      validateNewOrder(data) match
        case Left(error) => Decision.reject(error)
        case Right(()) => Decision.accept(OrderCreated(data, promisedDeliveryDate))
    case _ => Decision.reject(OrderAlreadyCreated)
  }
    .validate(_.mustBeInProgress)

  /** Suspends an in-progress order. */
  def suspend(reason: Option[String :| SuspensionReason], now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeInProgress.toDecision *> OrderSuspended(now, reason).accept).validate(_.mustBeSuspended)

  /** Reactivates a suspended order. */
  def reactivate(now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeSuspended.toDecision *> OrderReactivated(now).accept).validate(_.mustBeInProgress)

  /** Completes an active or suspended order. */
  def complete(now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeInProgressOrSuspended.toDecision *> OrderCompleted(now).accept).validate(_.mustBeCompleted)

  /** Delivers a completed order. */
  def deliver(now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.perform(mustBeCompleted.toDecision *> OrderDelivered(now).accept).validate(_.mustBeDelivered)

  /** Cancels an active or suspended order; completed and delivered orders can no longer be cancelled. */
  def cancel(reason: Option[String :| CancellationReason], now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.decide {
      case _: InProgressOrder | _: SuspendedOrder => Decision.accept(OrderCancelled(now, reason))
      case NewOrder => Decision.reject(NoSuchOrder)
      case CancelledOrder(_, _, _) => Decision.reject(OrderAlreadyCancelled)
      case _: CompletedOrder | _: DeliveredOrder => Decision.reject(OrderMustBeInProgressOrPaused)
    }
      .validate(_.mustBeCancelled)

  /** Changes the work-completion deadline for an active or suspended order. */
  def updatePromisedDeliveryDate(newPromisedDeliveryDate: DateTime, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.decide {
      case _: InProgressOrder | _: SuspendedOrder =>
        Decision.accept(OrderPromisedDeliveryDateChanged(newPromisedDeliveryDate, now))
      case _ =>
        Decision.reject(OrderMustBeInProgressOrPaused)
    }
      .validate(_.mustBeInProgressOrSuspended)

  /** Reopens a cancelled or completed order, returning it to the in-progress state. */
  def reopen(now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this.decide {
      case CancelledOrder(_, _, _) | CompletedOrder(_, _) => Decision.accept(OrderReactivated(now))
      case _ => Decision.reject(OnlyCancelledOrCompletedOrdersCanBeReopened)
    }
      .validate(_.mustBeInProgress)

  /** Changes the priority of an active or suspended order. */
  def changePriority(newPriority: OrderPriority, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> OrderPriorityChanged(newPriority, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Sets (or clears) the description of an active or suspended order. */
  def changeDescription(newDescription: Option[String], now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> OrderDescriptionChanged(newDescription, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Sets (or clears) the preferred employee for a manufacturing inside an active or suspended order. */
  def changeManufacturingPreferredEmployee(
      manufacturingId: ScheduledManufacturingId,
      employeeId: Option[UUID],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingPreferredEmployeeChanged(manufacturingId, employeeId, now))

  /** Sets (or clears) the preferred employee of a task inside a manufacturing of an active or suspended order. */
  def changeTaskPreferredEmployee(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      employeeId: Option[UUID],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingTaskPreferredEmployeeChanged(manufacturingId, taskId, employeeId, now))

  /** Changes the description of a manufacturing inside an active or suspended order. */
  def changeManufacturingDescription(
      manufacturingId: ScheduledManufacturingId,
      newDescription: Option[String],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingDescriptionChanged(manufacturingId, newDescription, now))

  /** Changes the work deadline of a manufacturing inside an active or suspended order. */
  def changeManufacturingCompletionDate(
      manufacturingId: ScheduledManufacturingId,
      newCompletionDate: DateTime,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingCompletionDateChanged(manufacturingId, newCompletionDate, now))

  /** Manually moves a manufacturing inside an active or suspended order to a new lifecycle status. */
  def changeManufacturingStatus(
      manufacturingId: ScheduledManufacturingId,
      newStatus: ManufacturingStatus,
      reason: Option[String],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingStatusChanged(manufacturingId, newStatus, reason, now))

  /** Adds a task to a manufacturing inside an active or suspended order; its id must be new and its prerequisites must exist without a cycle. */
  def addManufacturingTask(
      manufacturingId: ScheduledManufacturingId,
      task: ScheduledTask,
      dependsOn: List[TaskId],
      preferredEmployeeId: Option[UUID],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    editChecked(validateTaskAddition(_, manufacturingId, task, dependsOn.toSet))(
      ManufacturingTaskAdded(manufacturingId, task, dependsOn, now, preferredEmployeeId),
    )

  /** Removes a task from a manufacturing inside an active or suspended order. */
  def removeManufacturingTask(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingTaskRemoved(manufacturingId, taskId, now))

  /** Adds a manufacturing to an active or suspended order; its id and task ids must be new and its task graph valid. */
  def addManufacturing(manufacturing: ScheduledManufacturing, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    editChecked(validateManufacturingAddition(_, manufacturing))(ManufacturingAdded(manufacturing, now))

  /** Removes an existing manufacturing from an active or suspended order; the last one cannot be removed. */
  def removeManufacturing(manufacturingId: ScheduledManufacturingId, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    editChecked(validateManufacturingRemoval(_, manufacturingId))(ManufacturingRemoved(manufacturingId, now))

  /** Sets the absolute progress (completed minutes) of a task inside one of the order manufacturings. */
  def setTaskProgress(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      completedDuration: TaskDuration,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingTaskProgressSet(manufacturingId, taskId, completedDuration, Some(now)))

  /** Changes the total expected duration of a task inside one of the order manufacturings. */
  def changeTaskExpectedDuration(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      expectedDuration: TaskDuration,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingTaskExpectedHoursChanged(manufacturingId, taskId, expectedDuration, Some(now)))

  /** Completes a task inside one of the order manufacturings. */
  def completeTask(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      withDuration: TaskDuration,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingTaskCompleted(manufacturingId, taskId, withDuration, Some(now)))

  /** Reopens a completed task inside one of the order manufacturings. */
  def revertTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingTaskReverted(manufacturingId, taskId, Some(now)))

  /** Replaces the dependency graph between the manufacturings of an active or suspended order. */
  def changeManufacturingDependencies(newDependencies: OrderDependencies, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    edit(ManufacturingDependenciesChanged(newDependencies, now))

  /** Replaces the task dependency graph of a manufacturing inside an active or suspended order. */
  def changeTaskDependencies(
      manufacturingId: ScheduledManufacturingId,
      newDependencies: ManufacturingDependencies,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    edit(TaskDependenciesChanged(manufacturingId, newDependencies, now))

  /** Accepts a nested-content `event` when the change it makes ([[Order.nestedChange]]) applies to this editable order. */
  private def edit(event: OrderEvent): Decision[OrderError, OrderEvent, Order] = editChecked(_ => ().asRight)(event)

  /** Like [[edit]], with an extra structural `check` on the current order content. */
  private def editChecked(check: OrderData => Either[OrderError, Unit])(event: OrderEvent): Decision[OrderError, OrderEvent, Order] =
    val applies = mustBeEditable(check).andThen(order => Order.nestedChange(event).fold(().validNec)(change => change(order).toValidatedNec.void))
    this.perform(applies.toDecision *> Decision.accept(event)).validate(_.mustBeInProgressOrSuspended)

  private def mustBeDelivered: ValidatedNec[OrderError, DeliveredOrder] = this match
    case order: DeliveredOrder => order.validNec
    case _ => OrderMustBeDelivered.invalidNec

  private def mustBeCompleted: ValidatedNec[OrderError, CompletedOrder] = this match
    case order: CompletedOrder => order.validNec
    case _ => OrderMustBeCompleted.invalidNec

  private def mustBeSuspended: ValidatedNec[OrderError, SuspendedOrder] = this match
    case order: SuspendedOrder => order.validNec
    case _ => OrderMustBeSuspended.invalidNec

  private def mustBeCancelled: ValidatedNec[OrderError, CancelledOrder] = this match
    case order: CancelledOrder => order.validNec
    case _ => OrderMustBeCancelled.invalidNec

  private def mustBeInProgressOrSuspended: ValidatedNec[OrderError, InProgressOrder | SuspendedOrder] = this match
    case order: InProgressOrder => order.validNec
    case order: SuspendedOrder => order.validNec
    case _ => OrderMustBeInProgressOrPaused.invalidNec

  /** An editable order whose content also passes `check`. */
  private def mustBeEditable(check: OrderData => Either[OrderError, Unit]): ValidatedNec[OrderError, InProgressOrder | SuspendedOrder] =
    mustBeInProgressOrSuspended.andThen(order => check(dataOf(order)).toValidatedNec.as(order))

  /** The order content, for every state but `NewOrder`. */
  def orderData: Option[OrderData] = this match
    case NewOrder => None
    case InProgressOrder(data, _) => Some(data)
    case SuspendedOrder(data, _, _, _) => Some(data)
    case CompletedOrder(data, _) => Some(data)
    case DeliveredOrder(data, _, _) => Some(data)
    case CancelledOrder(data, _, _) => Some(data)

  private def mustBeInProgress: ValidatedNec[OrderError, InProgressOrder] = this match
    case order: InProgressOrder => order.validNec
    case _ => OrderMustBeInProgress.invalidNec
end Order

/** `DomainModel` instance for the event-sourced `Order` aggregate. */
object Order extends DomainModel[Order, OrderEvent, OrderError]:
  override def initial: Order = NewOrder

  override def transition: OrderEvent => Order => ValidatedNec[OrderError, Order] = {
    case OrderCreated(orderData, promisedDeliveryDate) => _ => InProgressOrder(orderData, promisedDeliveryDate).validNec
    case OrderCancelled(date, reason) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, _) => CancelledOrder(data, date, reason)
        case SuspendedOrder(data, _, _, _) => CancelledOrder(data, date, reason)
      }
    case OrderSuspended(date, reason) =>
      _.mustBeInProgress.map { case InProgressOrder(orderData, promisedDeliveryDate) =>
        SuspendedOrder(orderData, promisedDeliveryDate, date, reason)
      }
    case OrderReactivated(_) => {
      case SuspendedOrder(orderData, promisedDeliveryDate, _, _) => InProgressOrder(orderData, promisedDeliveryDate).validNec
      case CancelledOrder(data, _, _) => InProgressOrder(data, data.deliveryDate).validNec
      case CompletedOrder(data, _) => InProgressOrder(data, data.deliveryDate).validNec
      case _ => OrderMustBeSuspended.invalidNec
    }
    case OrderCompleted(date) =>
      _.mustBeInProgressOrSuspended.map {
        case InProgressOrder(data, _) => CompletedOrder(data, date)
        case SuspendedOrder(data, _, _, _) => CompletedOrder(data, date)
      }
    case OrderDelivered(date) =>
      _.mustBeCompleted.map { case CompletedOrder(data, completionDate) => DeliveredOrder(data, completionDate, date) }
    case OrderPromisedDeliveryDateChanged(newDate, _) =>
      _.mustBeInProgressOrSuspended.map {
        case order: InProgressOrder => order.focus(_.promisedDeliveryDate).replace(newDate)
        case order: SuspendedOrder => order.focus(_.promisedDeliveryDate).replace(newDate)
      }
    case OrderPriorityChanged(newPriority, _) =>
      _.mustBeInProgressOrSuspended.map {
        case order: InProgressOrder => order.focus(_.data.priority).replace(newPriority)
        case order: SuspendedOrder => order.focus(_.data.priority).replace(newPriority)
      }
    case OrderDescriptionChanged(newDescription, _) =>
      _.mustBeInProgressOrSuspended.map {
        case order: InProgressOrder => order.focus(_.data.description).replace(newDescription)
        case order: SuspendedOrder => order.focus(_.data.description).replace(newDescription)
      }
    case ManufacturingDependenciesChanged(newDependencies, _) =>
      _.mustBeInProgressOrSuspended.map(replayManufacturingDependencies(_, newDependencies))
    case TaskDependenciesChanged(manufacturingId, newDependencies, _) =>
      _.mustBeInProgressOrSuspended.map(replayTaskDependencies(_, manufacturingId, newDependencies))
    case nested =>
      // Content changes inside an editable order. The decision already required the change to apply; replaying an event persisted under older
      // rules must never fail, so a change that no longer applies leaves the order content unchanged.
      _.mustBeInProgressOrSuspended.map(order => nestedChange(nested).fold(order)(change => change(order).getOrElse(order)))
  }

  /**
   * The content change a nested event makes to an editable order, or `None` for lifecycle events.
   *
   * Shared by the decisions, which accept the event only when the change succeeds, and by the fold, which applies it leniently: the rules are checked
   * once, when deciding, never when replaying.
   */
  private[order] def nestedChange(event: OrderEvent): Option[ActiveOrder => Either[OrderError, ActiveOrder]] = event match
    case ManufacturingAdded(manufacturing, _) => Some(addManufacturing(_, manufacturing).asRight)
    case ManufacturingRemoved(manufacturingId, _) => Some(removeManufacturing(_, manufacturingId).asRight)
    case ManufacturingPreferredEmployeeChanged(manufacturingId, employeeId, _) =>
      Some(changeManufacturingPreferredEmployee(_, manufacturingId, employeeId))
    case ManufacturingTaskPreferredEmployeeChanged(manufacturingId, taskId, employeeId, _) =>
      Some(changeTaskPreferredEmployee(_, manufacturingId, taskId, employeeId))
    case ManufacturingDescriptionChanged(manufacturingId, newDescription, _) =>
      Some(changeManufacturingDescription(_, manufacturingId, newDescription))
    case ManufacturingCompletionDateChanged(manufacturingId, newCompletionDate, _) =>
      Some(changeManufacturingCompletionDate(_, manufacturingId, newCompletionDate))
    case ManufacturingStatusChanged(manufacturingId, newStatus, reason, changedOn) =>
      Some(changeManufacturingStatus(_, manufacturingId, newStatus, reason, changedOn))
    case ManufacturingTaskAdded(manufacturingId, task, dependsOn, _, preferredEmployeeId) =>
      Some(addManufacturingTask(_, manufacturingId, task, dependsOn.toSet, preferredEmployeeId))
    case ManufacturingTaskRemoved(manufacturingId, taskId, _) => Some(removeManufacturingTask(_, manufacturingId, taskId))
    case ManufacturingTaskAdvanced(manufacturingId, taskId, advancedBy) => Some(advanceTask(_, manufacturingId, taskId, advancedBy))
    case ManufacturingTaskRolledBack(manufacturingId, taskId, deAdvancedBy) => Some(rollbackTask(_, manufacturingId, taskId, deAdvancedBy))
    case ManufacturingTaskProgressSet(manufacturingId, taskId, completedDuration, occurredOn) =>
      Some(setTaskProgress(_, manufacturingId, taskId, completedDuration, occurredOn))
    case ManufacturingTaskExpectedHoursChanged(manufacturingId, taskId, expectedDuration, occurredOn) =>
      Some(changeTaskExpectedDuration(_, manufacturingId, taskId, expectedDuration, occurredOn))
    case ManufacturingTaskCompleted(manufacturingId, taskId, withDuration, occurredOn) =>
      Some(completeTask(_, manufacturingId, taskId, withDuration, occurredOn))
    case ManufacturingTaskReverted(manufacturingId, taskId, occurredOn) => Some(revertTaskToInProgress(_, manufacturingId, taskId, occurredOn))
    case ManufacturingDependenciesChanged(newDependencies, _) => Some(changeManufacturingDependencies(_, newDependencies))
    case TaskDependenciesChanged(manufacturingId, newDependencies, _) => Some(changeTaskDependencies(_, manufacturingId, newDependencies))
    case OrderCreated(_, _) | OrderCancelled(_, _) | OrderSuspended(_, _) | OrderReactivated(_) | OrderCompleted(_) | OrderDelivered(_) |
        OrderPromisedDeliveryDateChanged(_, _) | OrderPriorityChanged(_, _) | OrderDescriptionChanged(_, _) =>
      None
end Order
