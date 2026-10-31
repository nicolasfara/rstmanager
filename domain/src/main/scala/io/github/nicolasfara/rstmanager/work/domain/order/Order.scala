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
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingPreferredEmployeeChanged(manufacturingId, employeeId, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Sets (or clears) the preferred employee of a task inside a manufacturing of an active or suspended order. */
  def changeTaskPreferredEmployee(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      employeeId: Option[UUID],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(
        mustBeInProgressOrSuspended.toDecision *>
          ManufacturingTaskPreferredEmployeeChanged(manufacturingId, taskId, employeeId, now).accept,
      )
      .validate(_.mustBeInProgressOrSuspended)

  /** Changes the description of a manufacturing inside an active or suspended order. */
  def changeManufacturingDescription(
      manufacturingId: ScheduledManufacturingId,
      newDescription: Option[String],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingDescriptionChanged(manufacturingId, newDescription, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Changes the work deadline of a manufacturing inside an active or suspended order. */
  def changeManufacturingCompletionDate(
      manufacturingId: ScheduledManufacturingId,
      newCompletionDate: DateTime,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(
        mustBeInProgressOrSuspended.toDecision *> ManufacturingCompletionDateChanged(manufacturingId, newCompletionDate, now).accept,
      )
      .validate(_.mustBeInProgressOrSuspended)

  /** Manually moves a manufacturing inside an active or suspended order to a new lifecycle status. */
  def changeManufacturingStatus(
      manufacturingId: ScheduledManufacturingId,
      newStatus: ManufacturingStatus,
      reason: Option[String],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingStatusChanged(manufacturingId, newStatus, reason, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Adds a task to a manufacturing inside an active or suspended order; its id must be new and its prerequisites must exist without a cycle. */
  def addManufacturingTask(
      manufacturingId: ScheduledManufacturingId,
      task: ScheduledTask,
      dependsOn: List[TaskId],
      preferredEmployeeId: Option[UUID],
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(
        mustBeEditable(validateTaskAddition(_, manufacturingId, task, dependsOn.toSet)).toDecision *>
          ManufacturingTaskAdded(manufacturingId, task, dependsOn, now, preferredEmployeeId).accept,
      )
      .validate(_.mustBeInProgressOrSuspended)

  /** Removes a task from a manufacturing inside an active or suspended order. */
  def removeManufacturingTask(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskRemoved(manufacturingId, taskId, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Adds a manufacturing to an active or suspended order; its id and task ids must be new and its task graph valid. */
  def addManufacturing(manufacturing: ScheduledManufacturing, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeEditable(validateManufacturingAddition(_, manufacturing)).toDecision *> ManufacturingAdded(manufacturing, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Removes an existing manufacturing from an active or suspended order; the last one cannot be removed. */
  def removeManufacturing(manufacturingId: ScheduledManufacturingId, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeEditable(validateManufacturingRemoval(_, manufacturingId)).toDecision *> ManufacturingRemoved(manufacturingId, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Sets the absolute progress (completed minutes) of a task inside one of the order manufacturings. */
  def setTaskProgress(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      completedDuration: TaskDuration,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(
        mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskProgressSet(
          manufacturingId,
          taskId,
          completedDuration,
          Some(now),
        ).accept,
      )
      .validate(_.mustBeInProgressOrSuspended)

  /** Changes the total expected duration of a task inside one of the order manufacturings. */
  def changeTaskExpectedDuration(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      expectedDuration: TaskDuration,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(
        mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskExpectedHoursChanged(
          manufacturingId,
          taskId,
          expectedDuration,
          Some(now),
        ).accept,
      )
      .validate(_.mustBeInProgressOrSuspended)

  /** Completes a task inside one of the order manufacturings. */
  def completeTask(
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      withDuration: TaskDuration,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(
        mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskCompleted(manufacturingId, taskId, withDuration, Some(now)).accept,
      )
      .validate(_.mustBeInProgressOrSuspended)

  /** Reopens a completed task inside one of the order manufacturings. */
  def revertTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingTaskReverted(manufacturingId, taskId, Some(now)).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Replaces the dependency graph between the manufacturings of an active or suspended order. */
  def changeManufacturingDependencies(newDependencies: OrderDependencies, now: DateTime): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> ManufacturingDependenciesChanged(newDependencies, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

  /** Replaces the task dependency graph of a manufacturing inside an active or suspended order. */
  def changeTaskDependencies(
      manufacturingId: ScheduledManufacturingId,
      newDependencies: ManufacturingDependencies,
      now: DateTime,
  ): Decision[OrderError, OrderEvent, Order] =
    this
      .perform(mustBeInProgressOrSuspended.toDecision *> TaskDependenciesChanged(manufacturingId, newDependencies, now).accept)
      .validate(_.mustBeInProgressOrSuspended)

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
    case ManufacturingAdded(manufacturing, _) => _.mustBeInProgressOrSuspended.map(addManufacturing(_, manufacturing))
    case ManufacturingRemoved(manufacturingId, _) => _.mustBeInProgressOrSuspended.map(removeManufacturing(_, manufacturingId))
    case ManufacturingPreferredEmployeeChanged(manufacturingId, employeeId, _) =>
      _.mustBeInProgressOrSuspended.andThen(changeManufacturingPreferredEmployee(_, manufacturingId, employeeId).toValidatedNec)
    case ManufacturingTaskPreferredEmployeeChanged(manufacturingId, taskId, employeeId, _) =>
      _.mustBeInProgressOrSuspended.andThen(changeTaskPreferredEmployee(_, manufacturingId, taskId, employeeId).toValidatedNec)
    case ManufacturingDescriptionChanged(manufacturingId, newDescription, _) =>
      _.mustBeInProgressOrSuspended.andThen(changeManufacturingDescription(_, manufacturingId, newDescription).toValidatedNec)
    case ManufacturingCompletionDateChanged(manufacturingId, newCompletionDate, _) =>
      _.mustBeInProgressOrSuspended.andThen(changeManufacturingCompletionDate(_, manufacturingId, newCompletionDate).toValidatedNec)
    case ManufacturingStatusChanged(manufacturingId, newStatus, reason, changedOn) =>
      _.mustBeInProgressOrSuspended.andThen(changeManufacturingStatus(_, manufacturingId, newStatus, reason, changedOn).toValidatedNec)
    case ManufacturingTaskAdded(manufacturingId, task, dependsOn, _, preferredEmployeeId) =>
      _.mustBeInProgressOrSuspended.andThen(addManufacturingTask(_, manufacturingId, task, dependsOn.toSet, preferredEmployeeId).toValidatedNec)
    case ManufacturingTaskRemoved(manufacturingId, taskId, _) =>
      _.mustBeInProgressOrSuspended.andThen(removeManufacturingTask(_, manufacturingId, taskId).toValidatedNec)
    case ManufacturingTaskAdvanced(manufacturingId, taskId, advancedBy) =>
      _.mustBeInProgressOrSuspended.andThen(advanceTask(_, manufacturingId, taskId, advancedBy).toValidatedNec)
    case ManufacturingTaskRolledBack(manufacturingId, taskId, deAdvancedBy) =>
      _.mustBeInProgressOrSuspended.andThen(rollbackTask(_, manufacturingId, taskId, deAdvancedBy).toValidatedNec)
    case ManufacturingTaskProgressSet(manufacturingId, taskId, completedDuration, occurredOn) =>
      _.mustBeInProgressOrSuspended.andThen(setTaskProgress(_, manufacturingId, taskId, completedDuration, occurredOn).toValidatedNec)
    case ManufacturingTaskExpectedHoursChanged(manufacturingId, taskId, expectedDuration, occurredOn) =>
      _.mustBeInProgressOrSuspended.andThen(changeTaskExpectedDuration(_, manufacturingId, taskId, expectedDuration, occurredOn).toValidatedNec)
    case ManufacturingTaskCompleted(manufacturingId, taskId, withDuration, occurredOn) =>
      _.mustBeInProgressOrSuspended.andThen(completeTask(_, manufacturingId, taskId, withDuration, occurredOn).toValidatedNec)
    case ManufacturingTaskReverted(manufacturingId, taskId, occurredOn) =>
      _.mustBeInProgressOrSuspended.andThen(revertTaskToInProgress(_, manufacturingId, taskId, occurredOn).toValidatedNec)
    case ManufacturingDependenciesChanged(newDependencies, _) =>
      _.mustBeInProgressOrSuspended.andThen(changeManufacturingDependencies(_, newDependencies).toValidatedNec)
    case TaskDependenciesChanged(manufacturingId, newDependencies, _) =>
      _.mustBeInProgressOrSuspended.andThen(changeTaskDependencies(_, manufacturingId, newDependencies).toValidatedNec)
  }
end Order
