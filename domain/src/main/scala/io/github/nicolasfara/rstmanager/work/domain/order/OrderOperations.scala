package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId.given
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import monocle.syntax.all.*

/** Pure helpers used by `Order.transition` to update nested manufacturings and tasks. */
object OrderOperations:
  /** Adds a manufacturing to an active or suspended order. */
  def addManufacturing(order: InProgressOrder | SuspendedOrder, manufacturing: ScheduledManufacturing): Order =
    order match
      case order: InProgressOrder => order.focus(_.data).modify(_.addManufacturing(manufacturing))
      case order: SuspendedOrder => order.focus(_.data).modify(_.addManufacturing(manufacturing))

  /** Removes a manufacturing from an active or suspended order. */
  def removeManufacturing(order: InProgressOrder | SuspendedOrder, manufacturingId: ScheduledManufacturingId): Order =
    order match
      case order: InProgressOrder => order.focus(_.data).modify(_.removeManufacturing(manufacturingId))
      case order: SuspendedOrder => order.focus(_.data).modify(_.removeManufacturing(manufacturingId))

  /** Advances a task inside one of the order manufacturings. */
  def advanceTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      advancedBy: TaskHours,
  ): Either[OrderError, InProgressOrder] =
    def advance(data: OrderData, promisedDeliveryDate: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.advanceTask(taskId, advancedBy).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, promisedDeliveryDate)
    order match
      case InProgressOrder(data, promisedDeliveryDate) => advance(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => advance(data, promisedDeliveryDate)

  /** Rolls back task progress inside one of the order manufacturings. */
  def rollbackTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      rollbackBy: TaskHours,
  ): Either[OrderError, InProgressOrder] =
    def rollback(data: OrderData, promisedDeliveryDate: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.rollbackTask(taskId, rollbackBy).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, promisedDeliveryDate)
    order match
      case InProgressOrder(data, promisedDeliveryDate) => rollback(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => rollback(data, promisedDeliveryDate)

  /** Completes a task and completes the whole order when every manufacturing is done. */
  def completeTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      withHours: TaskHours,
  ): Either[OrderError, Order] =
    def complete(data: OrderData, promisedDeliveryDate: DateTime): Either[OrderError, Order] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.completeTask(taskId, withHours).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield
        if areAllManufacturingsCompleted(updatedData) then CompletedOrder(updatedData, DateTime.now())
        else InProgressOrder(updatedData, promisedDeliveryDate)
    order match
      case InProgressOrder(data, promisedDeliveryDate) => complete(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => complete(data, promisedDeliveryDate)

  /** Reopens a task inside one of the order manufacturings. */
  def revertTaskToInProgress(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
  ): Either[OrderError, InProgressOrder] =
    def revert(data: OrderData, promisedDeliveryDate: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.revertTaskToInProgress(taskId).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, promisedDeliveryDate)
    order match
      case InProgressOrder(data, promisedDeliveryDate) => revert(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => revert(data, promisedDeliveryDate)

  private def areAllManufacturingsCompleted(data: OrderData): Boolean =
    data.setOfManufacturing.forall {
      case _: ScheduledManufacturing.CompletedManufacturing => true
      case _ => false
    }
end OrderOperations
