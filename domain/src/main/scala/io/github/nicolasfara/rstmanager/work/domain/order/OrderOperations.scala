package io.github.nicolasfara.rstmanager.work.domain.order

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ScheduledManufacturing, ScheduledManufacturingId}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId.given
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId
import monocle.syntax.all.*

object OrderOperations:
  def addManufacturing(order: InProgressOrder | SuspendedOrder, manufacturing: ScheduledManufacturing): Order =
    order match
      case order: InProgressOrder => order.focus(_.data).modify(_.addManufacturing(manufacturing))
      case order: SuspendedOrder  => order.focus(_.data).modify(_.addManufacturing(manufacturing))

  def removeManufacturing(order: InProgressOrder | SuspendedOrder, manufacturingId: ScheduledManufacturingId): Order =
    order match
      case order: InProgressOrder => order.focus(_.data).modify(_.removeManufacturing(manufacturingId))
      case order: SuspendedOrder  => order.focus(_.data).modify(_.removeManufacturing(manufacturingId))

  def advanceTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      advancedBy: TaskHours
  ): Either[OrderError, InProgressOrder] =
    def advance(data: OrderData, plannedDelivery: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.advanceTask(taskId, advancedBy).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, plannedDelivery)
    order match
      case InProgressOrder(data, plannedDelivery)      => advance(data, plannedDelivery)
      case SuspendedOrder(data, plannedDelivery, _, _) => advance(data, plannedDelivery)

  def rollbackTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      rollbackBy: TaskHours
  ): Either[OrderError, InProgressOrder] =
    def rollback(data: OrderData, plannedDelivery: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.rollbackTask(taskId, rollbackBy).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, plannedDelivery)
    order match
      case InProgressOrder(data, plannedDelivery)      => rollback(data, plannedDelivery)
      case SuspendedOrder(data, plannedDelivery, _, _) => rollback(data, plannedDelivery)

  def completeTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      withHours: TaskHours
  ): Either[OrderError, Order] =
    def complete(data: OrderData, plannedDelivery: DateTime): Either[OrderError, Order] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.completeTask(taskId, withHours).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield
        if areAllManufacturingsCompleted(updatedData) then CompletedOrder(updatedData, DateTime.now())
        else InProgressOrder(updatedData, plannedDelivery)
    order match
      case InProgressOrder(data, plannedDelivery)      => complete(data, plannedDelivery)
      case SuspendedOrder(data, plannedDelivery, _, _) => complete(data, plannedDelivery)

  def revertTaskToInProgress(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId
  ): Either[OrderError, InProgressOrder] =
    def revert(data: OrderData, plannedDelivery: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.revertTaskToInProgress(taskId).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, plannedDelivery)
    order match
      case InProgressOrder(data, plannedDelivery)      => revert(data, plannedDelivery)
      case SuspendedOrder(data, plannedDelivery, _, _) => revert(data, plannedDelivery)

  private def areAllManufacturingsCompleted(data: OrderData): Boolean =
    data.setOfManufacturing.forall {
      case _: ScheduledManufacturing.CompletedManufacturing => true
      case _                                                => false
    }
