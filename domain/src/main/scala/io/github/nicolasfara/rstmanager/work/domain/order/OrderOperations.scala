package io.github.nicolasfara.rstmanager.work.domain.order

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.{ScheduledManufacturing, ScheduledManufacturingId}
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

object OrderOperations:
  protected[order] def addManufacturing(order: InProgressOrder | SuspendedOrder, manufacturing: ScheduledManufacturing): Order =
    order match
      case InProgressOrder(data, plannedDelivery) =>
        InProgressOrder(data.addManufacturing(manufacturing), plannedDelivery)
      case SuspendedOrder(data, plannedDelivery, pausedOn, reason) =>
        SuspendedOrder(data.addManufacturing(manufacturing), plannedDelivery, pausedOn, reason)

  protected[order] def removeManufacturing(order: InProgressOrder | SuspendedOrder, manufacturingId: ScheduledManufacturingId): Order =
    order match
      case InProgressOrder(data, plannedDelivery) =>
        InProgressOrder(data.removeManufacturing(manufacturingId), plannedDelivery)
      case SuspendedOrder(data, plannedDelivery, pausedOn, reason) =>
        SuspendedOrder(data.removeManufacturing(manufacturingId), plannedDelivery, pausedOn, reason)

  protected[order] def advanceTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      advancedBy: TaskHours
  ): Either[OrderError, InProgressOrder] = {
    def advance(data: OrderData): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.advanceTask(taskId, advancedBy).leftMap(ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, data.deliveryDate)
    order match {
      case InProgressOrder(data, _)      => advance(data)
      case SuspendedOrder(data, _, _, _) => advance(data)
    }
  }

  protected[order] def rollbackTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      rollbackBy: TaskHours
  ): Either[OrderError, InProgressOrder] = {
    def rollback(data: OrderData): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.rollbackTask(taskId, rollbackBy).leftMap(ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, data.deliveryDate)
    order match {
      case InProgressOrder(data, _)      => rollback(data)
      case SuspendedOrder(data, _, _, _) => rollback(data)
    }
  }

  private def areAllManufacturingsCompleted(data: OrderData): Boolean =
    data.setOfManufacturing.forall {
      case _: ScheduledManufacturing.CompletedManufacturing => true
      case _                                                => false
    }

  protected[order] def completeTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      withHours: TaskHours
  ): Either[OrderError, Order] = {
    def complete(data: OrderData): Either[OrderError, Order] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.completeTask(taskId, withHours).leftMap(ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield
        if areAllManufacturingsCompleted(updatedData) then CompletedOrder(updatedData, DateTime.now())
        else InProgressOrder(updatedData, data.deliveryDate)
    order match {
      case InProgressOrder(data, _)      => complete(data)
      case SuspendedOrder(data, _, _, _) => complete(data)
    }
  }

  protected[order] def revertTaskToInProgress(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId
  ): Either[OrderError, InProgressOrder] = {
    def revert(data: OrderData): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- manufacturing.revertTaskToInProgress(taskId).leftMap(ManufacturingError.apply)
        updatedData = data.removeManufacturing(manufacturingId).addManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, data.deliveryDate)

    order match {
      case InProgressOrder(data, _)      => revert(data)
      case SuspendedOrder(data, _, _, _) => revert(data)
    }
  }
