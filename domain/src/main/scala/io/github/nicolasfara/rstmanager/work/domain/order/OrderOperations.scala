package io.github.nicolasfara.rstmanager.work.domain.order

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingDependencies, ManufacturingDependencyError }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{
  ManufacturingStatus,
  ScheduledManufacturing,
  ScheduledManufacturingError,
  ScheduledManufacturingId,
}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId.given
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderDependencies.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

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
        updatedData = data.replaceManufacturing(updatedManufacturing)
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
        updatedData = data.replaceManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, promisedDeliveryDate)
    order match
      case InProgressOrder(data, promisedDeliveryDate) => rollback(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => rollback(data, promisedDeliveryDate)

  /** Sets the absolute progress (completed hours) of a task inside one of the order manufacturings. */
  def setTaskProgress(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      completedHours: TaskHours,
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.setTaskProgress(taskId, completedHours))

  /** Changes the total expected hours of a task inside one of the order manufacturings. */
  def changeTaskExpectedHours(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      expectedHours: TaskHours,
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.changeTaskExpectedHours(taskId, expectedHours))

  /** Sets (or clears) the preferred employee for one of the order manufacturings. */
  def changeManufacturingPreferredEmployee(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      employeeId: Option[UUID],
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.withPreferredEmployee(employeeId).asRight)

  /** Sets (or clears) the description of one of the order manufacturings. */
  def changeManufacturingDescription(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      description: Option[String],
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.withDescription(description).asRight)

  /** Changes the work deadline of one of the order manufacturings. */
  def changeManufacturingCompletionDate(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      completionDate: DateTime,
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.withCompletionDate(completionDate).asRight)

  /** Manually moves one of the order manufacturings to a new lifecycle status. */
  def changeManufacturingStatus(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      status: ManufacturingStatus,
      reason: Option[String],
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.changeStatus(status, reason))

  /** Adds a task to one of the order manufacturings. */
  def addManufacturingTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      task: ScheduledTask,
      dependsOn: Set[TaskId],
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.addTask(task, dependsOn).asRight)

  /** Removes a task from one of the order manufacturings. */
  def removeManufacturingTask(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
  ): Either[OrderError, InProgressOrder] =
    updateManufacturing(order, manufacturingId)(_.removeTask(taskId))

  /** Replaces the dependency graph between the order manufacturings, validating references and acyclicity. */
  def changeManufacturingDependencies(
      order: InProgressOrder | SuspendedOrder,
      newDependencies: OrderDependencies,
  ): Either[OrderError, InProgressOrder] =
    def change(data: OrderData, promisedDeliveryDate: DateTime): Either[OrderError, InProgressOrder] =
      validateManufacturingDependencies(data, newDependencies).map(_ => InProgressOrder(data.withDependencies(newDependencies), promisedDeliveryDate))
    order match
      case InProgressOrder(data, promisedDeliveryDate) => change(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => change(data, promisedDeliveryDate)

  /** Replaces the task dependency graph of one of the order manufacturings, validating references and acyclicity. */
  def changeTaskDependencies(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
      newDependencies: ManufacturingDependencies,
  ): Either[OrderError, InProgressOrder] =
    def change(data: OrderData, promisedDeliveryDate: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        _ <- validateTaskDependencies(manufacturing, newDependencies)
        updatedData = data.replaceManufacturing(manufacturing.withDependencies(newDependencies))
      yield InProgressOrder(updatedData, promisedDeliveryDate)
    order match
      case InProgressOrder(data, promisedDeliveryDate) => change(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => change(data, promisedDeliveryDate)

  /** Validates every dependency graph carried by the order data: the manufacturing graph and each manufacturing's task graph. */
  def validateDependencies(data: OrderData): Either[OrderError, Unit] =
    for
      _ <- validateManufacturingDependencies(data, data.dependencies)
      _ <- data.setOfManufacturing.toList.traverse_(manufacturing => validateTaskDependencies(manufacturing, manufacturing.info.dependencies))
    yield ()

  /** Ensures the manufacturing graph only references manufacturings of the order and contains no cycle. */
  private def validateManufacturingDependencies(data: OrderData, dependencies: OrderDependencies): Either[OrderError, Unit] =
    val knownIds = data.setOfManufacturing.toList.map(_.info.id).toSet
    val unknownIds = dependencies.referencedManufacturingIds.filterNot(knownIds.contains)
    if unknownIds.nonEmpty then OrderError.UnknownManufacturingInDependencies(unknownIds).asLeft
    else
      dependencies.sort match
        case Left(OrderDependencyError.CycleDetected(cycle)) => OrderError.ManufacturingDependencyCycle(cycle).asLeft
        case Right(_) => ().asRight

  /** Ensures the task graph only references template tasks of the manufacturing and contains no cycle. */
  private def validateTaskDependencies(
      manufacturing: ScheduledManufacturing,
      dependencies: ManufacturingDependencies,
  ): Either[OrderError, Unit] =
    val knownIds = manufacturing.info.tasks.toList.map(_.taskId).toSet
    val referencedIds = dependencies.toEdgePairs.flatMap((source, target) => List(source, target)).toSet
    val unknownIds = referencedIds.filterNot(knownIds.contains)
    if unknownIds.nonEmpty then OrderError.UnknownTaskInDependencies(manufacturing.info.id, unknownIds).asLeft
    else
      dependencies.sort match
        case Left(ManufacturingDependencyError.CycleDetected(cycle)) => OrderError.TaskDependencyCycle(manufacturing.info.id, cycle).asLeft
        case Right(_) => ().asRight

  /** Applies a manufacturing-level update to the targeted manufacturing, keeping the order in progress. */
  private def updateManufacturing(
      order: InProgressOrder | SuspendedOrder,
      manufacturingId: ScheduledManufacturingId,
  )(update: ScheduledManufacturing => Either[ScheduledManufacturingError, ScheduledManufacturing]): Either[OrderError, InProgressOrder] =
    def apply(data: OrderData, promisedDeliveryDate: DateTime): Either[OrderError, InProgressOrder] =
      for
        manufacturing <- data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))
        updatedManufacturing <- update(manufacturing).leftMap(OrderError.ManufacturingError.apply)
        updatedData = data.replaceManufacturing(updatedManufacturing)
      yield InProgressOrder(updatedData, promisedDeliveryDate)
    order match
      case InProgressOrder(data, promisedDeliveryDate) => apply(data, promisedDeliveryDate)
      case SuspendedOrder(data, promisedDeliveryDate, _, _) => apply(data, promisedDeliveryDate)

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
        updatedData = data.replaceManufacturing(updatedManufacturing)
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
        updatedData = data.replaceManufacturing(updatedManufacturing)
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
