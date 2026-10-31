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
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import monocle.syntax.all.*

/**
 * Pure helpers used by `Order.transition` to update nested manufacturings and tasks.
 *
 * Every helper edits the order content while preserving its lifecycle state: a suspended order stays suspended (with its pause date and reason), an
 * in-progress order stays in progress. Task-level changes may complete a manufacturing, but never the order itself: completing an order is the
 * deliberate `Complete` lifecycle command.
 *
 * Helpers that may change a lifecycle timestamp take the event's `occurredOn`; events persisted before that field existed carry `None`, in which case
 * the order creation date is used so that replay stays deterministic.
 */
object OrderOperations:
  /** An order whose content can still be edited. */
  type ActiveOrder = InProgressOrder | SuspendedOrder

  /** Adds a manufacturing to an active or suspended order. */
  def addManufacturing(order: ActiveOrder, manufacturing: ScheduledManufacturing): ActiveOrder =
    mapData(order)(_.addManufacturing(manufacturing))

  /** Removes a manufacturing from an active or suspended order. */
  def removeManufacturing(order: ActiveOrder, manufacturingId: ScheduledManufacturingId): ActiveOrder =
    mapData(order)(_.removeManufacturing(manufacturingId))

  /** Advances a task inside one of the order manufacturings (legacy event only). */
  def advanceTask(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      advancedBy: TaskDuration,
  ): Either[OrderError, ActiveOrder] =
    updateManufacturingAt(order, manufacturingId, None)((manufacturing, at) => manufacturing.advanceTask(taskId, advancedBy, at))

  /** Rolls back task progress inside one of the order manufacturings (legacy event only). */
  def rollbackTask(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      rollbackBy: TaskDuration,
  ): Either[OrderError, ActiveOrder] =
    updateManufacturingAt(order, manufacturingId, None)((manufacturing, at) => manufacturing.rollbackTask(taskId, rollbackBy, at))

  /** Sets the absolute progress (completed minutes) of a task inside one of the order manufacturings. */
  def setTaskProgress(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      completedDuration: TaskDuration,
      occurredOn: Option[DateTime],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturingAt(order, manufacturingId, occurredOn)((manufacturing, at) => manufacturing.setTaskProgress(taskId, completedDuration, at))

  /** Changes the total expected duration of a task inside one of the order manufacturings. */
  def changeTaskExpectedDuration(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      expectedDuration: TaskDuration,
      occurredOn: Option[DateTime],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturingAt(order, manufacturingId, occurredOn) { (manufacturing, at) =>
      manufacturing.changeTaskExpectedDuration(taskId, expectedDuration, at)
    }

  /** Completes a task, completing its manufacturing when every task is done; the order itself stays active. */
  def completeTask(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      withDuration: TaskDuration,
      occurredOn: Option[DateTime],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturingAt(order, manufacturingId, occurredOn)((manufacturing, at) => manufacturing.completeTask(taskId, withDuration, at))

  /** Reopens a task inside one of the order manufacturings. */
  def revertTaskToInProgress(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      occurredOn: Option[DateTime],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturingAt(order, manufacturingId, occurredOn)((manufacturing, at) => manufacturing.revertTaskToInProgress(taskId, at))

  /** Manually moves one of the order manufacturings to a new lifecycle status at the instant the change was recorded. */
  def changeManufacturingStatus(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      status: ManufacturingStatus,
      reason: Option[String],
      changedOn: DateTime,
  ): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(_.changeStatus(status, reason, changedOn))

  /** Sets (or clears) the preferred employee for one of the order manufacturings. */
  def changeManufacturingPreferredEmployee(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      employeeId: Option[UUID],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(_.withPreferredEmployee(employeeId).asRight)

  /** Sets (or clears) the preferred employee of a task inside one of the order manufacturings. */
  def changeTaskPreferredEmployee(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
      employeeId: Option[UUID],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(_.withTaskPreferredEmployee(taskId, employeeId))

  /** Sets (or clears) the description of one of the order manufacturings. */
  def changeManufacturingDescription(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      description: Option[String],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(_.withDescription(description).asRight)

  /** Changes the work deadline of one of the order manufacturings. */
  def changeManufacturingCompletionDate(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      completionDate: DateTime,
  ): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(_.withCompletionDate(completionDate).asRight)

  /** Adds a task to one of the order manufacturings. */
  def addManufacturingTask(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      task: ScheduledTask,
      dependsOn: Set[TaskId],
      preferredEmployee: Option[UUID],
  ): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(_.addTask(task, dependsOn, preferredEmployee).asRight)

  /** Removes a task from one of the order manufacturings. */
  def removeManufacturingTask(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      taskId: ScheduledTaskId,
  ): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(_.removeTask(taskId))

  /** Replaces the dependency graph between the order manufacturings, validating references and acyclicity. */
  def changeManufacturingDependencies(order: ActiveOrder, newDependencies: OrderDependencies): Either[OrderError, ActiveOrder] =
    editData(order)(data => validateManufacturingDependencies(data, newDependencies).as(data.withDependencies(newDependencies)))

  /** Replaces the task dependency graph of one of the order manufacturings, validating references and acyclicity. */
  def changeTaskDependencies(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      newDependencies: ManufacturingDependencies,
  ): Either[OrderError, ActiveOrder] =
    editData(order) { data =>
      for
        manufacturing <- findManufacturing(data, manufacturingId)
        _ <- validateTaskDependencies(manufacturing, newDependencies)
      yield data.replaceManufacturing(manufacturing.withDependencies(newDependencies))
    }

  /** Checks a new order: unique manufacturing and task ids, and valid manufacturing and task dependency graphs. */
  def validateNewOrder(data: OrderData): Either[OrderError, Unit] =
    uniqueIds(data.setOfManufacturing.toList) *> data.setOfManufacturing.toList.traverse_(noRepeatedCatalogTasks) *> validateDependencies(data)

  /** Checks that a manufacturing can join the order: its id and task ids are new, and its task dependency graph is valid. */
  def validateManufacturingAddition(data: OrderData, manufacturing: ScheduledManufacturing): Either[OrderError, Unit] =
    uniqueIds(manufacturing :: data.setOfManufacturing.toList) *> noRepeatedCatalogTasks(manufacturing) *>
      validateTaskDependencies(manufacturing, manufacturing.info.dependencies)

  /** Checks that a task can join a manufacturing of the order: its id is new, and its prerequisites exist in the manufacturing without a cycle. */
  def validateTaskAddition(
      data: OrderData,
      manufacturingId: ScheduledManufacturingId,
      task: ScheduledTask,
      dependsOn: Set[TaskId],
  ): Either[OrderError, Unit] =
    findManufacturing(data, manufacturingId).flatMap { manufacturing =>
      val withTask = manufacturing.addTask(task, dependsOn, None)
      uniqueIds(withTask :: data.setOfManufacturing.toList.filterNot(_.info.id == manufacturingId)) *> noRepeatedCatalogTasks(withTask) *>
        validateTaskDependencies(withTask, withTask.info.dependencies)
    }

  /** Checks that a manufacturing exists and is not the last one: an order always keeps at least one manufacturing. */
  def validateManufacturingRemoval(data: OrderData, manufacturingId: ScheduledManufacturingId): Either[OrderError, Unit] =
    findManufacturing(data, manufacturingId) *>
      Either.cond(data.setOfManufacturing.size > 1, (), OrderError.CannotRemoveLastManufacturing)

  /** Applies a recorded manufacturing dependency graph as-is: validation happened when the event was decided, under the rules of that time. */
  def replayManufacturingDependencies(order: ActiveOrder, dependencies: OrderDependencies): ActiveOrder =
    mapData(order)(_.withDependencies(dependencies))

  /**
   * Applies a recorded task dependency graph as-is (see [[replayManufacturingDependencies]]); an unknown manufacturing leaves the order unchanged.
   */
  def replayTaskDependencies(order: ActiveOrder, manufacturingId: ScheduledManufacturingId, dependencies: ManufacturingDependencies): ActiveOrder =
    mapData(order)(data =>
      findManufacturing(data, manufacturingId)
        .fold(_ => data, manufacturing => data.replaceManufacturing(manufacturing.withDependencies(dependencies))),
    )

  /** Validates every dependency graph carried by the order data: the manufacturing graph and each manufacturing's task graph. */
  def validateDependencies(data: OrderData): Either[OrderError, Unit] =
    for
      _ <- validateManufacturingDependencies(data, data.dependencies)
      _ <- data.setOfManufacturing.toList.traverse_(manufacturing => validateTaskDependencies(manufacturing, manufacturing.info.dependencies))
    yield ()

  /** Ensures no two manufacturings share an id and no two scheduled tasks share an id anywhere in the order. */
  private def uniqueIds(manufacturings: List[ScheduledManufacturing]): Either[OrderError, Unit] =
    val duplicateManufacturings = duplicates(manufacturings.map(_.info.id))
    val duplicateTasks = duplicates(manufacturings.flatMap(_.info.tasks.toList.map(_.id)))
    if duplicateManufacturings.nonEmpty then OrderError.DuplicateManufacturingIds(duplicateManufacturings).asLeft
    else if duplicateTasks.nonEmpty then OrderError.DuplicateTaskIds(duplicateTasks).asLeft
    else ().asRight

  /**
   * Ensures a manufacturing schedules each catalog task at most once. Task dependencies are keyed by catalog task id (as in the templates and the
   * order editor), so a repeated catalog task would make them ambiguous; more work on the same task means raising its expected duration.
   */
  private def noRepeatedCatalogTasks(manufacturing: ScheduledManufacturing): Either[OrderError, Unit] =
    val repeated = duplicates(manufacturing.info.tasks.toList.map(_.taskId))
    Either.cond(repeated.isEmpty, (), OrderError.RepeatedCatalogTasks(manufacturing.info.id, repeated))

  private def duplicates[A](values: List[A]): Set[A] =
    values.groupBy(identity).collect { case (value, occurrences) if occurrences.size > 1 => value }.toSet

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

  /** Applies a manufacturing-level update that needs the event instant (falling back to the order creation date for legacy events). */
  private def updateManufacturingAt(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
      occurredOn: Option[DateTime],
  )(update: (ScheduledManufacturing, DateTime) => Either[ScheduledManufacturingError, ScheduledManufacturing]): Either[OrderError, ActiveOrder] =
    updateManufacturing(order, manufacturingId)(update(_, occurredOn.getOrElse(dataOf(order).creationDate)))

  /** Applies a manufacturing-level update to the targeted manufacturing. */
  private def updateManufacturing(
      order: ActiveOrder,
      manufacturingId: ScheduledManufacturingId,
  )(update: ScheduledManufacturing => Either[ScheduledManufacturingError, ScheduledManufacturing]): Either[OrderError, ActiveOrder] =
    editData(order) { data =>
      for
        manufacturing <- findManufacturing(data, manufacturingId)
        updatedManufacturing <- update(manufacturing).leftMap(OrderError.ManufacturingError.apply)
      yield data.replaceManufacturing(updatedManufacturing)
    }

  private def findManufacturing(data: OrderData, manufacturingId: ScheduledManufacturingId): Either[OrderError, ScheduledManufacturing] =
    data.setOfManufacturing.find(_.info.id == manufacturingId).toRight(OrderError.ManufacturingNotFound(manufacturingId))

  /** The content of an editable order. */
  def dataOf(order: ActiveOrder): OrderData = order match
    case order: InProgressOrder => order.data
    case order: SuspendedOrder => order.data

  /** Edits the order data, keeping the order in its current (in-progress or suspended) state. */
  private def editData(order: ActiveOrder)(edit: OrderData => Either[OrderError, OrderData]): Either[OrderError, ActiveOrder] =
    order match
      case order: InProgressOrder => edit(order.data).map(data => order.focus(_.data).replace(data))
      case order: SuspendedOrder => edit(order.data).map(data => order.focus(_.data).replace(data))

  private def mapData(order: ActiveOrder)(update: OrderData => OrderData): ActiveOrder =
    order match
      case order: InProgressOrder => order.focus(_.data).modify(update)
      case order: SuspendedOrder => order.focus(_.data).modify(update)
end OrderOperations
