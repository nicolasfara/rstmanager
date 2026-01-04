package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import cats.syntax.all.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, TaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.{ScheduledTask, ScheduledTaskId}
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.ScheduledManufacturingError.{
  ManufacturingNotStarted,
  TaskAddedToCompletedManufacturing,
  TaskError,
  TaskIdNotFound
}

enum ScheduledManufacturing(
    val id: ScheduledManufacturingId,
    val code: ManufacturingCode,
    val completionDate: DateTime,
    val tasks: NonEmptyList[ScheduledTask],
    val dependencies: ManufacturingDependencies
):
  case NotStartedManufacturing(
      id: ScheduledManufacturingId,
      code: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies
  ) extends ScheduledManufacturing(id, code, completionDate, tasks, dependencies)
  case InProgressManufacturing(
      id: ScheduledManufacturingId,
      code: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies,
      startedAt: DateTime
  ) extends ScheduledManufacturing(id, code, completionDate, tasks, dependencies)
  case CompletedManufacturing(
      id: ScheduledManufacturingId,
      code: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask.CompletedTask],
      dependencies: ManufacturingDependencies,
      startedAt: DateTime,
      completedAt: DateTime
  ) extends ScheduledManufacturing(id, code, completionDate, tasks, dependencies)
  case PausedManufacturing(
      id: ScheduledManufacturingId,
      code: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies,
      reason: Option[String],
      startedAt: DateTime,
      pausedAt: DateTime
  ) extends ScheduledManufacturing(id, code, completionDate, tasks, dependencies)

  /** Calculate the total expected hours for all tasks in the manufacturing.
    * @return
    *   Total expected hours.
    */
  def expectedHours: Hours = tasks.foldLeft(Hours(0): Hours)(_ + _.expectedHours)

  /** Calculate the total remaining hours for all tasks in the manufacturing.
    * @return
    *   Total remaining hours.
    */
  def remainingHours: Hours = tasks.foldLeft(Hours(0): Hours)(_ + _.remainingHours)

  /** Calculate the total completed hours for all tasks in the manufacturing.
    * @return
    *   Total completed hours.
    */
  def completedHours: Hours = tasks.foldLeft(Hours(0): Hours)(_ + _.completedHours)

  def addTask(
      task: ScheduledTask,
      dependsOn: Set[TaskId]
  ): Either[ScheduledManufacturingError, ScheduledManufacturing] = this match
    case NotStartedManufacturing(id, code, cd, tasks, deps) =>
      NotStartedManufacturing(id, code, cd, tasks :+ task, deps.setDependency(task.taskId, dependsOn)).asRight
    case InProgressManufacturing(id, code, cd, tasks, deps, startedAt) =>
      InProgressManufacturing(id, code, cd, tasks :+ task, deps.setDependency(task.taskId, dependsOn), startedAt).asRight
    case CompletedManufacturing(_, _, _, _, _, _, _) =>
      TaskAddedToCompletedManufacturing.asLeft
    case PausedManufacturing(id, code, cd, tasks, deps, reason, startedAt, pausedAt) =>
      PausedManufacturing(id, code, cd, tasks :+ task, deps.setDependency(task.taskId, dependsOn), reason, startedAt, pausedAt).asRight

  def removeTask(id: TaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    ???

  def advanceTask(taskId: ScheduledTaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    ???

  def rollbackTask(taskId: ScheduledTaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    ???

  /** Mark a task as completed.
    * @param taskId
    *   the ID of the task to complete.
    * @return
    *   A [[TaskIdNotFound]] error if the task ID does not exist, or the updated ScheduledManufacturing with the task marked as completed.
    */
  def completeTask(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    ???

  /** Revert a completed task back to in-progress state. If the manufacturing is completed, an error is returned.
    * @param taskId
    *   The ID of the task to revert.
    * @return
    *   Either an error or the updated ScheduledManufacturing with the task reverted.
    */
  def revertTaskToInProgress(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] = {
    val newTasks = this match
      case NotStartedManufacturing(_, _, _, _, _) => ManufacturingNotStarted.asLeft
      case _ =>
        for
          _ <- ensureTaskExists(taskId)
          updatedTasks <- tasks.map { task =>
            if task.id == taskId then task.revertToInProgress.leftMap(TaskError.apply)
            else Right(task)
          }.sequence
        yield updatedTasks
    for
      tasks <- newTasks
      newManufacturing <- updateTasks(tasks, dependencies)
    yield newManufacturing
  }

  private def ensureTaskExists(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, Unit] =
    Either.cond(tasks.map(_.id).contains_(taskId), (), TaskIdNotFound(taskId))

  private def updateTasks(
      tasks: NonEmptyList[ScheduledTask],
      deps: ManufacturingDependencies
  ): Either[ScheduledManufacturingError, ScheduledManufacturing] = this match
    case NotStartedManufacturing(id, code, cd, _, _) =>
      Right(NotStartedManufacturing(id, code, cd, tasks, deps))
    case InProgressManufacturing(id, code, cd, _, _, startedAt) =>
      Right(InProgressManufacturing(id, code, cd, tasks, deps, startedAt))
    case PausedManufacturing(id, code, cd, _, _, reason, startedAt, pausedAt) =>
      Right(PausedManufacturing(id, code, cd, tasks, deps, reason, startedAt, pausedAt))
    case CompletedManufacturing(_, _, _, _, _, _, _) =>
      Left(ScheduledManufacturingError.ManufacturingCompleted)
