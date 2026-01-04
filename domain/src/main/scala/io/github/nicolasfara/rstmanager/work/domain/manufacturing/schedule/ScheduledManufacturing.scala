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
  TaskAddedToCompletedManufacturing,
  TaskError,
  TaskIdNotFound
}

enum ScheduledManufacturing:
  case NotStartedManufacturing(
      id: ScheduledManufacturingId,
      manufacturingCode: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies
  )
  case InProgressManufacturing(
      id: ScheduledManufacturingId,
      manufacturingCode: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies,
      startedAt: DateTime
  )
  case CompletedManufacturing(
      id: ScheduledManufacturingId,
      manufacturingCode: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask.CompletedTask],
      startedAt: DateTime,
      completedAt: DateTime
  )
  case PausedManufacturing(
      id: ScheduledManufacturingId,
      manufacturingCode: ManufacturingCode,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies,
      reason: Option[String],
      startedAt: DateTime,
      pausedAt: DateTime
  )

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
    case CompletedManufacturing(_, _, _, _, _, _) =>
      TaskAddedToCompletedManufacturing.asLeft
    case PausedManufacturing(id, code, cd, tasks, deps, reason, startedAt, pausedAt) =>
      PausedManufacturing(id, code, cd, tasks :+ task, deps.setDependency(task.taskId, dependsOn), reason, startedAt, pausedAt).asRight

  def removeTask(id: TaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] = for
    deps <- dependencies
    updatedDeps = deps.removeTaskDependency(id)
    updatedTasksUnsafe = tasks.filterNot(_.taskId == id)
    updatedTasks <- updatedTasksUnsafe match
      case head :: tail => Right(NonEmptyList(head, tail))
      case Nil          => Left(ScheduledManufacturingError.ManufacturingWithNoTasks)
    newManufacturing <- updateTasks(updatedTasks, updatedDeps)
  yield newManufacturing

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
    case CompletedManufacturing(_, _, _, _, _, _) =>
      Left(ScheduledManufacturingError.ManufacturingCompleted)

  def advanceTask(taskId: ScheduledTaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    ???

  def rollbackTask(taskId: ScheduledTaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    ???

  def completeTask(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    ???

  def revertTaskToInProgress(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    Either
      .cond(tasks.map(_.id).contains_(taskId), (), TaskIdNotFound(taskId))
      .flatMap { _ =>
        for
          updatedTasks <- tasks.map { task =>
            if task.id == taskId then task.revertToInProgress.leftMap(TaskError.apply)
            else Right(task)
          }.sequence
          deps <- dependencies
          newManufacturing <- updateTasks(updatedTasks, deps)
        yield newManufacturing
      }

  private def dependencies: Either[ScheduledManufacturingError, ManufacturingDependencies] = this match
    case NotStartedManufacturing(_, _, _, _, deps)      => Right(deps)
    case InProgressManufacturing(_, _, _, _, deps, _)   => Right(deps)
    case CompletedManufacturing(_, _, _, _, _, _)       => Left(ScheduledManufacturingError.ManufacturingCompleted)
    case PausedManufacturing(_, _, _, _, deps, _, _, _) => Right(deps)

  private def tasks: NonEmptyList[ScheduledTask] = this match
    case NotStartedManufacturing(_, _, _, tasks, _)      => tasks
    case InProgressManufacturing(_, _, _, tasks, _, _)   => tasks
    case CompletedManufacturing(_, _, _, tasks, _, _)    => tasks
    case PausedManufacturing(_, _, _, tasks, _, _, _, _) => tasks
