package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import cats.syntax.all.*
import cats.data.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies
import io.github.nicolasfara.rstmanager.work.domain.task.{TaskHours, TaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ScheduledTask, ScheduledTaskError, ScheduledTaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.*
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId.given
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingError.{
  ManufacturingWithNoTasks,
  TaskError,
  TaskIdNotFound
}

enum ScheduledManufacturing(val info: ScheduledManufacturingInfo):
  case NotStartedManufacturing(override val info: ScheduledManufacturingInfo) extends ScheduledManufacturing(info)
  case InProgressManufacturing(override val info: ScheduledManufacturingInfo, startedAt: DateTime) extends ScheduledManufacturing(info)
  case CompletedManufacturing(override val info: ScheduledManufacturingInfo, startedAt: DateTime, completedAt: DateTime) extends ScheduledManufacturing(info)
  case PausedManufacturing(override val info: ScheduledManufacturingInfo, reason: Option[String], startedAt: DateTime, pausedAt: DateTime)
      extends ScheduledManufacturing(info)

  /** Calculate the total expected hours for all tasks in the manufacturing.
    * @return
    *   Total expected hours.
    */
  def expectedHours: TaskHours = info.tasks.foldMap(_.expectedHours)

  /** Calculate the total remaining hours for all tasks in the manufacturing.
    * @return
    *   Total remaining hours.
    */
  def remainingHours: TaskHours = info.tasks.foldMap(_.remainingHours)

  /** Calculate the total completed hours for all tasks in the manufacturing.
    * @return
    *   Total completed hours.
    */
  def completedHours: TaskHours = info.tasks.foldMap(_.completedHours)

  /** Add a new task to the manufacturing with specified dependencies.
    * @param task
    *   the task to add.
    * @param dependsOn
    *   the set of task IDs that the new task depends on.
    * @return
    *   the updated [[ScheduledManufacturing]] with the new task added.
    */
  def addTask(task: ScheduledTask, dependsOn: Set[TaskId]): ScheduledManufacturing =
    updateTasks(info.tasks :+ task, info.dependencies.addTaskDependencies(task.taskId, dependsOn))

  /** Remove a task from the manufacturing. If removing the task results in no remaining tasks, an error is returned.
    * @param id
    *   the ID of the task to remove.
    * @return
    *   A [[TaskIdNotFound]] error if the task ID does not exist, a [[ManufacturingWithNoTasks]] error if removing the task would leave no tasks,
    *   otherwise the updated [[ScheduledManufacturing]] without the specified task.
    */
  def removeTask(id: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    for
      task <- ensureTaskExists(id)
      updatedTasks <- info.tasks.filterNot(_.id == id) match {
        case Nil          => ManufacturingWithNoTasks.asLeft
        case head :: tail => Right(NonEmptyList(head, tail))
      }
      updatedDependencies = info.dependencies.removeTask(task.taskId)
      newManufacturing = updateTasks(updatedTasks, updatedDependencies)
    yield newManufacturing

  /** Advance the progress of a task by a specified number of hours. If the task is not found, an error is returned.
    * @param taskId
    *   the ID of the task to advance.
    * @param hours
    *   the number of hours to advance the task's progress.
    * @return
    *   an error if the task ID does not exist, otherwise the updated ScheduledManufacturing with the task's progress advanced.
    */
  def advanceTask(taskId: ScheduledTaskId, hours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.advanceInProgressTask(hours))

  /** Rollback the progress of a task by a specified number of hours. If the task is not found, an error is returned.
    * @param taskId
    *   the ID of the task to rollback.
    * @param hours
    *   the number of hours to roll back the task's progress.
    * @return
    *   an error if the task ID does not exist, otherwise the updated ScheduledManufacturing with the task's progress rolled back.
    */
  def rollbackTask(taskId: ScheduledTaskId, hours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.rollbackInProgressTask(hours))

  /** Mark a task as completed. If all tasks are completed, the manufacturing transitions to CompletedManufacturing.
    * @param taskId
    *   the ID of the task to complete.
    * @return
    *   A [[TaskIdNotFound]] error if the task ID does not exist, or the updated ScheduledManufacturing with the task marked as completed.
    */
  def completeTask(taskId: ScheduledTaskId, hours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.completeTask(hours)).flatMap { updatedManufacturing =>
      if areAllTasksCompleted(updatedManufacturing) then transitionToCompleted(updatedManufacturing)
      else Right(updatedManufacturing)
    }

  /** Revert a completed task back to in-progress state. If the manufacturing was marked as completed, it will transition back to in-progress.
    * @param taskId
    *   The ID of the task to revert.
    * @return
    *   An error if the task ID does not exist or cannot be reverted, otherwise the updated ScheduledManufacturing.
    */
  def revertTaskToInProgress(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.revertToInProgress)

  private def changeTaskState(
      taskId: ScheduledTaskId,
      f: ScheduledTask => Either[ScheduledTaskError, ScheduledTask]
  ): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    for
      _ <- ensureTaskExists(taskId)
      updatedTasks <- info.tasks.traverse { task =>
        if task.id == taskId then f(task).leftMap(TaskError.apply)
        else Right(task)
      }
      newManufacturing = updateTasks(updatedTasks, info.dependencies)
    yield transitionToInProgressIfNeeded(newManufacturing)

  private def ensureTaskExists(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledTask] =
    info.tasks.find(_.id == taskId) match
      case Some(task) => Right(task)
      case None       => TaskIdNotFound(taskId).asLeft

  private def transitionToInProgressIfNeeded(manufacturing: ScheduledManufacturing): ScheduledManufacturing =
    manufacturing match
      case NotStartedManufacturing(info) => InProgressManufacturing(info, DateTime.now())
      case other                         => other

  private def areAllTasksCompleted(manufacturing: ScheduledManufacturing): Boolean =
    manufacturing.info.tasks.forall {
      case CompletedTask(_, _, _, _, _) => true
      case _                            => false
    }

  private def getStartedAt(manufacturing: ScheduledManufacturing): DateTime =
    manufacturing match
      case InProgressManufacturing(_, startedAt)   => startedAt
      case PausedManufacturing(_, _, startedAt, _) => startedAt
      case _                                       => DateTime.now()

  private def transitionToCompleted(manufacturing: ScheduledManufacturing): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    val completedAt = DateTime.now()
    val completedTasks = NonEmptyList.fromListUnsafe(manufacturing.info.tasks.collect { case ct: CompletedTask => ct })
    Right(CompletedManufacturing(manufacturing.info.copy(tasks = completedTasks), getStartedAt(manufacturing), completedAt))

  private def updateTasks(
      tasks: NonEmptyList[ScheduledTask],
      deps: ManufacturingDependencies
  ): ScheduledManufacturing = this match
    case NotStartedManufacturing(info)            => NotStartedManufacturing(info.copy(tasks = tasks, dependencies = deps))
    case InProgressManufacturing(info, startedAt) => InProgressManufacturing(info.copy(tasks = tasks, dependencies = deps), startedAt)
    case PausedManufacturing(info, reason, startedAt, pausedAt) =>
      PausedManufacturing(info.copy(tasks = tasks, dependencies = deps), reason, startedAt, pausedAt)
    case CompletedManufacturing(info, startedAt, _) =>
      // Allow updates when reverting tasks - transition back to InProgressManufacturing
      InProgressManufacturing(info.copy(tasks = tasks, dependencies = deps), startedAt)
