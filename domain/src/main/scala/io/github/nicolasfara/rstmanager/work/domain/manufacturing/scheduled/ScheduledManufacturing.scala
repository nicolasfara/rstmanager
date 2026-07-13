package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingError.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskError, ScheduledTaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.*
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId.given

import cats.data.*
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime

/** Lifecycle status a manufacturing can be manually moved to (see [[ScheduledManufacturing.changeStatus]]). */
enum ManufacturingStatus derives CanEqual:
  case NotStarted, InProgress, Paused, Completed

/**
 * Scheduled execution of a manufacturing.
 *
 * State model:
 *   - `NotStartedManufacturing`: no task activity has happened yet.
 *   - `InProgressManufacturing`: at least one task has started.
 *   - `PausedManufacturing`: execution is temporarily halted.
 *   - `CompletedManufacturing`: every task is completed.
 */
enum ScheduledManufacturing(val info: ScheduledManufacturingInfo) derives CanEqual:
  case NotStartedManufacturing(override val info: ScheduledManufacturingInfo) extends ScheduledManufacturing(info)
  case InProgressManufacturing(override val info: ScheduledManufacturingInfo, startedAt: DateTime) extends ScheduledManufacturing(info)
  case CompletedManufacturing(override val info: ScheduledManufacturingInfo, startedAt: DateTime, completedAt: DateTime)
      extends ScheduledManufacturing(info)
  case PausedManufacturing(override val info: ScheduledManufacturingInfo, reason: Option[String], startedAt: DateTime, pausedAt: DateTime)
      extends ScheduledManufacturing(info)

  /** Returns the sum of expected hours across all tasks. */
  def expectedHours: TaskHours = info.tasks.foldMap(_.expectedHours)

  /** Returns the remaining work across all tasks. */
  def remainingHours: TaskHours = info.tasks.foldMap(_.remainingHours)

  /** Returns the total completed work across all tasks. */
  def completedHours: TaskHours = info.tasks.foldMap(_.completedHours)

  /** Adds a scheduled task and registers its dependency edges. */
  def addTask(task: ScheduledTask, dependsOn: Set[TaskId]): ScheduledManufacturing =
    updateTasks(info.tasks :+ task, info.dependencies.addTaskDependencies(task.taskId, dependsOn))

  /** Removes a scheduled task if doing so would not leave the manufacturing empty. */
  def removeTask(id: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    for
      task <- ensureTaskExists(id)
      updatedTasks <- info.tasks.filterNot(_.id == id) match
        case Nil => ManufacturingWithNoTasks.asLeft
        case head :: tail => Right(NonEmptyList(head, tail))
      updatedDependencies = info.dependencies.removeTask(task.taskId)
      newManufacturing = updateTasks(updatedTasks, updatedDependencies)
    yield newManufacturing

  /** Sets (or clears) the free-text description. */
  def withDescription(description: Option[String]): ScheduledManufacturing =
    withInfo(info.copy(description = description))

  /** Current lifecycle status of the manufacturing. */
  def status: ManufacturingStatus = this match
    case NotStartedManufacturing(_)      => ManufacturingStatus.NotStarted
    case InProgressManufacturing(_, _)   => ManufacturingStatus.InProgress
    case PausedManufacturing(_, _, _, _) => ManufacturingStatus.Paused
    case CompletedManufacturing(_, _, _) => ManufacturingStatus.Completed

  /**
   * Manually moves the manufacturing to `target`, keeping the domain invariants:
   *   - `NotStarted` / `InProgress` / `Paused` freely interconvert (pausing a not-started manufacturing first starts it);
   *   - `Completed` is only reachable when every task is already completed, otherwise [[ScheduledManufacturingError.CannotCompleteWithOpenTasks]];
   *   - reopening a `Completed` manufacturing returns it to `InProgress`.
   */
  def changeStatus(
      target: ManufacturingStatus,
      reason: Option[String] = None,
  ): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    val now = DateTime.now()
    val started = getStartedAt(this)
    target match
      case ManufacturingStatus.NotStarted => NotStartedManufacturing(info).asRight
      case ManufacturingStatus.InProgress => InProgressManufacturing(info, started).asRight
      case ManufacturingStatus.Paused     => PausedManufacturing(info, reason, started, now).asRight
      case ManufacturingStatus.Completed =>
        if areAllTasksCompleted(this) then transitionToCompleted(this)
        else ScheduledManufacturingError.CannotCompleteWithOpenTasks.asLeft

  /** Advances an in-progress task by the given amount of hours. */
  def advanceTask(taskId: ScheduledTaskId, hours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.advanceInProgressTask(hours))

  /** Rolls back progress on an in-progress task. */
  def rollbackTask(taskId: ScheduledTaskId, hours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.rollbackInProgressTask(hours))

  /** Completes a task and promotes the manufacturing to completed when all tasks are done. */
  def completeTask(taskId: ScheduledTaskId, hours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.completeTask(hours)).flatMap(promoteWhenAllTasksCompleted)

  /** Sets the absolute completed hours of a task and promotes the manufacturing to completed when all tasks are done. */
  def setTaskProgress(taskId: ScheduledTaskId, completedHours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, task => task.setProgress(completedHours).asRight).flatMap(promoteWhenAllTasksCompleted)

  /** Changes the total expected hours of a task and promotes the manufacturing to completed when all tasks are done. */
  def changeTaskExpectedHours(taskId: ScheduledTaskId, expectedHours: TaskHours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, task => task.changeExpectedHours(expectedHours).asRight).flatMap(promoteWhenAllTasksCompleted)

  /** Reopens a completed task and, if necessary, reopens the manufacturing itself. */
  def revertTaskToInProgress(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    changeTaskState(taskId, _.revertToInProgress)

  private def changeTaskState(
      taskId: ScheduledTaskId,
      f: ScheduledTask => Either[ScheduledTaskError, ScheduledTask],
  ): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    for
      _ <- ensureTaskExists(taskId)
      updatedTasks <- info.tasks.traverse { task =>
        if task.id == taskId then f(task).leftMap(TaskError.apply)
        else Right(task)
      }
      newManufacturing = updateTasks(updatedTasks, info.dependencies)
    yield transitionToInProgressIfNeeded(newManufacturing)

  private def promoteWhenAllTasksCompleted(
      manufacturing: ScheduledManufacturing,
  ): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    if areAllTasksCompleted(manufacturing) then transitionToCompleted(manufacturing)
    else Right(manufacturing)

  private def ensureTaskExists(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledTask] =
    info.tasks.find(_.id == taskId) match
      case Some(task) => Right(task)
      case None => TaskIdNotFound(taskId).asLeft

  private def transitionToInProgressIfNeeded(manufacturing: ScheduledManufacturing): ScheduledManufacturing =
    manufacturing match
      case NotStartedManufacturing(info) => InProgressManufacturing(info, DateTime.now())
      case other => other

  private def areAllTasksCompleted(manufacturing: ScheduledManufacturing): Boolean =
    manufacturing.info.tasks.forall {
      case CompletedTask(_, _, _, _, _) => true
      case _ => false
    }

  private def getStartedAt(manufacturing: ScheduledManufacturing): DateTime =
    manufacturing match
      case InProgressManufacturing(_, startedAt) => startedAt
      case PausedManufacturing(_, _, startedAt, _) => startedAt
      case _ => DateTime.now()

  private def transitionToCompleted(manufacturing: ScheduledManufacturing): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    val completedAt = DateTime.now()
    val completedTasks = NonEmptyList.fromListUnsafe(manufacturing.info.tasks.collect { case ct: CompletedTask => ct })
    Right(CompletedManufacturing(manufacturing.info.copy(tasks = completedTasks), getStartedAt(manufacturing), completedAt))

  /** Rebuilds this manufacturing with replaced `info`, preserving its exact lifecycle state and timestamps. */
  private def withInfo(newInfo: ScheduledManufacturingInfo): ScheduledManufacturing = this match
    case NotStartedManufacturing(_)                       => NotStartedManufacturing(newInfo)
    case InProgressManufacturing(_, startedAt)            => InProgressManufacturing(newInfo, startedAt)
    case PausedManufacturing(_, reason, startedAt, paused) => PausedManufacturing(newInfo, reason, startedAt, paused)
    case CompletedManufacturing(_, startedAt, completedAt) => CompletedManufacturing(newInfo, startedAt, completedAt)

  private def updateTasks(
      tasks: NonEmptyList[ScheduledTask],
      deps: ManufacturingDependencies,
  ): ScheduledManufacturing = this match
    case NotStartedManufacturing(info) => NotStartedManufacturing(info.copy(tasks = tasks, dependencies = deps))
    case InProgressManufacturing(info, startedAt) => InProgressManufacturing(info.copy(tasks = tasks, dependencies = deps), startedAt)
    case PausedManufacturing(info, reason, startedAt, pausedAt) =>
      PausedManufacturing(info.copy(tasks = tasks, dependencies = deps), reason, startedAt, pausedAt)
    case CompletedManufacturing(info, startedAt, _) =>
      // Allow updates when reverting tasks - transition back to InProgressManufacturing
      InProgressManufacturing(info.copy(tasks = tasks, dependencies = deps), startedAt)
end ScheduledManufacturing
