package io.github.nicolasfara.rstmanager.work.domain.task.schedule

import cats.syntax.all.*
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, TaskId}
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTaskError.{TaskMustBeInProgress, TaskWithNegativeProgress}

/** A scheduled task in the system.
  *
  * It can be in one of three states: [[InProgressTask]] - The task is currently being worked on. [[CompletedTask]] - The task has been completed.
  * [[PendingTask]] - The task is scheduled but not yet started.
  *
  * The transitions between states are managed through methods that ensure valid state changes. A [[InProgressTask]] can be advanced or completed, a
  * [[PendingTask]] can be marked as in progress, and a [[CompletedTask]] can be reverted to in progress. Any invalid transitions will return a
  * [[ScheduledTaskError]].
  */
enum ScheduledTask:
  case InProgressTask(id: ScheduledTaskId, taskId: TaskId, expectedHours: Hours, completedHours: Hours)
  case CompletedTask(
      id: ScheduledTaskId,
      taskId: TaskId,
      expectedHours: Hours,
      completedHours: Hours,
      completionDate: DateTime
  )
  case PendingTask(id: ScheduledTaskId, taskId: TaskId, expectedHours: Hours)

  def id: ScheduledTaskId = this match
    case InProgressTask(id, _, _, _)   => id
    case CompletedTask(id, _, _, _, _) => id
    case PendingTask(id, _, _)         => id

  def taskId: TaskId = this match
    case InProgressTask(_, taskId, _, _)   => taskId
    case CompletedTask(_, taskId, _, _, _) => taskId
    case PendingTask(_, taskId, _)         => taskId

  /** Calculate the remaining hours for the task based on its current state.
    * @return
    *   The number of hours remaining to complete the task if it is in progress or pending; zero if completed.
    */
  def remainingHours: Hours = this match
    case InProgressTask(_, _, expectedHours, completedHours) => expectedHours - completedHours
    case CompletedTask(_, _, _, _, _)                        => Hours(0)
    case PendingTask(_, _, expectedHours)                    => expectedHours

  def expectedHours: Hours = this match
    case InProgressTask(_, _, expectedHours, _)   => expectedHours
    case CompletedTask(_, _, expectedHours, _, _) => expectedHours
    case PendingTask(_, _, expectedHours)         => expectedHours

  def completedHours: Hours = this match
    case InProgressTask(_, _, _, completedHours)   => completedHours
    case CompletedTask(_, _, _, completedHours, _) => completedHours
    case PendingTask(_, _, _)                      => Hours(0)

  def revertToInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case CompletedTask(id, taskId, _, completedHours, _) =>
      InProgressTask(id, taskId, completedHours, completedHours).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _) => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case PendingTask(_, _, _)       => ScheduledTaskError.TaskMustBeInProgress.asLeft

  def markAsInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case PendingTask(id, taskId, expectedHours) =>
      InProgressTask(id, taskId, expectedHours, Hours(0)).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _)   => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case CompletedTask(_, _, _, _, _) => ScheduledTaskError.TaskAlreadyCompleted.asLeft

  def advanceInProgressTask(withHours: Hours): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress
    .map { task => task.copy(completedHours = task.completedHours + withHours) }

  def rollbackInProgressTask(withHours: Hours): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress
    .flatMap { task =>
      val newCompletedHours = task.completedHours - withHours
      if newCompletedHours < Hours(0) then TaskWithNegativeProgress.asLeft
      else task.copy(completedHours = newCompletedHours).asRight
    }

  def completeTask(withHours: Hours): Either[ScheduledTaskError, ScheduledTask] = this match
    case InProgressTask(id, taskId, expectedHours, completedHours) =>
      Right(CompletedTask(id, taskId, expectedHours, completedHours + withHours, DateTime.now()))
    case CompletedTask(_, _, _, _, _) => Left(ScheduledTaskError.TaskAlreadyCompleted)
    case PendingTask(_, _, _)         => Left(ScheduledTaskError.TaskMustBeInProgress)

  private def mustBeInProgress: Either[ScheduledTaskError, InProgressTask] = this match
    case task @ InProgressTask(_, _, _, _) => task.asRight
    case _                                 => TaskMustBeInProgress.asLeft
