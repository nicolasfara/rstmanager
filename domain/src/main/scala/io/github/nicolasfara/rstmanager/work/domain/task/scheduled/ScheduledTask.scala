package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

import cats.data.*
import cats.syntax.all.*
import io.github.nicolasfara.rstmanager.work.domain.task.{TaskHours, TaskId}
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.numeric.Interval.*
import io.github.iltotore.iron.cats.*
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskError.{TaskMustBeInProgress, TaskWithNegativeProgress}

import java.util.UUID

type ScheduledTaskId = UUID
object ScheduledTaskId:
  given CanEqual[ScheduledTaskId, ScheduledTaskId] = CanEqual.derived
  
type Percentage = DescribedAs[Closed[0, 100], "Percentage must be between 0 and 100"]

/** A scheduled task in the system.
  *
  * It can be in one of three states: [[InProgressTask]] - The task is currently being worked on. [[CompletedTask]] - The task has been completed.
  * [[PendingTask]] - The task is scheduled but not yet started.
  *
  * The transitions between states are managed through methods that ensure valid state changes. A [[InProgressTask]] can be advanced or completed, a
  * [[PendingTask]] can be marked as in progress, and a [[CompletedTask]] can be reverted to in progress. Any invalid transitions will return a
  * [[ScheduledTaskError]].
  */
enum ScheduledTask(val id: ScheduledTaskId, val taskId: TaskId, val expectedHours: TaskHours):
  case InProgressTask(id: ScheduledTaskId, taskId: TaskId, expectedHours: TaskHours, completedHours: TaskHours)
      extends ScheduledTask(id, taskId, expectedHours)
  case CompletedTask(id: ScheduledTaskId, taskId: TaskId, expectedHours: TaskHours, completedHours: TaskHours, completionDate: DateTime)
      extends ScheduledTask(id, taskId, expectedHours)
  case PendingTask(id: ScheduledTaskId, taskId: TaskId, expectedHours: TaskHours) extends ScheduledTask(id, taskId, expectedHours)

  def remainingHours: TaskHours = this match
    case InProgressTask(_, _, expectedHours, completedHours) => TaskHours.option(expectedHours - completedHours).getOrElse(TaskHours(0))
    case CompletedTask(_, _, _, _, _)                        => TaskHours(0)
    case PendingTask(_, _, expectedHours)                    => expectedHours

  def completedHours: TaskHours = this match
    case InProgressTask(_, _, _, completedHours)   => completedHours
    case CompletedTask(_, _, _, completedHours, _) => completedHours
    case PendingTask(_, _, _)                      => TaskHours(0)

  def revertToInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case CompletedTask(id, taskId, _, completedHours, _) =>
      InProgressTask(id, taskId, completedHours, completedHours).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _) => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case PendingTask(_, _, _)       => ScheduledTaskError.TaskMustBeInProgress.asLeft

  def markAsInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case PendingTask(id, taskId, expectedHours) =>
      InProgressTask(id, taskId, expectedHours, TaskHours(0)).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _)   => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case CompletedTask(_, _, _, _, _) => ScheduledTaskError.TaskAlreadyCompleted.asLeft

  def advanceInProgressTask(withHours: TaskHours): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress
    .map { task => task.copy(completedHours = task.completedHours + withHours) }

  def rollbackInProgressTask(withHours: TaskHours): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress
    .flatMap { task =>
      val newCompletedHours = task.completedHours - withHours
      if newCompletedHours < 0 then TaskWithNegativeProgress.asLeft
      else task.copy(completedHours = TaskHours.applyUnsafe(newCompletedHours)).asRight
    }

  def completeTask(withHours: TaskHours): Either[ScheduledTaskError, ScheduledTask] = this match
    case InProgressTask(id, taskId, expectedHours, completedHours) =>
      Right(CompletedTask(id, taskId, expectedHours, completedHours + withHours, DateTime.now()))
    case CompletedTask(_, _, _, _, _) => Left(ScheduledTaskError.TaskAlreadyCompleted)
    case PendingTask(_, _, _)         => Left(ScheduledTaskError.TaskMustBeInProgress)

  private def mustBeInProgress: Either[ScheduledTaskError, InProgressTask] = this match
    case task @ InProgressTask(_, _, _, _) => task.asRight
    case _                                 => TaskMustBeInProgress.asLeft

object ScheduledTask:
  def createScheduledTask(id: UUID, taskId: TaskId, expectedHours: Int): ValidatedNec[String, PendingTask] =
    (
      Validated.valid(id),
      Validated.valid(taskId),
      TaskHours.validatedNec(expectedHours)
    ).mapN(PendingTask(_, _, _))