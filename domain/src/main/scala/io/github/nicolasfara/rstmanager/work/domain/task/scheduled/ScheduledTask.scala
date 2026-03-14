package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskError.{ TaskMustBeInProgress, TaskWithNegativeProgress }

import cats.data.*
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.numeric.Interval.*
import monocle.syntax.all.*

/** Unique identifier for a scheduled task instance. */
type ScheduledTaskId = UUID
object ScheduledTaskId:
  given CanEqual[ScheduledTaskId, ScheduledTaskId] = CanEqual.derived

/** Percentage value constrained to the inclusive range `0` to `100`. */
type Percentage = DescribedAs[Closed[0, 100], "Percentage must be between 0 and 100"]

/** Task instance tracked inside a scheduled manufacturing.
  *
  * State model:
  *  - `PendingTask`: planned but not started.
  *  - `InProgressTask`: work has started and progress can move forward or backward.
  *  - `CompletedTask`: work is finished and carries a completion timestamp.
  *
  * Transition methods keep the state machine consistent and return `ScheduledTaskError` when a
  * requested change is invalid.
  */
enum ScheduledTask(val id: ScheduledTaskId, val taskId: TaskId, val expectedHours: TaskHours):
  case InProgressTask(
      override val id: ScheduledTaskId,
      override val taskId: TaskId,
      override val expectedHours: TaskHours,
      override val completedHours: TaskHours,
  ) extends ScheduledTask(id, taskId, expectedHours)
  case CompletedTask(
      override val id: ScheduledTaskId,
      override val taskId: TaskId,
      override val expectedHours: TaskHours,
      override val completedHours: TaskHours,
      completionDate: DateTime,
  ) extends ScheduledTask(id, taskId, expectedHours)
  case PendingTask(override val id: ScheduledTaskId, override val taskId: TaskId, override val expectedHours: TaskHours)
      extends ScheduledTask(id, taskId, expectedHours)

  /** Returns the amount of work still left on the task. */
  def remainingHours: TaskHours = this match
    case InProgressTask(_, _, expectedHours, completedHours) => TaskHours.option(expectedHours - completedHours).getOrElse(TaskHours(0))
    case CompletedTask(_, _, _, _, _) => TaskHours(0)
    case PendingTask(_, _, expectedHours) => expectedHours

  /** Returns the amount of work already completed on the task. */
  def completedHours: TaskHours = this match
    case InProgressTask(_, _, _, completedHours) => completedHours
    case CompletedTask(_, _, _, completedHours, _) => completedHours
    case PendingTask(_, _, _) => TaskHours(0)

  /** Reopens a completed task so progress can continue. */
  def revertToInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case CompletedTask(id, taskId, _, completedHours, _) =>
      InProgressTask(id, taskId, completedHours, completedHours).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _) => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case PendingTask(_, _, _) => ScheduledTaskError.TaskMustBeInProgress.asLeft

  /** Marks a pending task as started. */
  def markAsInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case PendingTask(id, taskId, expectedHours) =>
      InProgressTask(id, taskId, expectedHours, TaskHours(0)).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _) => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case CompletedTask(_, _, _, _, _) => ScheduledTaskError.TaskAlreadyCompleted.asLeft

  /** Advances the progress of an in-progress task by the given hours. */
  def advanceInProgressTask(withHours: TaskHours): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress
    .map(_.focus(_.completedHours).modify(_ + withHours))

  /** Rolls back progress on an in-progress task by the given hours. */
  def rollbackInProgressTask(withHours: TaskHours): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress.flatMap { task =>
    val newCompletedHours = task.completedHours - withHours
    if newCompletedHours < 0 then TaskWithNegativeProgress.asLeft
    else task.focus(_.completedHours).replace(TaskHours.applyUnsafe(newCompletedHours)).asRight
  }

  /** Completes an in-progress task and records the completion timestamp. */
  def completeTask(withHours: TaskHours): Either[ScheduledTaskError, ScheduledTask] = this match
    case InProgressTask(id, taskId, expectedHours, completedHours) =>
      Right(CompletedTask(id, taskId, expectedHours, completedHours + withHours, DateTime.now()))
    case CompletedTask(_, _, _, _, _) => Left(ScheduledTaskError.TaskAlreadyCompleted)
    case PendingTask(_, _, _) => Left(ScheduledTaskError.TaskMustBeInProgress)

  private def mustBeInProgress: Either[ScheduledTaskError, InProgressTask] = this match
    case task @ InProgressTask(_, _, _, _) => task.asRight
    case _ => TaskMustBeInProgress.asLeft
end ScheduledTask

object ScheduledTask:
  /** Creates a pending scheduled task from raw input values. */
  def createScheduledTask(id: UUID, taskId: TaskId, expectedHours: Int): ValidatedNec[String, PendingTask] =
    (
      Validated.valid(id),
      Validated.valid(taskId),
      TaskHours.validatedNec(expectedHours),
    ).mapN(PendingTask(_, _, _))
