package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
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

/**
 * Task instance tracked inside a scheduled manufacturing.
 *
 * State model:
 *   - `PendingTask`: planned but not started.
 *   - `InProgressTask`: work has started and progress can move forward or backward.
 *   - `CompletedTask`: work is finished and carries a completion timestamp.
 *
 * Transition methods keep the state machine consistent and return `ScheduledTaskError` when a requested change is invalid.
 */
enum ScheduledTask(val id: ScheduledTaskId, val taskId: TaskId, val expectedDuration: TaskDuration):
  case InProgressTask(
      override val id: ScheduledTaskId,
      override val taskId: TaskId,
      override val expectedDuration: TaskDuration,
      override val completedDuration: TaskDuration,
  ) extends ScheduledTask(id, taskId, expectedDuration)
  case CompletedTask(
      override val id: ScheduledTaskId,
      override val taskId: TaskId,
      override val expectedDuration: TaskDuration,
      override val completedDuration: TaskDuration,
      completionDate: DateTime,
  ) extends ScheduledTask(id, taskId, expectedDuration)
  case PendingTask(override val id: ScheduledTaskId, override val taskId: TaskId, override val expectedDuration: TaskDuration)
      extends ScheduledTask(id, taskId, expectedDuration)

  /** Returns the amount of work still left on the task, in minutes. */
  def remainingDuration: TaskDuration = this match
    case InProgressTask(_, _, expectedDuration, completedDuration) =>
      TaskDuration.option(expectedDuration - completedDuration).getOrElse(TaskDuration(0))
    case CompletedTask(_, _, _, _, _) => TaskDuration(0)
    case PendingTask(_, _, expectedDuration) => expectedDuration

  /** Returns the amount of work already completed on the task, in minutes. */
  def completedDuration: TaskDuration = this match
    case InProgressTask(_, _, _, completedDuration) => completedDuration
    case CompletedTask(_, _, _, completedDuration, _) => completedDuration
    case PendingTask(_, _, _) => TaskDuration(0)

  /** Reopens a completed task so progress can continue. */
  def revertToInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case CompletedTask(id, taskId, _, completedDuration, _) =>
      InProgressTask(id, taskId, completedDuration, completedDuration).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _) => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case PendingTask(_, _, _) => ScheduledTaskError.TaskMustBeInProgress.asLeft

  /** Marks a pending task as started. */
  def markAsInProgress: Either[ScheduledTaskError, ScheduledTask] = this match
    case PendingTask(id, taskId, expectedDuration) =>
      InProgressTask(id, taskId, expectedDuration, TaskDuration(0)).asRight[ScheduledTaskError]
    case InProgressTask(_, _, _, _) => ScheduledTaskError.TaskAlreadyInProgress.asLeft
    case CompletedTask(_, _, _, _, _) => ScheduledTaskError.TaskAlreadyCompleted.asLeft

  /** Advances the progress of an in-progress task by the given duration. */
  def advanceInProgressTask(withDuration: TaskDuration): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress
    .map(_.focus(_.completedDuration).modify(_ + withDuration))

  /** Rolls back progress on an in-progress task by the given duration. */
  def rollbackInProgressTask(withDuration: TaskDuration): Either[ScheduledTaskError, ScheduledTask] = mustBeInProgress.flatMap { task =>
    val newCompletedDuration = task.completedDuration - withDuration
    if newCompletedDuration < 0 then TaskWithNegativeProgress.asLeft
    else task.focus(_.completedDuration).replace(TaskDuration.applyUnsafe(newCompletedDuration)).asRight
  }

  /**
   * Sets the absolute completed duration of the task, re-deriving its state from the new progress:
   *   - `>= expectedDuration` completes the task (recording the completion timestamp),
   *   - `<= 0` returns it to pending,
   *   - otherwise it is (or stays) in progress.
   *
   * Unlike [[advanceInProgressTask]], this works from any state, so it can start a pending task or reopen a completed one.
   */
  def setProgress(completed: TaskDuration): ScheduledTask = withDuration(expectedDuration, completed)

  /**
   * Changes the total expected duration while preserving the completed duration, re-deriving the task state accordingly (a task whose completed
   * duration now covers the new estimate becomes completed, and vice versa).
   */
  def changeExpectedDuration(newExpectedDuration: TaskDuration): ScheduledTask = withDuration(newExpectedDuration, completedDuration)

  private def withDuration(expected: TaskDuration, completed: TaskDuration): ScheduledTask =
    if completed.value >= expected.value then CompletedTask(id, taskId, expected, completed, DateTime.now())
    else if completed.value <= 0 then PendingTask(id, taskId, expected)
    else InProgressTask(id, taskId, expected, completed)

  /** Completes an in-progress task and records the completion timestamp. */
  def completeTask(withDuration: TaskDuration): Either[ScheduledTaskError, ScheduledTask] = this match
    case InProgressTask(id, taskId, expectedDuration, completedDuration) =>
      Right(CompletedTask(id, taskId, expectedDuration, completedDuration + withDuration, DateTime.now()))
    case CompletedTask(_, _, _, _, _) => Left(ScheduledTaskError.TaskAlreadyCompleted)
    case PendingTask(_, _, _) => Left(ScheduledTaskError.TaskMustBeInProgress)

  private def mustBeInProgress: Either[ScheduledTaskError, InProgressTask] = this match
    case task @ InProgressTask(_, _, _, _) => task.asRight
    case _ => TaskMustBeInProgress.asLeft
end ScheduledTask

object ScheduledTask:
  /** Creates a pending scheduled task from raw input values (`expectedMinutes` in total minutes). */
  def createScheduledTask(id: UUID, taskId: TaskId, expectedMinutes: Int): ValidatedNec[String, PendingTask] =
    (
      Validated.valid(id),
      Validated.valid(taskId),
      TaskDuration.validatedNec(expectedMinutes),
    ).mapN(PendingTask(_, _, _))
