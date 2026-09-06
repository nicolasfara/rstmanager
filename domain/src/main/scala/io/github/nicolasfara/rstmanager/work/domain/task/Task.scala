package io.github.nicolasfara.rstmanager.work.domain.task

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId

import cats.Monoid
import cats.data.*
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

/** Unique identifier for a task. */
type TaskId = UUID

/** Refined constraint for a non-empty task name. */
type TaskName = DescribedAs[Not[Empty], "The task name must be alphanumeric"]

/** Refined constraint for a non-empty task description. */
type TaskDescription = DescribedAs[Not[Empty], "The task description must be alphanumeric"]

/** Estimated duration of a task, expressed in total minutes. */
type TaskDuration = TaskDuration.T

/** Refined type companion for `TaskDuration` (total minutes), including arithmetic helpers and a `Monoid` instance. */
object TaskDuration extends RefinedType[Int, Positive0]:
  given Monoid[TaskDuration] with
    def empty: TaskDuration = TaskDuration(0)
    def combine(x: TaskDuration, y: TaskDuration): TaskDuration = x + y
  extension (value: TaskDuration)
    def +(other: TaskDuration): TaskDuration = TaskDuration.applyUnsafe(value.value + other.value)
    def -(other: TaskDuration): Int = value.value - other.value

/**
 * Immutable task definition used inside manufacturings.
 *
 * @param id
 *   Stable task identifier.
 * @param name
 *   Human-readable task name.
 * @param taskDescription
 *   Optional task description.
 * @param requiredDuration
 *   Estimated effort required to complete the task, in total minutes.
 * @param defaultEmployeeId
 *   Default employee proposed for this task when it is added to a manufacturing or an order; overridable at both steps.
 */
final case class Task(
    id: TaskId,
    name: String :| TaskName,
    taskDescription: Option[String :| TaskDescription],
    requiredDuration: TaskDuration,
    defaultEmployeeId: Option[EmployeeId] = None,
)

object Task:
  /**
   * Creates a `Task` from raw values after applying refined validation.
   *
   * @param id
   *   Task identifier.
   * @param name
   *   Raw task name.
   * @param description
   *   Optional raw description.
   * @param requiredMinutes
   *   Raw task effort in total minutes.
   * @param defaultEmployeeId
   *   Optional default employee proposed for this task.
   */
  def createTask(
      id: UUID,
      name: String,
      description: Option[String],
      requiredMinutes: Int,
      defaultEmployeeId: Option[EmployeeId],
  ): ValidatedNec[String, Task] =
    (
      Validated.validNec(id),
      name.refineValidatedNec[TaskName],
      description.traverse(_.refineValidatedNec[TaskDescription]),
      TaskDuration.validatedNec(requiredMinutes),
      Validated.validNec(defaultEmployeeId),
    ).mapN(Task.apply)
end Task
