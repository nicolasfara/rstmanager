package io.github.nicolasfara.rstmanager.work.domain.task

import java.util.UUID

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

/** Estimated duration of a task in hours. */
type TaskHours = TaskHours.T

/** Refined type companion for `TaskHours`, including arithmetic helpers and a `Monoid` instance. */
object TaskHours extends RefinedType[Int, Positive0]:
  given Monoid[TaskHours] with
    def empty: TaskHours = TaskHours(0)
    def combine(x: TaskHours, y: TaskHours): TaskHours = x + y
  extension (value: TaskHours)
    def +(other: TaskHours): TaskHours = TaskHours.applyUnsafe(value.value + other.value)
    def -(other: TaskHours): Int = value.value - other.value

/** Immutable task definition used inside manufacturings.
  *
  * @param id
  *   Stable task identifier.
  * @param name
  *   Human-readable task name.
  * @param taskDescription
  *   Optional task description.
  * @param requiredHours
  *   Estimated effort required to complete the task.
  */
final case class Task(id: TaskId, name: String :| TaskName, taskDescription: Option[String :| TaskDescription], requiredHours: TaskHours)

object Task:
  /** Creates a `Task` from raw values after applying refined validation.
    *
    * @param id
    *   Task identifier.
    * @param name
    *   Raw task name.
    * @param description
    *   Optional raw description.
    * @param requiredHours
    *   Raw task effort in hours.
    */
  def createTask(id: UUID, name: String, description: Option[String], requiredHours: Int): ValidatedNec[String, Task] =
    (
      Validated.validNec(id),
      name.refineValidatedNec[TaskName],
      description.traverse(_.refineValidatedNec[TaskDescription]),
      TaskHours.validatedNec(requiredHours),
    ).mapN(Task.apply)
