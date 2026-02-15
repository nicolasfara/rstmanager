package io.github.nicolasfara.rstmanager.work.domain.task

import cats.syntax.all.*
import cats.data.*
import cats.Monoid
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

import java.util.UUID

type TaskId = UUID
type TaskName = DescribedAs[Not[Empty], "The task name must be alphanumeric"]
type TaskDescription = DescribedAs[Not[Empty], "The task description must be alphanumeric"]
type TaskHours = TaskHours.T
object TaskHours extends RefinedType[Int, Positive]:
  given Monoid[TaskHours] with
    def combine(x: TaskHours, y: TaskHours): TaskHours = x + y
  extension (value: TaskHours)
    def +(other: TaskHours): TaskHours = TaskHours.applyUnsafe(value.value + other.value)
    def -(other: TaskHours): Int = value.value - other.value

/** Entity that represents a Task in the system.
  */
final case class Task(id: TaskId, name: String :| TaskName, taskDescription: Option[String :| TaskDescription], requiredHours: TaskHours)

object Task:
  /** Smart constructor for a [[Task]] providing the [[id]], [[name]], an optional [[description]], and the [[requiredHours]] to complete the task.
   * The creation may fail if an empty name, an empty description if provided or a negative value for required hours are provided.  
   */
  def createTask(id: UUID, name: String, description: Option[String], requiredHours: Int): ValidatedNec[String, Task] =
    (
      Validated.validNec(id),
      name.refineValidatedNec[TaskName],
      description.traverse(_.refineValidatedNec[TaskDescription]),
      TaskHours.validatedNec(requiredHours)
    ).mapN(Task.apply)
