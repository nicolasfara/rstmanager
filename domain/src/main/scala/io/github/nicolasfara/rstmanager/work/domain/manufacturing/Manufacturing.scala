package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskHours }

import cats.data.*
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs

/** Refined constraint for a non-empty manufacturing code. */
type ManufacturingCode = DescribedAs[Not[Empty], "The code manufacturing should be not empty"]

/** Refined constraint for a non-empty manufacturing name. */
type ManufacturingName = DescribedAs[Not[Empty], "The manufacturing name should be not empty"]

/** Refined constraint for a non-empty manufacturing description. */
type ManufacturingDescription = DescribedAs[Not[Empty], "The manufacturing description should be not empty"]

/** Manufacturing template composed of tasks and dependency constraints.
  *
  * @param code
  *   Unique manufacturing code.
  * @param name
  *   Human-readable manufacturing name.
  * @param description
  *   Optional textual description.
  * @param tasks
  *   Non-empty list of tasks required by the manufacturing.
  * @param dependencies
  *   Dependency graph between tasks.
  */
final case class Manufacturing(
    code: String :| ManufacturingCode,
    name: String :| ManufacturingName,
    description: Option[String :| ManufacturingDescription],
    tasks: NonEmptyList[Task],
    dependencies: ManufacturingDependencies,
):
  /** Sums the estimated hours for all tasks in the manufacturing. */
  def totalHours: TaskHours = tasks.foldLeft(TaskHours(0): TaskHours) { (acc, task) =>
    acc + task.requiredHours
  }

object Manufacturing:
  /** Creates a `Manufacturing` aggregate from raw values and validated dependencies. */
  def createManufacturing(
      code: String,
      name: String,
      description: Option[String],
      tasks: List[Task],
      dependencies: ManufacturingDependencies,
  ): ValidatedNec[String, Manufacturing] =
    (
      code.refineValidatedNec[ManufacturingCode],
      name.refineValidatedNec[ManufacturingName],
      description.traverse(_.refineValidatedNec[ManufacturingDescription]),
      NonEmptyList.fromList(tasks).toValidNec("Empty task collection provided. At least one task is required by the manufacturing"),
      Validated.valid(dependencies),
    ).mapN(Manufacturing(_, _, _, _, _))
