package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import cats.data.*
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.nicolasfara.rstmanager.work.domain.task.{TaskHours, Task}
import io.github.iltotore.iron.constraint.any.DescribedAs

type ManufacturingCode = DescribedAs[Not[Empty], "The code manufacturing should be not empty"]
type ManufacturingName = DescribedAs[Not[Empty], "The manufacturing name should be not empty"]
type ManufacturingDescription = DescribedAs[Not[Empty], "The manufacturing description should be not empty"]

/** Aggregate root representing a manufacturing process. It is composed of multiple tasks with dependencies.
  */
final case class Manufacturing(
    code: String :| ManufacturingCode,
    name: String :| ManufacturingName,
    description: Option[String :| ManufacturingDescription],
    tasks: NonEmptyList[Task],
    dependencies: ManufacturingDependencies
):
  def totalHours: TaskHours = tasks.foldLeft(TaskHours(0): TaskHours) { (acc, task) =>
    acc + task.requiredHours
  }

object Manufacturing:
  def createManufacturing(
      code: String,
      name: String,
      description: Option[String],
      tasks: List[Task],
      dependencies: ManufacturingDependencies
  ): ValidatedNec[String, Manufacturing] =
    (
      code.refineValidatedNec[ManufacturingCode],
      name.refineValidatedNec[ManufacturingName],
      description.traverse(_.refineValidatedNec[ManufacturingDescription]),
      NonEmptyList.fromList(tasks).toValidNec("Empty task collection provided. At least one task is required by the manufacturing"),
      Validated.valid(dependencies)
    ).mapN(Manufacturing(_, _, _, _, _))
