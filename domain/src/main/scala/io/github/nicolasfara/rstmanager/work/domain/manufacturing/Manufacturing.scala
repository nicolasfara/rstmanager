package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, Task}

import io.github.iltotore.iron.*
import org.scalactic.anyvals.NonEmptySet

/** Aggregate root representing a manufacturing process. It is composed of multiple tasks with dependencies.
  */
final case class Manufacturing(
    code: ManufacturingCode,
    name: ManufacturingName,
    description: Option[ManufacturingDescription],
    tasks: NonEmptySet[Task],
    dependencies: ManufacturingDependencies
):
  def totalHours: Hours = tasks.foldLeft(Hours(0): Hours) { (acc, task) =>
    acc + task.requiredHours
  }
