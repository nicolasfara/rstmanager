package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId

import cats.data.*
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs

/** Unique identifier for a catalog manufacturing. */
type ManufacturingId = UUID

/** Refined constraint for a non-empty manufacturing code. */
type ManufacturingCode = DescribedAs[Not[Empty], "The code manufacturing should be not empty"]

/** Refined constraint for a non-empty manufacturing name. */
type ManufacturingName = DescribedAs[Not[Empty], "The manufacturing name should be not empty"]

/** Refined constraint for a non-empty manufacturing description. */
type ManufacturingDescription = DescribedAs[Not[Empty], "The manufacturing description should be not empty"]

/**
 * Manufacturing catalog template composed of live references to catalog tasks.
 *
 * @param id
 *   Stable manufacturing identifier.
 * @param code
 *   Unique manufacturing code.
 * @param name
 *   Human-readable manufacturing name.
 * @param description
 *   Optional textual description.
 * @param taskIds
 *   Ordered, non-empty list of catalog task ids required by the manufacturing.
 * @param dependencies
 *   Dependency graph between tasks.
 * @param defaultEmployees
 *   Default employee proposed for each task when the manufacturing is scheduled inside an order; keys must reference `taskIds`.
 */
final case class Manufacturing(
    id: ManufacturingId,
    code: String :| ManufacturingCode,
    name: String :| ManufacturingName,
    description: Option[String :| ManufacturingDescription],
    taskIds: NonEmptyList[TaskId],
    dependencies: ManufacturingDependencies,
    defaultEmployees: Map[TaskId, EmployeeId] = Map.empty,
)

object Manufacturing:
  /** Creates a catalog manufacturing from raw values and validated task dependency edges. */
  def createManufacturing(
      id: UUID,
      code: String,
      name: String,
      description: Option[String],
      taskIds: List[TaskId],
      dependencies: ManufacturingDependencies,
      defaultEmployees: Map[TaskId, EmployeeId] = Map.empty,
  ): ValidatedNec[String, Manufacturing] =
    (
      Validated.validNec(id),
      code.refineValidatedNec[ManufacturingCode],
      name.refineValidatedNec[ManufacturingName],
      description.traverse(_.refineValidatedNec[ManufacturingDescription]),
      validateTaskIds(taskIds),
      validateDependencies(taskIds, dependencies),
      validateDefaultEmployees(taskIds, defaultEmployees),
    ).mapN(Manufacturing(_, _, _, _, _, _, _))

  private def validateTaskIds(taskIds: List[TaskId]): ValidatedNec[String, NonEmptyList[TaskId]] =
    val duplicateIds = taskIds.groupBy(identity).collect { case (id, ids) if ids.size > 1 => id }.toList
    (
      NonEmptyList.fromList(taskIds).toValidNec("Empty task collection provided. At least one task is required by the manufacturing"),
      if duplicateIds.isEmpty then ().validNec else s"Duplicate task ids are not allowed: ${duplicateIds.mkString(", ")}".invalidNec,
    ).mapN((ids, _) => ids)

  private def validateDependencies(taskIds: List[TaskId], dependencies: ManufacturingDependencies): ValidatedNec[String, ManufacturingDependencies] =
    val allowed = taskIds.toSet
    val outside = dependencies.toEdgePairs.flatMap((task, dependsOn) => List(task, dependsOn)).filterNot(allowed.contains).distinct
    (
      if outside.isEmpty then ().validNec
      else s"Dependencies reference task ids that are not part of this manufacturing: ${outside.mkString(", ")}".invalidNec,
      if dependencies.hasCycle then "Manufacturing dependencies cannot contain cycles.".invalidNec else ().validNec,
    ).mapN((_, _) => dependencies)

  private def validateDefaultEmployees(
      taskIds: List[TaskId],
      defaultEmployees: Map[TaskId, EmployeeId],
  ): ValidatedNec[String, Map[TaskId, EmployeeId]] =
    val allowed = taskIds.toSet
    val outside = defaultEmployees.keys.filterNot(allowed.contains).toList
    if outside.isEmpty then defaultEmployees.validNec
    else s"Default employees reference task ids that are not part of this manufacturing: ${outside.mkString(", ")}".invalidNec
end Manufacturing
