package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*

/**
 * Pure transformations from the create-order form drafts to the order-creation DTOs.
 *
 * Deliberately free of Laminar `Var`s and the DOM: the caller resolves the mutable draft state into the immutable
 * snapshots below, and UUID minting is injected as `newId`, so the mapping and validation rules can be unit-tested
 * deterministically.
 */
object OrderDrafts:

  /** Immutable state of one task draft in the create form. `dependsOn` holds template-task id strings. */
  final case class TaskState(taskId: String, hours: String, dependsOn: Set[String], employeeId: String)

  /**
   * Immutable state of the non-list fields of a manufacturing draft. `taskEmployees` holds, for catalog mode, the
   * per-task preferred employee (template-task id -> employee id). `dependsOn` holds sibling draft key strings.
   */
  final case class MfgCoreState(
      mode: String,
      catalogId: String,
      code: String,
      completionDate: String,
      description: String,
      employeeId: String,
      dependsOn: Set[String],
      taskEmployees: Map[String, String],
  )

  /** A manufacturing draft resolved to plain data: its stable key, core fields, and the state of its task drafts. */
  final case class MfgSnapshot(key: Int, core: MfgCoreState, tasks: List[TaskState])

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** Trims free text and collapses blank strings to `None` (an empty value clears the field). */
  private def normalizeStr(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  /** Builds a manufacturing from a catalog template, prefilling tasks/dependencies and the per-task employees. */
  def fromCatalog(
      template: ManufacturingCatalogResponse,
      completionDate: String,
      employeeId: String,
      taskEmployees: Map[String, String],
      newId: () => UUID,
  ): ManufacturingDto =
    ManufacturingDto(
      template.code,
      Formats.toIso(completionDate),
      "not_started",
      template.tasks.map { task =>
        ScheduledTaskDto(
          newId(),
          task.id,
          "pending",
          task.requiredHours,
          Some(0),
          None,
          taskEmployees.get(task.id.toString).flatMap(parseUuid),
        )
      },
      template.dependencies.map(dependency => TaskDependencyDto(dependency.taskId, dependency.dependsOn)),
      None,
      None,
      None,
      None,
      template.description,
      parseUuid(employeeId),
    )

  /** Builds a manufacturing from a custom draft: only tasks with a valid id survive, and dependencies are kept only
    * between tasks that made it into the manufacturing (self-edges dropped). */
  def fromCustom(core: MfgCoreState, tasks: List[TaskState], completionDate: String, newId: () => UUID): ManufacturingDto =
    val scheduled = tasks.flatMap { ts =>
      parseUuid(ts.taskId).map { taskId =>
        ScheduledTaskDto(newId(), taskId, "pending", ts.hours.toIntOption.getOrElse(0), Some(0), None, parseUuid(ts.employeeId))
      }
    }
    val selectedIds = scheduled.map(_.taskId.toString).toSet
    val dependencies = tasks.flatMap { ts =>
      parseUuid(ts.taskId).flatMap { taskId =>
        val dependsOn = ts.dependsOn.toList.filter(dep => selectedIds.contains(dep) && dep != taskId.toString).flatMap(parseUuid)
        if dependsOn.isEmpty then None else Some(TaskDependencyDto(taskId, dependsOn))
      }
    }
    ManufacturingDto(
      core.code.trim.nn,
      Formats.toIso(completionDate),
      "not_started",
      scheduled,
      dependencies,
      None,
      None,
      None,
      None,
      normalizeStr(core.description),
      parseUuid(core.employeeId),
    )
  end fromCustom

  /** Resolves one manufacturing snapshot to a DTO. Empty completion date falls back to the order-level work deadline. */
  def manufacturingFromDraft(
      snapshot: MfgSnapshot,
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      newId: () => UUID,
  ): Either[ApiError, ManufacturingDto] =
    val core = snapshot.core
    val completionDate = Some(core.completionDate.trim.nn).filter(_.nonEmpty).getOrElse(fallbackDeadline)
    if core.mode == "catalog" then
      catalog(core.catalogId)
        .toRight(ApiError("invalid-form", "Seleziona una lavorazione a catalogo valida.", Nil))
        .map(template => fromCatalog(template, completionDate, core.employeeId, core.taskEmployees, newId))
    else Right(fromCustom(core, snapshot.tasks, completionDate, newId))

  /** Resolves every manufacturing snapshot, short-circuiting on the first invalid one. Order is preserved. */
  def collectManufacturings(
      snapshots: List[MfgSnapshot],
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      newId: () => UUID,
  ): Either[ApiError, List[ManufacturingDto]] =
    snapshots.foldLeft(Right(List.empty): Either[ApiError, List[ManufacturingDto]]) { (acc, snapshot) =>
      acc.flatMap(list => manufacturingFromDraft(snapshot, fallbackDeadline, catalog, newId).map(list :+ _))
    }

  /** Maps the draft-key based "depends on" selections to positional indexes for the creation request. */
  def collectDependencies(snapshots: List[MfgSnapshot]): Option[List[ManufacturingDependencyByIndexDto]] =
    val indexByKey = snapshots.zipWithIndex.map((snapshot, index) => snapshot.key.toString -> index).toMap
    val entries = snapshots.zipWithIndex.flatMap { (snapshot, index) =>
      val dependsOnIndexes = snapshot.core.dependsOn.toList.flatMap(indexByKey.get).filter(_ != index).sorted
      if dependsOnIndexes.isEmpty then None else Some(ManufacturingDependencyByIndexDto(index, dependsOnIndexes))
    }
    if entries.isEmpty then None else Some(entries)
end OrderDrafts
