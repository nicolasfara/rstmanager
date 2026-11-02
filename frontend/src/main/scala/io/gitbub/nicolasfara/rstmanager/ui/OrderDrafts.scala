package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*

/**
 * Pure transformations from the create-order form drafts to the order-creation DTOs.
 *
 * Deliberately free of Laminar `Var`s and the DOM: the caller resolves the mutable draft state into the immutable snapshots below, and UUID minting
 * is injected as `newId`, so the mapping and validation rules can be unit-tested deterministically.
 */
object OrderDrafts:

  /** Immutable state of one task draft in the create form. `dependsOn` holds template-task id strings. */
  final case class TaskState(taskId: String, hours: String, dependsOn: Set[String], employeeId: String)

  /**
   * Immutable state of the non-list fields of a manufacturing draft. `taskEmployees` holds, for catalog mode, the per-task preferred employee
   * (template-task id -> employee id). `dependsOn` holds sibling draft key strings.
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

  /**
   * A manufacturing draft resolved to plain data: its stable key, core fields, the state of its task drafts, and its material lines (see
   * [[MaterialDrafts]]; blank lines are ignored, none means no materials).
   */
  final case class MfgSnapshot(key: Int, core: MfgCoreState, tasks: List[TaskState], materials: List[MaterialDrafts.Row] = Nil)

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
    // Per-task duration override (minutes) configured on the catalog template; a task without an entry keeps its task-catalog duration.
    val durationOverride = template.taskDurations.map(entry => entry.taskId -> entry.minutes).toMap
    ManufacturingDto(
      template.code,
      Formats.toIso(completionDate),
      "not_started",
      template.tasks.map { task =>
        ScheduledTaskDto(
          newId(),
          task.id,
          "pending",
          durationOverride.getOrElse(task.id, task.requiredMinutes),
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
  end fromCatalog

  /**
   * Builds a manufacturing from a custom draft: only tasks with a valid id survive, and dependencies are kept only between tasks that made it into
   * the manufacturing (self-edges dropped).
   */
  def fromCustom(core: MfgCoreState, tasks: List[TaskState], completionDate: String, newId: () => UUID): ManufacturingDto =
    val scheduled = tasks.flatMap { ts =>
      parseUuid(ts.taskId).map { taskId =>
        ScheduledTaskDto(newId(), taskId, "pending", Formats.parseDuration(ts.hours).getOrElse(0), Some(0), None, parseUuid(ts.employeeId))
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

  /** Resolves one manufacturing snapshot to a DTO, without materials context (any material line is then reported as unavailable). */
  def manufacturingFromDraft(
      snapshot: MfgSnapshot,
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      newId: () => UUID,
  ): Either[ApiError, ManufacturingDto] =
    manufacturingFromDraft(snapshot, fallbackDeadline, catalog, MaterialDrafts.Context.empty, newId)

  /**
   * Resolves one manufacturing snapshot to a DTO. Empty completion date falls back to the order-level work deadline; the material lines are checked
   * against `materials` (a manufacturing without lines omits `materials`).
   */
  def manufacturingFromDraft(
      snapshot: MfgSnapshot,
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      materials: MaterialDrafts.Context,
      newId: () => UUID,
  ): Either[ApiError, ManufacturingDto] =
    val core = snapshot.core
    val completionDate = Some(core.completionDate.trim.nn).filter(_.nonEmpty).getOrElse(fallbackDeadline)
    val manufacturing =
      if core.mode == "catalog" then
        catalog(core.catalogId)
          .toRight(ApiError("invalid-form", "Seleziona una lavorazione a catalogo valida.", Nil))
          .map(template => fromCatalog(template, completionDate, core.employeeId, core.taskEmployees, newId))
      else Right(fromCustom(core, snapshot.tasks, completionDate, newId))
    manufacturing.flatMap(withMaterials(_, snapshot.materials, materials))

  /** Attaches the non-blank material lines of a draft to `manufacturing`, or reports their validation messages. */
  private def withMaterials(
      manufacturing: ManufacturingDto,
      rows: List[MaterialDrafts.Row],
      context: MaterialDrafts.Context,
  ): Either[ApiError, ManufacturingDto] =
    if rows.forall(MaterialDrafts.isBlank) then Right(manufacturing)
    else
      MaterialDrafts
        .request(rows, context)
        .left
        .map { errors =>
          val subject = Some(manufacturing.code).filter(_.nonEmpty).fold("di una lavorazione")(code => s"della lavorazione $code")
          ApiError("invalid-form", s"Materiali $subject non validi.", errors)
        }
        .map(lines => manufacturing.copy(materials = Some(lines)))

  /** Resolves every manufacturing snapshot without materials context (see the overload with one). */
  def collectManufacturings(
      snapshots: List[MfgSnapshot],
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      newId: () => UUID,
  ): Either[ApiError, List[ManufacturingDto]] =
    collectManufacturings(snapshots, fallbackDeadline, catalog, MaterialDrafts.Context.empty, newId)

  /** Resolves every manufacturing snapshot, short-circuiting on the first invalid one. Order is preserved. */
  def collectManufacturings(
      snapshots: List[MfgSnapshot],
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      materials: MaterialDrafts.Context,
      newId: () => UUID,
  ): Either[ApiError, List[ManufacturingDto]] =
    snapshots.foldLeft(Right(List.empty): Either[ApiError, List[ManufacturingDto]]) { (acc, snapshot) =>
      acc.flatMap(list => manufacturingFromDraft(snapshot, fallbackDeadline, catalog, materials, newId).map(list :+ _))
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
