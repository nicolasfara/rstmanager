package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.warehouse.domain.Amount
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*

/** Unique identifier of a manufacturing scheduled inside an order: a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type ScheduledManufacturingId = UUID

object ScheduledManufacturingId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): ScheduledManufacturingId = value

  extension (id: ScheduledManufacturingId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[ScheduledManufacturingId, ScheduledManufacturingId] = CanEqual.derived

/**
 * Data shared by all scheduled manufacturing states.
 *
 * @param id
 *   Scheduled manufacturing identifier.
 * @param code
 *   Reference to the manufacturing template code.
 * @param completionDate
 *   Target completion date for the manufacturing.
 * @param tasks
 *   Scheduled tasks that make up the manufacturing.
 * @param dependencies
 *   Dependency graph between scheduled tasks.
 * @param description
 *   Optional free-text description of the manufacturing.
 * @param preferredEmployeeId
 *   Manufacturing-wide preferred employee, used by the planner when a task has no dedicated preference.
 * @param taskPreferredEmployees
 *   Preferred employee for individual scheduled tasks (keyed by scheduled task instance id); takes precedence over `preferredEmployeeId`.
 * @param materials
 *   Warehouse materials the manufacturing uses, at most one line per material, each with its frozen customer price.
 */
final case class ScheduledManufacturingInfo(
    id: ScheduledManufacturingId,
    code: String :| ManufacturingCode,
    completionDate: DateTime,
    tasks: NonEmptyList[ScheduledTask],
    dependencies: ManufacturingDependencies,
    description: Option[String] = None,
    preferredEmployeeId: Option[EmployeeId] = None,
    taskPreferredEmployees: Map[ScheduledTaskId, EmployeeId] = Map.empty,
    materials: List[ScheduledMaterial] = Nil,
):
  /** Planner-effective preferred employee for one scheduled task: its own preference, falling back to the manufacturing-wide one. */
  def preferredEmployeeForTask(taskId: ScheduledTaskId): Option[EmployeeId] =
    taskPreferredEmployees.get(taskId).orElse(preferredEmployeeId)

  /** What the materials cost the customer: the sum of their line costs. */
  def materialsCost: Amount = Amount.sum(materials.map(_.lineCost))
