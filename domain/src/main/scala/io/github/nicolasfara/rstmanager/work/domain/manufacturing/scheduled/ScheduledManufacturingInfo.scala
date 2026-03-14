package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime

/** Unique identifier for a scheduled manufacturing instance. */
type ScheduledManufacturingId = UUID

object ScheduledManufacturingId:
  given CanEqual[ScheduledManufacturingId, ScheduledManufacturingId] = CanEqual.derived

/** Data shared by all scheduled manufacturing states.
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
  */
final case class ScheduledManufacturingInfo(
    id: ScheduledManufacturingId,
    code: ManufacturingCode,
    completionDate: DateTime,
    tasks: NonEmptyList[ScheduledTask],
    dependencies: ManufacturingDependencies,
)
