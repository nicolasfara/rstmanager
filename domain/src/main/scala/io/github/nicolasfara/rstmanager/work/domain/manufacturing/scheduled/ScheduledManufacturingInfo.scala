package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask
import java.util.UUID

type ScheduledManufacturingId = UUID

object ScheduledManufacturingId:
  given CanEqual[ScheduledManufacturingId, ScheduledManufacturingId] = CanEqual.derived

final case class ScheduledManufacturingInfo(
    id: ScheduledManufacturingId,
    code: ManufacturingCode,
    completionDate: DateTime,
    tasks: NonEmptyList[ScheduledTask],
    dependencies: ManufacturingDependencies
)
