package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTask

final case class ScheduledManufacturingInfo(
    id: ScheduledManufacturingId,
    code: ManufacturingCode,
    completionDate: DateTime,
    tasks: NonEmptyList[ScheduledTask],
    dependencies: ManufacturingDependencies
)
