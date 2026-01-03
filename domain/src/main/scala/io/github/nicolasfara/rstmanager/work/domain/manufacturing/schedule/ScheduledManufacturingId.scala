package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import java.util.UUID

opaque type ScheduledManufacturingId = UUID

object ScheduledManufacturingId:
  given CanEqual[ScheduledManufacturingId, ScheduledManufacturingId] = CanEqual.derived
  def apply(value: UUID): ScheduledManufacturingId = value
