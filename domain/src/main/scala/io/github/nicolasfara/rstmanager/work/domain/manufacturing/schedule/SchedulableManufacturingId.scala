package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import java.util.UUID

opaque type SchedulableManufacturingId = UUID

object SchedulableManufacturingId:
  def apply(value: UUID): SchedulableManufacturingId = value
