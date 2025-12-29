package io.github.nicolasfara.rstmanager.work.domain.task.schedule

import java.util.UUID

opaque type SchedulableTaskId = UUID

object SchedulableTaskId:
  given CanEqual[SchedulableTaskId, SchedulableTaskId] = CanEqual.derived
  def apply(value: UUID): SchedulableTaskId = value
