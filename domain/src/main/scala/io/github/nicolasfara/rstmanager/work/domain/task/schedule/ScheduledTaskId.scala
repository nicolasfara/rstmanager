package io.github.nicolasfara.rstmanager.work.domain.task.schedule

import cats.kernel.Eq

import java.util.UUID

opaque type ScheduledTaskId = UUID

object ScheduledTaskId:
  given CanEqual[ScheduledTaskId, ScheduledTaskId] = CanEqual.derived
  given Eq[ScheduledTaskId] = Eq.fromUniversalEquals
  def apply(value: UUID): ScheduledTaskId = value
