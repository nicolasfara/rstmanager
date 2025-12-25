package io.github.nicolasfara.rstmanager.work.domain.task

import java.util.UUID

opaque type TaskId = UUID
object TaskId:
  given CanEqual[TaskId, TaskId] = CanEqual.derived
  def apply(value: UUID): TaskId = value
