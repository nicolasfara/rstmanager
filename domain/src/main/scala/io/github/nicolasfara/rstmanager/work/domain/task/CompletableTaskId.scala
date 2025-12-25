package io.github.nicolasfara.rstmanager.work.domain.task

import java.util.UUID

opaque type CompletableTaskId = UUID
object CompletableTaskId:
  given CanEqual[CompletableTaskId, CompletableTaskId] = CanEqual.derived
  def apply(value: UUID): CompletableTaskId = value
