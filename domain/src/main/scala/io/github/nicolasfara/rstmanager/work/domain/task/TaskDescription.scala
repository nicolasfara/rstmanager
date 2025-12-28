package io.github.nicolasfara.rstmanager.work.domain.task

import cats.data.Validated
import io.github.nicolasfara.rstmanager.*

opaque type TaskDescription = String :| Not[Empty]
object TaskDescription:
  given CanEqual[TaskDescription, TaskDescription] = CanEqual.derived
  def apply(value: String): Validated[String, TaskDescription] = value.refineValidated
