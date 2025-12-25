package io.github.nicolasfara.rstmanager.work.domain.task

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type TaskDescription = String :| Not[Empty]
object TaskDescription:
  given CanEqual[TaskDescription, TaskDescription] = CanEqual.derived
  def apply(value: String): Validated[String, TaskDescription] = value.refineValidated
