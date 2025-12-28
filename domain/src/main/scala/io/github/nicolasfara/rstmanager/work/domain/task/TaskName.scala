package io.github.nicolasfara.rstmanager.work.domain.task

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type TaskName = String :| Not[Empty]
object TaskName:
  given CanEqual[TaskName, TaskName] = CanEqual.derived
  def apply(value: String): Validated[String, TaskName] = value.refineValidated
  def apply(value: String :| Not[Empty]): TaskName = value
