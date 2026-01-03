package io.github.nicolasfara.rstmanager.work.domain.order

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.cats.*

opaque type SuspensionReason = String :| Not[Empty]
object SuspensionReason:
  given CanEqual[SuspensionReason, SuspensionReason] = CanEqual.derived
  def apply(value: String): Validated[String, SuspensionReason] = value.refineValidated
  def apply(value: String :| Not[Empty]): SuspensionReason = value
