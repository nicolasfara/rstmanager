package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type ManufacturingCode = String :| Not[Empty]
object ManufacturingCode:
  given CanEqual[ManufacturingCode, ManufacturingCode] = CanEqual.derived
  def apply(value: String): Validated[String, ManufacturingCode] = value.refineValidated
