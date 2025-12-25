package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type ManufacturingName = String :| Not[Empty]
object ManufacturingName:
  given CanEqual[ManufacturingName, ManufacturingName] = CanEqual.derived
  def apply(value: String): Validated[String, ManufacturingName] = value.refineValidated
  def apply(value: String :| Not[Empty]): ManufacturingName = value
