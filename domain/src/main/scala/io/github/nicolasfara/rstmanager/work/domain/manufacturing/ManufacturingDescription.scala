package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type ManufacturingDescription = String :| Not[Empty]
object ManufacturingDescription:
  given CanEqual[ManufacturingDescription, ManufacturingDescription] = CanEqual.derived
  def apply(value: String): Validated[String, ManufacturingDescription] = value.refineValidated
  def apply(value: String :| Not[Empty]): ManufacturingDescription = value
