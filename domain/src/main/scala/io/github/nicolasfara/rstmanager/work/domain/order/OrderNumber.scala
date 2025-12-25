package io.github.nicolasfara.rstmanager.work.domain.order

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type OrderNumber = String :| Not[Empty]
object OrderNumber:
  given CanEqual[OrderNumber, OrderNumber] = CanEqual.derived
  def apply(value: String): Validated[String, OrderNumber] = value.refineValidated
  def apply(value: String :| Not[Empty]): OrderNumber = value
