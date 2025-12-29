package io.github.nicolasfara.rstmanager.work.domain.task.schedule

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type Percentage = Double :| (GreaterEqual[0] & LessEqual[100])
object Percentage:
  given CanEqual[Percentage, Percentage] = CanEqual.derived
  def apply(value: Double): Validated[String, Percentage] = value.refineValidated
  def apply(value: Double :| (GreaterEqual[0] & LessEqual[100])): Percentage = value

  extension (value: Double)
    def toPercentage(max: Double): Percentage =
      if value <= 0 then Percentage(0)
      else Percentage(value / max * 100).valueOr(_ => Percentage(0))

  extension (value: Int)
    def toPercentage(max: Int): Percentage =
      if value <= 0 then Percentage(0)
      else Percentage(value / max * 100).valueOr(_ => Percentage(0))