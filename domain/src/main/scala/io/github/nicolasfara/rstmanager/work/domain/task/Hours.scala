package io.github.nicolasfara.rstmanager.work.domain.task

import cats.{Monoid, Order}
import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*

opaque type Hours = Int :| GreaterEqual[0]
object Hours:
  given CanEqual[Hours, Hours] = CanEqual.derived
  def apply(value: Int): Validated[String, Hours] = value.refineValidated
  def apply(value: Int :| GreaterEqual[0]): Hours = value

  given Order[Hours] with
    def compare(x: Hours, y: Hours): Int = x.value.compare(y.value)

  given Monoid[Hours] with
    def empty: Hours = Hours(0).valueOr(_ => Hours(0))
    def combine(x: Hours, y: Hours): Hours = Hours(x.value + y.value).valueOr(_ => Hours(0))

  extension (h: Hours)
    def value: Int = h
    infix def +(other: Hours): Hours = Hours(h.value + other.value).valueOr(_ => Hours(0))
    infix def -(other: Hours): Hours =
      val result = h.value - other.value
      if result < 0 then Hours(0) else Hours(result).valueOr(_ => Hours(0))
