package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.Validated
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.cats.*

opaque type WeeklyHours = Int :| (GreaterEqual[0] & LessEqual[168])

object WeeklyHours:
  given CanEqual[WeeklyHours, WeeklyHours] = CanEqual.derived
  def apply(value: Int): Validated[String, WeeklyHours] = value.refineValidated
  def apply(value: Int :| (GreaterEqual[0] & LessEqual[168])): WeeklyHours = value

  extension (wh: WeeklyHours) def value: Int = wh
