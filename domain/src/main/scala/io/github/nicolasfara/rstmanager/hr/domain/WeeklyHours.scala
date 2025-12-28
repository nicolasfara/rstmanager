package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.Validated
import io.github.nicolasfara.rstmanager.*

opaque type WeeklyHours = Int :| (GreaterEqual[0] & LessEqual[168])

object WeeklyHours:
  given CanEqual[WeeklyHours, WeeklyHours] = CanEqual.derived
  def apply(value: Int): Validated[String, WeeklyHours] = value.refineValidated

  extension (wh: WeeklyHours) def value: Int = wh
