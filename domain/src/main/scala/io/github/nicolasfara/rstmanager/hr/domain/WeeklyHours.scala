package io.github.nicolasfara.rstmanager.hr.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

/** Weekly budgeted hours constrained to the inclusive range `0` to `168`. */
type WeeklyHours = WeeklyHours.T

/** Refined type companion for `WeeklyHours`. */
object WeeklyHours extends RefinedType[Int, GreaterEqual[0] & LessEqual[168]]
