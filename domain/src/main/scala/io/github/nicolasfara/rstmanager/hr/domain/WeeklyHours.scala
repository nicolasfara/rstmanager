package io.github.nicolasfara.rstmanager.hr.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type WeeklyHours = WeeklyHours.T
object WeeklyHours extends RefinedType[Int, GreaterEqual[0] & LessEqual[168]]
