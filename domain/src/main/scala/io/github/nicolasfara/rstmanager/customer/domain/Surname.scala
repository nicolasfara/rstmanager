package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Surname = String :| Not[Empty]

object Surname:
  def apply(surname: String): Either[String, Surname] = surname.refineEither
