package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Name = String :| Not[Empty]

object Name:
  def apply(value: String): Either[String, Name] = value.refineEither
