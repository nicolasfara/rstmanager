package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Email = String :| Match["^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$"]

object Email:
  def apply(email: String): Either[String, Email] = email.refineEither
