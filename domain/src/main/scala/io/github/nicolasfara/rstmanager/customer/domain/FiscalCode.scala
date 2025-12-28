package io.github.nicolasfara.rstmanager.customer.domain

import io.github.nicolasfara.rstmanager.*

import cats.data.Validated

type FiscalCode =
  String :| Match["[A-Z]{6}[0-9]{2}[A-Z][0-9]{2}[A-Z][0-9]{3}[A-Z]"]

object FiscalCode:
  def apply(value: String): Validated[String, FiscalCode] = refineValidated(value.toUpperCase)
