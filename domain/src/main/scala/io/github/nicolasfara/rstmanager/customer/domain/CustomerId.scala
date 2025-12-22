package io.github.nicolasfara.rstmanager.customer.domain

import java.util.UUID

type CustomerId = UUID

object CustomerId:
  def apply(id: UUID): CustomerId = id
