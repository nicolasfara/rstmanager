package io.github.nicolasfara.rstmanager.customer.domain

import java.util.UUID

/** Unique identifier of a customer: a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type CustomerId = UUID

object CustomerId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): CustomerId = value

  extension (id: CustomerId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[CustomerId, CustomerId] = CanEqual.derived
