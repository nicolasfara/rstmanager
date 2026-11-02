package io.github.nicolasfara.rstmanager.warehouse.domain.supplier

import java.util.UUID

/** Unique identifier of a supplier: a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type SupplierId = UUID

object SupplierId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): SupplierId = value

  extension (id: SupplierId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[SupplierId, SupplierId] = CanEqual.derived
