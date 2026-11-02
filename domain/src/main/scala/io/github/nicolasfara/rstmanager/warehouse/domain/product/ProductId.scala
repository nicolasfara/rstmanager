package io.github.nicolasfara.rstmanager.warehouse.domain.product

import java.util.UUID

/** Unique identifier of a warehouse product: a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type ProductId = UUID

object ProductId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): ProductId = value

  extension (id: ProductId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[ProductId, ProductId] = CanEqual.derived
