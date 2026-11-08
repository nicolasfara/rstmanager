package io.github.nicolasfara.rstmanager.warehouse.domain.material

import java.util.UUID

/** Unique identifier of a warehouse material: a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type MaterialId = UUID

object MaterialId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): MaterialId = value

  extension (id: MaterialId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[MaterialId, MaterialId] = CanEqual.derived
