package io.github.nicolasfara.rstmanager.warehouse.domain.material

import java.util.UUID

/**
 * Unique identifier of a stock movement within a material's ledger. The service generates it and passes it in the command, so the domain never draws
 * random numbers.
 */
opaque type MovementId = UUID

object MovementId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): MovementId = value

  extension (id: MovementId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[MovementId, MovementId] = CanEqual.derived
