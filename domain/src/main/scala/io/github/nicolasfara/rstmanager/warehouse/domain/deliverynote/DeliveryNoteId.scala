package io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote

import java.util.UUID

/** Unique identifier of a delivery note (DDT): a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type DeliveryNoteId = UUID

object DeliveryNoteId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): DeliveryNoteId = value

  extension (id: DeliveryNoteId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[DeliveryNoteId, DeliveryNoteId] = CanEqual.derived
