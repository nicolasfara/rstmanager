package io.github.nicolasfara.rstmanager.hr.domain.calendar

import java.util.UUID

/** Unique identifier of a company closure: a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type ClosureId = UUID

object ClosureId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): ClosureId = value

  extension (id: ClosureId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[ClosureId, ClosureId] = CanEqual.derived
