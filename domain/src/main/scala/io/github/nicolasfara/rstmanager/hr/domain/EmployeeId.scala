package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

/** Unique identifier of an employee: a distinct type, so it cannot be mixed up with the other identifiers. */
opaque type EmployeeId = UUID

object EmployeeId:
  /** Wraps an existing UUID. */
  def apply(value: UUID): EmployeeId = value

  extension (id: EmployeeId)
    /** The underlying UUID, for persistence and the API. */
    def value: UUID = id

  given CanEqual[EmployeeId, EmployeeId] = CanEqual.derived
