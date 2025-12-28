package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

opaque type EmployeeId = UUID
object EmployeeId:
  def apply(id: UUID): EmployeeId = id
