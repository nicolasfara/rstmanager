package io.github.nicolasfara.rstmanager.work.domain.order

import java.util.UUID

opaque type OrderId = UUID
object OrderId:
  given CanEqual[OrderId, OrderId] = CanEqual.derived
  def apply(value: UUID): OrderId = value
