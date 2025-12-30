package io.github.nicolasfara.rstmanager.work.domain.order

import com.github.nscala_time.time.Imports.*
import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.ScheduledManufacturingId
import org.scalactic.anyvals.NonEmptySet

/** Aggregate root for the Order */
final case class Order(
    id: OrderId,
    number: OrderNumber,
    customerId: CustomerId,
    creationDate: DateTime,
    deliveryDate: DateTime,
    priority: OrderPriority,
    setOfManufacturing: NonEmptySet[ScheduledManufacturingId]
)
