package io.github.nicolasfara.rstmanager.work.domain.order

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.*
import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.ScheduledManufacturing

final case class OrderData(
    id: OrderId,
    number: OrderNumber,
    customerId: CustomerId,
    creationDate: DateTime,
    deliveryDate: DateTime,
    priority: OrderPriority,
    setOfManufacturing: NonEmptyList[ScheduledManufacturing]
)
