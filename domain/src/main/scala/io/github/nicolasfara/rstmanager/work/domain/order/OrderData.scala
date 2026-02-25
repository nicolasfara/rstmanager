package io.github.nicolasfara.rstmanager.work.domain.order

import cats.data.NonEmptyList
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import com.github.nscala_time.time.Imports.*
import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ScheduledManufacturing, ScheduledManufacturingId}
import java.util.UUID

type OrderId = UUID
type OrderNumber = DescribedAs[Not[Empty], "The order number cannot be empty"]

final case class OrderData(
    id: OrderId,
    number: String :| OrderNumber,
    customerId: CustomerId,
    creationDate: DateTime,
    deliveryDate: DateTime,
    priority: OrderPriority,
    setOfManufacturing: NonEmptyList[ScheduledManufacturing]
):
  def addManufacturing(manufacturing: ScheduledManufacturing): OrderData =
    copy(setOfManufacturing = setOfManufacturing.append(manufacturing))

  def removeManufacturing(manufacturingId: ScheduledManufacturingId): OrderData = ???
    // setOfManufacturing.filterNot(_.info.id == manufacturingId) match
    //   case Nil          => this
    //   case head :: tail => copy(setOfManufacturing = NonEmptyList(head, tail))
