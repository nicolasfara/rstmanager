package io.github.nicolasfara.rstmanager.work.domain.order

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId.given

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import monocle.syntax.all.*

type OrderId = UUID
type OrderNumber = DescribedAs[Not[Empty], "The order number cannot be empty"]

final case class OrderData(
    id: OrderId,
    number: String :| OrderNumber,
    customerId: CustomerId,
    creationDate: DateTime,
    deliveryDate: DateTime,
    priority: OrderPriority,
    setOfManufacturing: NonEmptyList[ScheduledManufacturing],
):
  def addManufacturing(manufacturing: ScheduledManufacturing): OrderData =
    this.focus(_.setOfManufacturing).modify(_.append(manufacturing))

  def removeManufacturing(manufacturingId: ScheduledManufacturingId): OrderData =
    this.focus(_.setOfManufacturing).modify { nel =>
      nel.filterNot(_.info.id == manufacturingId) match
        case Nil => nel
        case head :: tail => NonEmptyList(head, tail)
    }
