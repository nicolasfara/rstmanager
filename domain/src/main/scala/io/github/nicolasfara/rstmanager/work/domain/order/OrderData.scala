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

/** Unique identifier for an order. */
type OrderId = UUID

/** Refined constraint for a non-empty order number. */
type OrderNumber = DescribedAs[Not[Empty], "The order number cannot be empty"]

/**
 * Shared business data carried by all active order states.
 *
 * @param id
 *   Stable order identifier.
 * @param number
 *   External order number.
 * @param customerId
 *   Customer that owns the order.
 * @param creationDate
 *   Timestamp when the order was created.
 * @param deliveryDate
 *   Expected or requested delivery date used as the baseline for delay decisions.
 * @param priority
 *   Order priority.
 * @param setOfManufacturing
 *   Non-empty list of scheduled manufacturings associated with the order.
 * @param description
 *   Optional free-text description of the order.
 */
final case class OrderData(
    id: OrderId,
    number: String :| OrderNumber,
    customerId: CustomerId,
    creationDate: DateTime,
    deliveryDate: DateTime,
    priority: OrderPriority,
    setOfManufacturing: NonEmptyList[ScheduledManufacturing],
    description: Option[String] = None,
):
  /** Sets (or clears) the order description. */
  def withDescription(newDescription: Option[String]): OrderData =
    this.focus(_.description).replace(newDescription)

  /** Appends a manufacturing to the order. */
  def addManufacturing(manufacturing: ScheduledManufacturing): OrderData =
    this.focus(_.setOfManufacturing).modify(_.append(manufacturing))

  /** Replaces the manufacturing sharing the given manufacturing's id in place, leaving the list unchanged if none matches. */
  def replaceManufacturing(manufacturing: ScheduledManufacturing): OrderData =
    this.focus(_.setOfManufacturing).modify(_.map(existing => if existing.info.id == manufacturing.info.id then manufacturing else existing))

  /** Removes a manufacturing when at least one manufacturing remains afterwards. */
  def removeManufacturing(manufacturingId: ScheduledManufacturingId): OrderData =
    this.focus(_.setOfManufacturing).modify { nel =>
      nel.filterNot(_.info.id == manufacturingId) match
        case Nil => nel
        case head :: tail => NonEmptyList(head, tail)
    }
end OrderData
