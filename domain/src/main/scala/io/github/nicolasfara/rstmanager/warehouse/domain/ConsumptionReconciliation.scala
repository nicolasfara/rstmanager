package io.github.nicolasfara.rstmanager.warehouse.domain

import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ ConsumptionRef, MaterialId, MaterialRecord, MaterialService }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing.CompletedManufacturing
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }

import com.github.nscala_time.time.Imports.LocalDate

/** What a completed order manufacturing should have consumed of a material: `quantity`, from the business day `on` it was completed. */
final case class DesiredConsumption(quantity: Quantity, on: LocalDate) derives CanEqual

/**
 * Pure reconciliation of the warehouse consumptions with the orders: the materials of every completed manufacturing of an order are consumed from the
 * day it was completed; when a manufacturing is reopened or removed (or loses a material) its consumption is reversed.
 *
 * The plan compares what the order wants now ([[desired]]) with what the materials hold ([[applied]]) and yields the absolute
 * [[MaterialService.Command.SetConsumption]] commands that close the gap. Applying the plan and planning again yields nothing, so the reconciliation
 * can run after every order notification and once a day without double counting.
 *
 * Only the current state of the order matters: a cancelled (or delivered) order keeps the consumptions of the manufacturings it completed, since
 * cancelling does not reopen them. An order that cannot be loaded is not reconciled at all: its consumptions are never reversed for lack of an order.
 */
object ConsumptionReconciliation:
  /** A consumption is identified by the material it takes from and the order manufacturing that uses it. */
  type Key = (MaterialId, ConsumptionRef)

  /**
   * What `order` (stored under `orderId`) should have consumed: the materials of each completed manufacturing, dated on the business day of its
   * completion. Repeated lines of one material in one manufacturing (the decisions forbid them) are summed.
   */
  def desired(orderId: OrderId, order: Order): Map[Key, DesiredConsumption] =
    order.orderData.fold(Map.empty[Key, DesiredConsumption]) { data =>
      data.setOfManufacturing.toList.collect { case completed: CompletedManufacturing => completed }.flatMap { completed =>
        val ref = ConsumptionRef(orderId, completed.info.id)
        val on = BusinessCalendar.day(completed.completedAt)
        completed.info.materials
          .groupMapReduce(_.materialId)(material => material.quantity.value: Long)(_ + _)
          .toList
          .collect { case (materialId, total) if total > 0L => (materialId, ref) -> DesiredConsumption(clamped(total), on) }
      }.toMap
    }

  /** What the materials currently consume for the manufacturings of `orderId` (zero consumptions are not listed). */
  def applied(orderId: OrderId, materials: List[MaterialRecord]): Map[Key, Quantity] =
    materials.flatMap { material =>
      material.ledger.consumptions.toList.collect {
        case (ref, quantity) if ref.orderId == orderId && quantity.value != 0L => (material.info.id, ref) -> quantity
      }
    }.toMap

  /**
   * The commands that make `applied` match `desired`, one per key whose quantity differs: a desired consumption at its quantity and day, a
   * consumption no longer desired back to zero on `today` (its reversal). Sorted by material, then order manufacturing, so the plan is deterministic.
   */
  def plan(
      desired: Map[Key, DesiredConsumption],
      applied: Map[Key, Quantity],
      today: LocalDate,
  ): List[(MaterialId, MaterialService.Command.SetConsumption)] =
    (desired.keySet ++ applied.keySet).toList
      .sortBy((materialId, ref) => (materialId.value.toString, ref.orderId.value.toString, ref.manufacturingId.value.toString))
      .flatMap { key =>
        val (materialId, ref) = key
        val current = applied.getOrElse(key, Quantity.zero)
        val target = desired.getOrElse(key, DesiredConsumption(Quantity.zero, today))
        Option.when(target.quantity != current)(materialId -> MaterialService.Command.SetConsumption(ref, target.quantity, target.on))
      }

  /** A sum of quantities clamped into the [[Quantity]] range. */
  private def clamped(total: Long): Quantity = Quantity.option(total).getOrElse(Quantity.applyUnsafe(1000000000000L))
end ConsumptionReconciliation
