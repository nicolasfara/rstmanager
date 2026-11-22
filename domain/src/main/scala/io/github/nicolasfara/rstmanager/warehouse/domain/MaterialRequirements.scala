package io.github.nicolasfara.rstmanager.warehouse.domain

import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialId, MaterialRecord }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingCode
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing.CompletedManufacturing
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderData, OrderId, OrderNumber, OrderPriority }

import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*

/**
 * Stock and commitment of one material.
 *
 * @param materialId
 *   The material.
 * @param stock
 *   Balance at the end of the day of the computation (may be negative: consumptions beyond the stock are an anomaly the reports flag).
 * @param committed
 *   What the open orders still need of it: the sum of the material lines of their manufacturings not completed yet.
 * @param available
 *   `stock - committed`: negative when the open orders need more than the warehouse holds.
 */
final case class MaterialRequirement(materialId: MaterialId, stock: SignedQuantity, committed: Quantity, available: SignedQuantity) derives CanEqual

/**
 * A manufacturing of an open order that the stock of a material cannot cover, once the manufacturings due before it took their part.
 *
 * @param orderNumber
 *   Number of the order, to name it.
 * @param manufacturingCode
 *   Catalog code of the manufacturing, to name it.
 * @param required
 *   What the manufacturing needs of the material.
 * @param missing
 *   The part of `required` the stock does not cover (positive, at most `required`): it has to be ordered.
 */
final case class MaterialShortage(
    orderId: OrderId,
    orderNumber: String :| OrderNumber,
    manufacturingId: ScheduledManufacturingId,
    manufacturingCode: String :| ManufacturingCode,
    materialId: MaterialId,
    required: Quantity,
    missing: Quantity,
) derives CanEqual

/** A material line of an open order naming a material that is not in the warehouse: it cannot be allocated, so it is reported apart. */
final case class UnknownMaterialDemand(
    orderId: OrderId,
    orderNumber: String :| OrderNumber,
    manufacturingId: ScheduledManufacturingId,
    manufacturingCode: String :| ManufacturingCode,
    materialId: MaterialId,
    required: Quantity,
) derives CanEqual

/**
 * What the open orders need of the warehouse.
 *
 * @param materials
 *   Stock, commitment and availability of every material of the warehouse (archived ones included), sorted by internal code, then id.
 * @param shortages
 *   The manufacturings left short, in allocation order per material, materials sorted like `materials`.
 * @param unknownMaterials
 *   Demand on materials the warehouse does not know, sorted by material, order and manufacturing.
 */
final case class Requirements(materials: List[MaterialRequirement], shortages: List[MaterialShortage], unknownMaterials: List[UnknownMaterialDemand])
    derives CanEqual

/**
 * Pure computation of the material requirements of the open orders: what is committed of each material, what is still available, and which
 * manufacturings the stock cannot cover.
 *
 * The demand is made of the material lines of the manufacturings **not completed** of the orders in progress or suspended: a completed manufacturing
 * has already been consumed from the ledger by the [[ConsumptionReconciliation]], so counting it again would subtract it twice. Completed, delivered
 * and cancelled orders need nothing more.
 *
 * The stock of a material is allocated to its demand in a deterministic order: the manufacturing's completion date first, then the urgent orders, the
 * older orders, and finally the order and manufacturing ids. A manufacturing is short of a material when the cumulated demand up to it exceeds the
 * stock: it misses `min(required, cumulated - stock)`. A negative stock leaves every manufacturing short of its whole demand.
 */
object MaterialRequirements:
  /** One material line of a manufacturing to allocate, with the keys that order the allocation. */
  private final case class Demand(
      orderId: OrderId,
      orderNumber: String :| OrderNumber,
      manufacturingId: ScheduledManufacturingId,
      manufacturingCode: String :| ManufacturingCode,
      materialId: MaterialId,
      required: Long,
      completionMillis: Long,
      urgent: Boolean,
      creationMillis: Long,
  ):
    def sortKey: (Long, Int, Long, String, String) =
      (completionMillis, if urgent then 0 else 1, creationMillis, orderId.value.toString, manufacturingId.value.toString)

  /**
   * The requirements of `orders` (each stored under its id) against `materials` (archived ones included), with the stock at the end of `today`.
   * `exclude` leaves one order out of the demand: the form editing it counts its own lines, which would otherwise be committed twice.
   */
  def compute(orders: List[(OrderId, Order)], materials: List[MaterialRecord], today: LocalDate, exclude: Option[OrderId]): Requirements =
    val known = materials.map(_.info.id).toSet
    val (demand, unknown) = orders.filterNot((id, _) => exclude.contains(id)).flatMap(demandOf).partition(line => known.contains(line.materialId))
    val byMaterial = demand.groupBy(_.materialId)
    val sorted = materials.sortBy(material => (material.info.internalCode: String, material.info.id.value.toString))
    val perMaterial = sorted.map { material =>
      val id = material.info.id
      val stock = material.ledger.balanceAt(today)
      val lines = byMaterial.getOrElse(id, Nil).sortBy(_.sortKey)
      val committed = lines.map(_.required).sum
      val requirement = MaterialRequirement(id, stock, clamped(committed), SignedQuantity.saturating(stock.value - committed))
      (requirement, shortagesOf(stock.value, lines))
    }
    Requirements(
      perMaterial.map(_._1),
      perMaterial.flatMap(_._2),
      unknown
        .sortBy(line => (line.materialId.value.toString, line.orderId.value.toString, line.manufacturingId.value.toString))
        .map(line =>
          UnknownMaterialDemand(line.orderId, line.orderNumber, line.manufacturingId, line.manufacturingCode, line.materialId, clamped(line.required)),
        ),
    )
  end compute

  /**
   * The material lines of the manufacturings not completed of an open order; repeated lines of a material (forbidden by the decisions) are summed.
   */
  private def demandOf(orderId: OrderId, order: Order): List[Demand] =
    openData(order).toList.flatMap { data =>
      data.setOfManufacturing.toList.filter {
        case _: CompletedManufacturing => false
        case _ => true
      }.flatMap { manufacturing =>
        val info = manufacturing.info
        info.materials
          .groupMapReduce(_.materialId)(line => line.quantity.value: Long)(_ + _)
          .toList
          .collect {
            case (materialId, required) if required > 0L =>
              Demand(
                orderId,
                data.number,
                info.id,
                info.code,
                materialId,
                required,
                info.completionDate.getMillis,
                data.priority == OrderPriority.Urgent,
                data.creationDate.getMillis,
              )
          }
      }
    }

  /** The data of an order still open (in progress or suspended): the only ones that still need materials. */
  private def openData(order: Order): Option[OrderData] = order match
    case Order.InProgressOrder(data, _) => Some(data)
    case Order.SuspendedOrder(data, _, _, _) => Some(data)
    case _ => None

  /** Allocates `stock` to `lines` in their order, reporting the lines the cumulated demand leaves uncovered. */
  private def shortagesOf(stock: Long, lines: List[Demand]): List[MaterialShortage] =
    lines
      .foldLeft((0L, List.empty[MaterialShortage])) { case ((cumulated, shortages), line) =>
        val total = cumulated + line.required
        val missing = (total - stock).min(line.required)
        val next =
          if missing <= 0L then shortages
          else
            MaterialShortage(
              line.orderId,
              line.orderNumber,
              line.manufacturingId,
              line.manufacturingCode,
              line.materialId,
              clamped(line.required),
              clamped(missing),
            ) :: shortages
        (total, next)
      }
      ._2
      .reverse

  /** A sum of quantities clamped into the [[Quantity]] range. */
  private def clamped(total: Long): Quantity = Quantity.option(total.max(0L)).getOrElse(Quantity.applyUnsafe(1000000000000L))
end MaterialRequirements
