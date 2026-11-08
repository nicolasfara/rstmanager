package io.github.nicolasfara.rstmanager.warehouse.domain

import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialId, MaterialRecord, MovementKind, StockMovement, StockSnapshot }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import com.github.nscala_time.time.Imports.LocalDate
import org.joda.time.DateTimeConstants

/** A material as the reports see it: its folded record and the day it was archived, if it was. */
final case class ReportedMaterial(record: MaterialRecord, archivedOn: Option[LocalDate]) derives CanEqual:
  /** Whether the material was already archived at the end of `day`. */
  def archivedAt(day: LocalDate): Boolean = archivedOn.exists(!_.isAfter(day))

/**
 * A material in the warehouse state of a day.
 *
 * @param customerPrice
 *   Customer price in effect on the day.
 * @param archived
 *   Whether the material was archived on or before the day (it is listed only while it still holds stock).
 */
final case class StateRow(material: ReportedMaterial, customerPrice: Option[UnitPrice], stock: StockSnapshot, archived: Boolean) derives CanEqual

/**
 * Totals of a warehouse state.
 *
 * @param value
 *   Sum of the material values (a material without any cost counts zero).
 * @param purchasedTotal
 *   Sum of what was spent on purchases of the listed materials up to the day.
 * @param negativeMaterials
 *   Materials with a negative balance (anomalies).
 * @param unvaluedMaterials
 *   Materials with stock but no cost to value it.
 */
final case class StateTotals(materials: Int, value: Amount, purchasedTotal: Amount, negativeMaterials: Int, unvaluedMaterials: Int) derives CanEqual

/** The warehouse as it stood at the end of `at`. */
final case class WarehouseState(at: LocalDate, rows: List[StateRow], totals: StateTotals) derives CanEqual

/** Length of the buckets of a flow report. */
enum FlowGranularity(val wireName: String) derives CanEqual:
  /** ISO weeks, from Monday to Sunday. */
  case Week extends FlowGranularity("week")

  /** Calendar months. */
  case Month extends FlowGranularity("month")

  /** The first day of the bucket holding `day`. */
  def start(day: LocalDate): LocalDate = this match
    case Week => day.withDayOfWeek(DateTimeConstants.MONDAY).nn
    case Month => day.withDayOfMonth(1).nn

  /** The first day of the bucket after the one starting on `start`. */
  def next(start: LocalDate): LocalDate = this match
    case Week => start.plusWeeks(1).nn
    case Month => start.plusMonths(1).nn

  /** The name of the bucket starting on `start`: `2026-W53` (ISO week-year and week) or `2026-12`. */
  def label(start: LocalDate): String = this match
    case Week => f"${start.getWeekyear}%04d-W${start.getWeekOfWeekyear}%02d"
    case Month => f"${start.getYear}%04d-${start.getMonthOfYear}%02d"
end FlowGranularity

object FlowGranularity:
  /** Reads a granularity from its [[FlowGranularity.wireName]]. */
  def fromWireName(raw: String): Either[String, FlowGranularity] =
    val normalized = raw.trim.nn
    values.find(_.wireName == normalized).toRight(s"La granularità '$raw' non è supportata. Usa ${values.map(_.wireName).mkString(", ")}.")

/**
 * Quantities that came in and went out in one unit of measure, summed over every material in that unit. `quantityIn` is never negative; `quantityOut`
 * is reduced by consumption reversals (storni), so it is negative in a period whose reversals exceed what went out.
 */
final case class UnitFlow(unit: UnitOfMeasure, quantityIn: SignedQuantity, quantityOut: SignedQuantity) derives CanEqual:
  def net: SignedQuantity = SignedQuantity.saturating(quantityIn.value - quantityOut.value)

/**
 * The flows of a period.
 *
 * @param inValue
 *   Incoming value: purchases at their cost plus incoming adjustments at the cost they record (an incoming adjustment without cost adds no value).
 * @param outValue
 *   Outgoing value: every outgoing movement at the highest cost of its day, less the consumption reversals (storni) at the highest cost of theirs;
 *   negative only in a period whose reversals exceed what went out.
 * @param purchaseValue
 *   The part of `inValue` bought from suppliers.
 * @param quantities
 *   Quantities per unit of measure with any movement, in unit order.
 */
final case class FlowTotals(inValue: Amount, outValue: Amount, purchaseValue: Amount, quantities: List[UnitFlow]) derives CanEqual:
  def net: Amount = inValue.minus(outValue)

/**
 * One bucket of a flow report.
 *
 * @param start
 *   First day of the week or month.
 * @param from
 *   First day counted: `start`, or the first day of the report.
 * @param to
 *   Last day counted: the end of the week or month, or the last day of the report.
 */
final case class FlowBucket(start: LocalDate, label: String, from: LocalDate, to: LocalDate, totals: FlowTotals) derives CanEqual

/** Incoming and outgoing stock from `from` to `to` (inclusive), in buckets, voided movements excluded. */
final case class FlowReport(from: LocalDate, to: LocalDate, granularity: FlowGranularity, buckets: List[FlowBucket], totals: FlowTotals)
    derives CanEqual

/** What was bought under one MPN of one material. */
final case class MpnSpend(materialId: MaterialId, mpnCode: String, unit: UnitOfMeasure, quantity: SignedQuantity, amount: Amount, purchases: Int)
    derives CanEqual

/** What was bought from one supplier, with the breakdown per material MPN (largest amount first). */
final case class SupplierSpend(supplierId: SupplierId, amount: Amount, purchases: Int, mpns: List[MpnSpend]) derives CanEqual

/** What was bought from each supplier from `from` to `to` (inclusive), largest amount first. */
final case class SupplierSpendReport(from: LocalDate, to: LocalDate, suppliers: List[SupplierSpend], total: Amount) derives CanEqual

/**
 * Pure reports over the warehouse materials: the state of any past day, the incoming and outgoing flows of a period, and the spend per supplier.
 *
 * Every figure comes from the materials' ledgers, as days: a movement on `D` counts for `D`. Voided movements count for no day.
 */
object WarehouseReports:
  /**
   * The warehouse at the end of `day`: every material created on or before it (or with a movement by then) and not archived before it, plus the
   * archived ones still holding stock then, by internal code.
   */
  def stateAt(materials: List[ReportedMaterial], day: LocalDate): WarehouseState =
    val rows = materials
      .filter(_.record.existsOn(day))
      .map(material => StateRow(material, material.record.customerPriceAt(day), material.record.ledger.snapshotAt(day), material.archivedAt(day)))
      .filter(row => !row.material.archivedOn.exists(_.isBefore(day)) || row.stock.quantity.value != 0L)
      .sortBy(row => (row.material.record.info.internalCode: String))
    val totals = StateTotals(
      rows.size,
      Amount.sum(rows.flatMap(_.stock.value)),
      Amount.sum(rows.map(_.stock.purchasedTotal)),
      rows.count(_.stock.negative),
      rows.count(row => row.stock.value.isEmpty && row.stock.quantity.value != 0L),
    )
    WarehouseState(day, rows, totals)

  /** The flows from `from` to `to` (inclusive), in the weeks or months covering the period; empty when `from` is after `to`. */
  def flows(materials: List[ReportedMaterial], from: LocalDate, to: LocalDate, granularity: FlowGranularity): FlowReport =
    val items = flowItems(materials, from, to)
    val buckets =
      if from.isAfter(to) then Nil
      else
        Iterator
          .iterate(granularity.start(from))(granularity.next)
          .takeWhile(!_.isAfter(to))
          .map { start =>
            val bucketFrom = if start.isBefore(from) then from else start
            val end = granularity.next(start).minusDays(1).nn
            val bucketTo = if end.isAfter(to) then to else end
            FlowBucket(
              start,
              granularity.label(start),
              bucketFrom,
              bucketTo,
              totalsOf(items.filter(item => !item.on.isBefore(bucketFrom) && !item.on.isAfter(bucketTo))),
            )
          }
          .toList
    FlowReport(from, to, granularity, buckets, totalsOf(items))
  end flows

  /** The purchases from `from` to `to` (inclusive), by supplier and then by material MPN. */
  def supplierSpend(materials: List[ReportedMaterial], from: LocalDate, to: LocalDate): SupplierSpendReport =
    val purchases =
      for
        material <- materials
        movement <- material.record.ledger.active.toList
        if !movement.on.isBefore(from) && !movement.on.isAfter(to)
        purchase <- movement.kind match
          case purchase: MovementKind.Purchase => List(purchase)
          case MovementKind.Unload(_) | MovementKind.Adjustment(_, _) | MovementKind.Consumption(_) => Nil
      yield (material, movement, purchase)
    val suppliers = purchases
      .groupBy(_._3.supplierId)
      .toList
      .map { (supplierId, bought) =>
        val mpns = bought
          .groupBy((material, _, purchase) => (material.record.info.id, purchase.mpnCode: String))
          .toList
          .map { case ((materialId, mpnCode), lines) =>
            MpnSpend(
              materialId,
              mpnCode,
              lines.headOption.fold(UnitOfMeasure.Pieces)(_._1.record.info.unit),
              SignedQuantity.saturating(lines.map(_._2.delta.value: Long).sum),
              Amount.sum(lines.map(_._3.totalCost)),
              lines.size,
            )
          }
          .sortBy(mpn => (-mpn.amount.value, mpn.mpnCode, mpn.materialId.value.toString))
        SupplierSpend(supplierId, Amount.sum(mpns.map(_.amount)), bought.size, mpns)
      }
      .sortBy(supplier => (-supplier.amount.value, supplier.supplierId.value.toString))
    SupplierSpendReport(from, to, suppliers, Amount.sum(suppliers.map(_.amount)))
  end supplierSpend

  /** A counted movement: its day, unit, delta, signed value (if it has one), whether it is an outflow, and whether it is a purchase. */
  private final case class FlowItem(
      on: LocalDate,
      unit: UnitOfMeasure,
      delta: SignedQuantity,
      value: Option[Amount],
      outflow: Boolean,
      purchase: Boolean,
  )

  private def flowItems(materials: List[ReportedMaterial], from: LocalDate, to: LocalDate): List[FlowItem] =
    for
      material <- materials
      ledger = material.record.ledger
      movement <- ledger.active.toList
      if !movement.on.isBefore(from) && !movement.on.isAfter(to)
    yield FlowItem(movement.on, material.record.info.unit, movement.delta, ledger.valueOf(movement), movement.outflow, isPurchase(movement))

  private def isPurchase(movement: StockMovement): Boolean = movement.kind match
    case _: MovementKind.Purchase => true
    case MovementKind.Unload(_) | MovementKind.Adjustment(_, _) | MovementKind.Consumption(_) => false

  /** Consumption reversals count among the outflows, with their positive delta and value: they reduce what went out. */
  private def totalsOf(items: List[FlowItem]): FlowTotals =
    val (outgoing, incoming) = items.partition(_.outflow)
    val quantities = UnitOfMeasure.values.toList.flatMap { unit =>
      val in = incoming.filter(_.unit == unit).map(_.delta.value: Long).sum
      val out = outgoing.filter(_.unit == unit).map(item => -(item.delta.value: Long)).sum
      if in == 0L && out == 0L then None else Some(UnitFlow(unit, SignedQuantity.saturating(in), SignedQuantity.saturating(out)))
    }
    FlowTotals(
      Amount.sum(incoming.flatMap(_.value)),
      Amount.sum(outgoing.flatMap(_.value)).negate,
      Amount.sum(incoming.filter(_.purchase).flatMap(_.value)),
      quantities,
    )
end WarehouseReports
