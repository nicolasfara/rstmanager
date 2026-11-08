package io.github.nicolasfara.rstmanager.warehouse.domain.material

import java.nio.charset.StandardCharsets
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, BusinessCalendar, Quantity, SignedQuantity, UnitPrice, Valuation }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

/** Refined constraint for the reason of a manual stock movement or of a void. */
type MovementReason = DescribedAs[Not[Empty], "Il motivo non può essere vuoto"]

/**
 * What a stock movement is, with the data only that kind carries. The sign of the delta is kept by [[StockMovement]]: a kind only says why stock
 * changed and at which cost, if any.
 */
enum MovementKind derives CanEqual:
  /** Bought under the MPN `mpnCode` of `supplierId` at `unitCost` per unit, for `totalCost` (quantity × unit cost, rounded to the cent). */
  case Purchase(mpnCode: String :| MpnCode, supplierId: SupplierId, unitCost: UnitPrice, totalCost: Amount, note: Option[String])

  /** Taken out of stock by hand for `reason`. */
  case Unload(reason: String :| MovementReason)

  /**
   * A manual correction for `reason`. An incoming adjustment may carry the `unitCost` of the stock it adds (opening stock at go-live): it is then a
   * price point and valued at that cost, but it is not a purchase from a supplier.
   */
  case Adjustment(unitCost: Option[UnitPrice], reason: String :| MovementReason)

  /**
   * Automatic consumption by the order manufacturing `ref`: an outflow when the consumed quantity grows, its reversal (storno, a positive delta) when
   * it shrinks. Both are valued at the highest cost of their day, like every outflow, and are never a price point. Only the reconciliation with the
   * orders changes them: they cannot be voided by hand.
   */
  case Consumption(ref: ConsumptionRef)
end MovementKind

/** Why and when a movement was voided: a voided movement no longer counts for any day, as if it had never been recorded. */
final case class MovementVoid(reason: String :| MovementReason, voidedOn: LocalDate) derives CanEqual

/**
 * One movement of a material's stock.
 *
 * @param id
 *   Identifier chosen by the service.
 * @param sequence
 *   Position in the ledger (0-based, in recording order): orders the movements of the same day.
 * @param on
 *   Business day the stock changed (may be earlier than the day it was recorded).
 * @param delta
 *   Signed change of the stock, never zero.
 * @param kind
 *   What the movement is.
 * @param voiding
 *   Present once the movement was voided.
 */
final case class StockMovement(
    id: MovementId,
    sequence: Int,
    on: LocalDate,
    delta: SignedQuantity,
    kind: MovementKind,
    voiding: Option[MovementVoid],
) derives CanEqual:
  def voided: Boolean = voiding.isDefined

  def incoming: Boolean = delta.value > 0

  /** The order manufacturing of an automatic consumption (or of its reversal). */
  def consumption: Option[ConsumptionRef] = kind match
    case MovementKind.Consumption(ref) => Some(ref)
    case MovementKind.Purchase(_, _, _, _, _) | MovementKind.Unload(_) | MovementKind.Adjustment(_, _) => None

  /**
   * Whether the movement goes out of stock for the flow reports: every movement with a negative delta, and every consumption, so that a reversal
   * (storno) reduces the outflows instead of counting as an inflow.
   */
  def outflow: Boolean = !incoming || consumption.isDefined

  /**
   * Whether the movement is valued at the cost it records (an incoming purchase or adjustment) rather than at the highest cost of its day (every
   * outflow, consumption reversals included).
   */
  def valuedAtRecordedCost: Boolean = !outflow

  /** The unit cost this movement records for the material, which takes part in the highest cost of its day and later ones. */
  def pricePoint: Option[UnitPrice] = kind match
    case MovementKind.Purchase(_, _, unitCost, _, _) => Some(unitCost)
    case MovementKind.Adjustment(unitCost, _) if incoming => unitCost
    case MovementKind.Adjustment(_, _) | MovementKind.Unload(_) | MovementKind.Consumption(_) => None

  /** The value of an incoming movement at the cost it records: the purchase total, or an incoming adjustment at its unit cost. */
  def recordedValue: Option[Amount] = kind match
    case MovementKind.Purchase(_, _, _, totalCost, _) => Some(totalCost)
    case MovementKind.Adjustment(Some(unitCost), _) if incoming => Some(Valuation.amount(delta.abs, unitCost))
    case MovementKind.Adjustment(_, _) | MovementKind.Unload(_) | MovementKind.Consumption(_) => None
end StockMovement

/**
 * Stock of a material on a day.
 *
 * @param quantity
 *   Balance at the end of the day; negative only after an automatic consumption beyond the stock (an anomaly).
 * @param maxUnitCost
 *   Highest unit cost recorded on or before the day, if any.
 * @param value
 *   `quantity × maxUnitCost`, rounded to the cent; absent without a cost.
 * @param purchasedTotal
 *   What was spent on purchases of the material up to the day.
 */
final case class StockSnapshot(quantity: SignedQuantity, maxUnitCost: Option[UnitPrice], value: Option[Amount], purchasedTotal: Amount)
    derives CanEqual:
  def negative: Boolean = quantity.value < 0

object StockSnapshot:
  val empty: StockSnapshot = StockSnapshot(SignedQuantity.zero, None, None, Amount.zero)

/**
 * A movement as listed in the ledger, with the balance at the end of it and its signed value: incoming movements at the cost they record (none for an
 * incoming adjustment without cost), outgoing ones and consumption reversals at the highest cost of their day (none before any cost exists).
 */
final case class LedgerLine(movement: StockMovement, balance: SignedQuantity, value: Option[Amount]) derives CanEqual

/**
 * The stock movements of a material, in recording order, and the pure questions about its stock on any day.
 *
 * The fold only appends movements and marks them voided; every rule (non-negative stock, known MPN, dates) is checked by the decisions before the
 * events exist. Days are business days and compared as days: a movement on `D` counts for `D` and every later day. Voided movements count for no day.
 *
 * @param consumptions
 *   Quantity currently consumed by each order manufacturing (only non-zero ones): the sum of its consumption movements, kept so that an absolute
 *   [[io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent.ConsumptionSet]] can be turned into its delta.
 */
final case class StockLedger(movements: Vector[StockMovement], consumptions: Map[ConsumptionRef, Quantity] = Map.empty) derives CanEqual:
  import BusinessCalendar.given

  def isEmpty: Boolean = movements.isEmpty

  def nonEmpty: Boolean = movements.nonEmpty

  def find(id: MovementId): Option[StockMovement] = movements.find(_.id == id)

  /** The ledger with a new movement at the end. */
  def appended(id: MovementId, on: LocalDate, delta: SignedQuantity, kind: MovementKind): StockLedger =
    copy(movements = movements :+ StockMovement(id, movements.size, on, delta, kind, None))

  /** The ledger with movement `id` voided (unchanged if it does not exist or is already voided). */
  def voided(id: MovementId, voiding: MovementVoid): StockLedger =
    copy(movements = movements.map(movement => if movement.id == id && !movement.voided then movement.copy(voiding = Some(voiding)) else movement))

  /** The quantity the order manufacturing `ref` currently consumes (zero when it consumes nothing). */
  def consumedBy(ref: ConsumptionRef): Quantity = consumptions.getOrElse(ref, Quantity.zero)

  /**
   * The ledger with the consumption of `ref` set to `quantity`: a movement on `on` with delta `previous - quantity` (an outflow when the consumption
   * grows, a reversal when it shrinks), none when the quantity does not change. Its id is derived from `ref` and its position, so replaying the
   * journal gives every movement the same id again.
   */
  def consumptionSet(ref: ConsumptionRef, quantity: Quantity, on: LocalDate): StockLedger =
    val delta = SignedQuantity.saturating(consumedBy(ref).value - quantity.value)
    val updated = if quantity.value == 0L then consumptions.removed(ref) else consumptions.updated(ref, quantity)
    val moved =
      if delta.value == 0L then this else appended(StockLedger.consumptionMovementId(ref, movements.size), on, delta, MovementKind.Consumption(ref))
    moved.copy(consumptions = updated)

  /** The movements that count: not voided. */
  def active: Vector[StockMovement] = movements.filterNot(_.voided)

  /** Every movement by day, then recording order. */
  def chronological: Vector[StockMovement] = movements.sortBy(movement => (movement.on, movement.sequence))

  /** The balance at the end of `day`. */
  def balanceAt(day: LocalDate): SignedQuantity = SignedQuantity.saturating(activeUntil(day).map(_.delta.value).sum)

  /**
   * The balance at the end of `day` and of every later day a movement happened on, in chronological order: the days on which the balance from `day`
   * on can reach its extremes.
   */
  def balancesFrom(day: LocalDate): NonEmptyList[SignedQuantity] =
    val start = balanceAt(day)
    val laterDays = active.filter(_.on.isAfter(day)).groupMapReduce(_.on)(movement => movement.delta.value: Long)(_ + _).toList.sortBy(_._1)
    val later = laterDays.scanLeft(start.value: Long)((balance, change) => balance + change._2).drop(1)
    NonEmptyList(start, later.map(SignedQuantity.saturating))

  /** The lowest balance on `day` or any later day. */
  def minBalanceFrom(day: LocalDate): SignedQuantity = balancesFrom(day).toList.minBy(_.value)

  /** The highest balance on `day` or any later day. */
  def maxBalanceFrom(day: LocalDate): SignedQuantity = balancesFrom(day).toList.maxBy(_.value)

  /** The highest unit cost recorded on or before `day` (purchases and incoming adjustments with a cost). */
  def maxCostAt(day: LocalDate): Option[UnitPrice] = activeUntil(day).flatMap(_.pricePoint).maxByOption(_.value)

  /** What was spent on purchases up to `day`. */
  def purchasedTotalAt(day: LocalDate): Amount =
    Amount.sum(activeUntil(day).collect { case StockMovement(_, _, _, _, MovementKind.Purchase(_, _, _, totalCost, _), _) => totalCost })

  /** The stock on `day`. */
  def snapshotAt(day: LocalDate): StockSnapshot =
    val quantity = balanceAt(day)
    val maxCost = maxCostAt(day)
    StockSnapshot(quantity, maxCost, maxCost.map(Valuation.signedAmount(quantity, _)), purchasedTotalAt(day))

  /** The unit cost `movement` is valued at: the cost it records (see [[StockMovement.valuedAtRecordedCost]]) or the highest cost of its day. */
  def unitCostOf(movement: StockMovement): Option[UnitPrice] =
    if movement.valuedAtRecordedCost then movement.pricePoint else maxCostAt(movement.on)

  /** The signed value of `movement`, as in [[LedgerLine]]. */
  def valueOf(movement: StockMovement): Option[Amount] =
    if movement.valuedAtRecordedCost then movement.recordedValue
    else maxCostAt(movement.on).map(Valuation.signedAmount(movement.delta, _))

  /** Every movement, voided ones included, in chronological order with the balance after it (a voided movement leaves the balance unchanged). */
  def lines: List[LedgerLine] =
    chronological.toList
      .foldLeft((0L, List.empty[LedgerLine])) { case ((balance, acc), movement) =>
        val after = if movement.voided then balance else balance + movement.delta.value
        (after, LedgerLine(movement, SignedQuantity.saturating(after), valueOf(movement)) :: acc)
      }
      ._2
      .reverse

  private def activeUntil(day: LocalDate): Vector[StockMovement] = active.filterNot(_.on.isAfter(day))
end StockLedger

object StockLedger:
  val empty: StockLedger = StockLedger(Vector.empty)

  /** Largest balance a material may reach: the range of a [[io.github.nicolasfara.rstmanager.warehouse.domain.Quantity]]. */
  val maxBalance: SignedQuantity = SignedQuantity(1000000000000L)

  /**
   * Id of the consumption movement of `ref` at position `sequence`: a name-based UUID (version 3), deterministic and distinct from the random ids
   * (version 4) the service draws for manual movements.
   */
  def consumptionMovementId(ref: ConsumptionRef, sequence: Int): MovementId =
    val name = s"warehouse-consumption-${ref.orderId.value}-${ref.manufacturingId.value}-$sequence"
    MovementId(UUID.nameUUIDFromBytes(name.getBytes(StandardCharsets.UTF_8)).nn)
