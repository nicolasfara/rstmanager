package io.github.nicolasfara.rstmanager.warehouse.domain.material

import io.github.nicolasfara.rstmanager.warehouse.domain.UnitPrice

import com.github.nscala_time.time.Imports.LocalDate

/** The customer price in effect from day `from` (inclusive) until the next point. */
final case class CustomerPricePoint(from: LocalDate, price: UnitPrice) derives CanEqual

/**
 * Folded state of an existing material: its current master data plus the history needed to answer questions about past days.
 *
 * Built only by the [[MaterialAggregate]] fold, from the business days carried in the events.
 *
 * @param info
 *   Current master data.
 * @param createdOn
 *   Business day the material was created.
 * @param customerPrices
 *   Customer price history, in chronological order of `from`, one point per day at most, never two consecutive equal prices; the first point is the
 *   creation price.
 * @param ledger
 *   Stock movements, voided ones included.
 */
final case class MaterialRecord(
    info: MaterialInfo,
    createdOn: LocalDate,
    customerPrices: List[CustomerPricePoint],
    ledger: StockLedger = StockLedger.empty,
) derives CanEqual:
  /** Whether stock movements were recorded for the material (voided ones count too): they lock its unit of measure. */
  def hasMovements: Boolean = ledger.nonEmpty

  /** The customer price in effect on `day`; before the first price point (stock recorded before creation) the first price. */
  def customerPriceAt(day: LocalDate): Option[UnitPrice] =
    customerPrices.takeWhile(!_.from.isAfter(day)).lastOption.orElse(customerPrices.headOption).map(_.price)

  /** Whether the material is part of the warehouse on `day`: created by then, or holding a movement (not voided) dated on or before it. */
  def existsOn(day: LocalDate): Boolean = !createdOn.isAfter(day) || ledger.active.exists(!_.on.isAfter(day))

  /** The record with new master data from `on`: the customer price `info` carries applies from that day on, replacing any later point. */
  def updated(info: MaterialInfo, on: LocalDate): MaterialRecord =
    val before = customerPrices.filter(_.from.isBefore(on))
    val prices =
      if before.lastOption.exists(_.price == info.customerPrice) then before else before :+ CustomerPricePoint(on, info.customerPrice)
    copy(info = info, customerPrices = prices)

  /** The record with `mpn` added to the MPNs of the material. */
  def withMpn(mpn: Mpn): MaterialRecord = copy(info = info.copy(mpns = info.mpns :+ mpn))

  /** The record with its ledger changed by `change`. */
  def withLedger(change: StockLedger => StockLedger): MaterialRecord = copy(ledger = change(ledger))
end MaterialRecord

object MaterialRecord:
  /** The record of a material just created on `on`. */
  def created(info: MaterialInfo, on: LocalDate): MaterialRecord = MaterialRecord(info, on, List(CustomerPricePoint(on, info.customerPrice)))
