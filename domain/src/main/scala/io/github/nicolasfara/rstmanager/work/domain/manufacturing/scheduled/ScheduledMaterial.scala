package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, UnitPrice, Valuation }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialId

/**
 * A warehouse material used by a scheduled manufacturing, charged to the customer.
 *
 * @param materialId
 *   Warehouse material consumed.
 * @param quantity
 *   Amount of the material used, in thousandths of its unit of measure; the decisions require it to be positive.
 * @param unitPrice
 *   Customer price per unit, frozen when the material was added: later changes to the material's price do not change it.
 */
final case class ScheduledMaterial(materialId: MaterialId, quantity: Quantity, unitPrice: UnitPrice) derives CanEqual:
  /** What this line costs the customer: `quantity × unitPrice`, rounded to the cent. */
  def lineCost: Amount = Valuation.amount(quantity, unitPrice)
