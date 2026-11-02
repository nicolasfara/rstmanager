package io.github.nicolasfara.rstmanager.warehouse.domain.product.events

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, SignedQuantity, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ ConsumptionRef, MovementId, MovementReason, MpnCode, ProductInfo }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.:|

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.warehouse.domain.product.ProductAggregate]]. Each carries the business day it
 * happened on (from the command time, in the company time zone), which the history of the product is built from.
 *
 * The journal is an append-only schema: new cases need no version bump, a changed payload does.
 */
enum ProductEvent derives CanEqual:
  /** The product was created on `createdOn`. */
  case ProductCreated(info: ProductInfo, createdOn: LocalDate)

  /** The product master data was replaced on `changedOn`. */
  case ProductUpdated(info: ProductInfo, changedOn: LocalDate)

  /** The product was archived on `archivedOn`; its record is kept for the history. */
  case ProductArchived(archivedOn: LocalDate)

  /**
   * `quantity` was bought on day `on` under the MPN `mpnCode` of `supplierId` at `unitCost`, for `totalCost` (computed by the decision). The supplier
   * is copied from the MPN, so the purchase keeps it after the MPN is removed from the product.
   */
  case PurchaseRecorded(
      movementId: MovementId,
      mpnCode: String :| MpnCode,
      supplierId: SupplierId,
      quantity: Quantity,
      unitCost: UnitPrice,
      totalCost: Amount,
      on: LocalDate,
      note: Option[String],
  )

  /** `quantity` was taken out of stock by hand on day `on` for `reason`. */
  case StockUnloaded(movementId: MovementId, quantity: Quantity, reason: String :| MovementReason, on: LocalDate)

  /** The stock was corrected by `delta` on day `on` for `reason`; an incoming correction may carry the `unitCost` of the stock it adds. */
  case StockAdjusted(movementId: MovementId, delta: SignedQuantity, unitCost: Option[UnitPrice], reason: String :| MovementReason, on: LocalDate)

  /** The movement `movementId` was voided on `voidedOn` for `reason`: it no longer counts for any day. */
  case MovementVoided(movementId: MovementId, reason: String :| MovementReason, voidedOn: LocalDate)

  /**
   * The order manufacturing `ref` consumes `quantity` of the product from day `on`. The event is absolute: the fold turns it into a movement of
   * `previous - quantity` (an outflow, or a reversal when the quantity shrinks), so replaying it twice or after a missed one keeps the right total.
   */
  case ConsumptionSet(ref: ConsumptionRef, quantity: Quantity, on: LocalDate)
end ProductEvent
