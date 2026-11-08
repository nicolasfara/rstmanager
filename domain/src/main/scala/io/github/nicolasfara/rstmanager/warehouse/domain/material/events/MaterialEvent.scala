package io.github.nicolasfara.rstmanager.warehouse.domain.material.events

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, SignedQuantity, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{
  ConsumptionRef,
  DeliveryNotePurchase,
  DeliveryNoteRef,
  MaterialInfo,
  MovementId,
  MovementReason,
  Mpn,
  MpnCode,
}
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.:|

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialAggregate]]. Each carries the business day it
 * happened on (from the command time, in the company time zone), which the history of the material is built from.
 *
 * The journal is an append-only schema: new cases need no version bump, a changed payload does.
 */
enum MaterialEvent derives CanEqual:
  /** The material was created on `createdOn`. */
  case MaterialCreated(info: MaterialInfo, createdOn: LocalDate)

  /** The material master data was replaced on `changedOn`. */
  case MaterialUpdated(info: MaterialInfo, changedOn: LocalDate)

  /** The material was archived on `archivedOn`; its record is kept for the history. */
  case MaterialArchived(archivedOn: LocalDate)

  /**
   * `quantity` was bought on day `on` under the MPN `mpnCode` of `supplierId` at `unitCost`, for `totalCost` (computed by the decision). The supplier
   * is copied from the MPN, so the purchase keeps it after the MPN is removed from the material.
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
   * The order manufacturing `ref` consumes `quantity` of the material from day `on`. The event is absolute: the fold turns it into a movement of
   * `previous - quantity` (an outflow, or a reversal when the quantity shrinks), so replaying it twice or after a missed one keeps the right total.
   */
  case ConsumptionSet(ref: ConsumptionRef, quantity: Quantity, on: LocalDate)

  /** The MPN `mpn` was added to the material on `on` by a delivery note line naming it, with the cost of the line as its expected unit cost. */
  case MpnAdded(mpn: Mpn, on: LocalDate)

  /**
   * The delivery note `ref` brings exactly `purchases` to the material from `changedOn` on. The event is absolute: the fold voids the movements the
   * note recorded before and records `purchases` as new ones (an empty list removes the note from the material), so a changed note is voided and
   * recorded again, and replaying the event yields the same movements.
   */
  case DeliveryNotePurchasesSet(ref: DeliveryNoteRef, purchases: List[DeliveryNotePurchase], changedOn: LocalDate)
end MaterialEvent
