package io.github.nicolasfara.rstmanager.warehouse.domain.material

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, UnitPrice, Valuation }
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.{ DeliveryNoteId, DeliveryNoteNumber }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.:|

/**
 * The delivery note (DDT) the purchases of a material come from. The number is copied from the note, so the movements and the export name it without
 * reading the note; a renumbered note re-records its purchases under the new number.
 */
final case class DeliveryNoteRef(id: DeliveryNoteId, number: String :| DeliveryNoteNumber) derives CanEqual

/**
 * A purchase a delivery note brings to a material, as the command carries it: `quantity` bought on day `on` under the MPN `mpnCode` of `supplierId`
 * at `unitCost`. Its total cost is computed by the decision ([[priced]]).
 */
final case class DeliveryNotePurchaseLine(
    mpnCode: String :| MpnCode,
    supplierId: SupplierId,
    quantity: Quantity,
    unitCost: UnitPrice,
    on: LocalDate,
) derives CanEqual:
  /** The purchase with its total cost (quantity × unit cost, rounded HALF_UP to the cent). */
  def priced: DeliveryNotePurchase = DeliveryNotePurchase(mpnCode, supplierId, quantity, unitCost, Valuation.amount(quantity, unitCost), on)

/** A purchase of a delivery note as recorded on the material, with the `totalCost` computed by the decision. */
final case class DeliveryNotePurchase(
    mpnCode: String :| MpnCode,
    supplierId: SupplierId,
    quantity: Quantity,
    unitCost: UnitPrice,
    totalCost: Amount,
    on: LocalDate,
) derives CanEqual

/** The purchases delivery note `ref` currently brings to a material (never empty: a note bringing nothing is not listed). */
final case class AppliedDeliveryNote(ref: DeliveryNoteRef, purchases: List[DeliveryNotePurchase]) derives CanEqual

object AppliedDeliveryNote:
  /** Whether a material holding `current` for a note already holds exactly `purchases` under `ref`: nothing to change. */
  def holds(current: Option[AppliedDeliveryNote], ref: DeliveryNoteRef, purchases: List[DeliveryNotePurchase]): Boolean = current match
    case Some(applied) => applied == AppliedDeliveryNote(ref, purchases)
    case None => purchases.isEmpty
