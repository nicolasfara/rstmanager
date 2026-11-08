package io.github.nicolasfara.rstmanager.warehouse.domain

import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.{ DeliveryNoteAggregate, DeliveryNoteId, DeliveryNoteInfo }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{
  AppliedDeliveryNote,
  DeliveryNotePurchaseLine,
  DeliveryNoteRef,
  MaterialId,
  MaterialRecord,
  MaterialService,
}

/** What a delivery note wants the materials to hold: its reference and the purchase lines of each material (none once it is cancelled). */
final case class DesiredDeliveryNote(ref: DeliveryNoteRef, lines: Map[MaterialId, List[DeliveryNotePurchaseLine]]) derives CanEqual

/**
 * Pure reconciliation of the purchases of the materials with the delivery notes: every line of a registered note is a purchase of its material on the
 * day of the note; a material removed from a note, or every material of a cancelled note, holds nothing for it.
 *
 * The plan compares what the note wants now ([[desired]]) with what the materials hold ([[applied]]) and yields the absolute
 * [[MaterialService.Command.SetDeliveryNotePurchases]] commands that close the gap. Applying the plan and planning again yields nothing, so the
 * reconciliation can run after every note command, at startup, and once a day without counting a purchase twice. A note that cannot be loaded is not
 * reconciled at all.
 */
object DeliveryNoteReconciliation:
  /** What `note` wants: nothing to say for a note that does not exist, no purchase for a cancelled one. */
  def desired(note: DeliveryNoteAggregate): Option[DesiredDeliveryNote] = note match
    case DeliveryNoteAggregate.Registered(record) => Some(DesiredDeliveryNote(record.info.ref, purchaseLines(record.info)))
    case DeliveryNoteAggregate.Cancelled(record, _, _) => Some(DesiredDeliveryNote(record.info.ref, Map.empty))
    case DeliveryNoteAggregate.Empty => None

  /** The purchase lines of each material of `info`, in the order of the note, dated on the day of the note and bought from its supplier. */
  def purchaseLines(info: DeliveryNoteInfo): Map[MaterialId, List[DeliveryNotePurchaseLine]] =
    info.lines.groupMap(_.materialId)(line => DeliveryNotePurchaseLine(line.mpnCode, info.supplierId, line.quantity, line.unitCost, info.date))

  /** What the materials currently hold for note `noteId` (materials holding nothing for it are not listed). */
  def applied(noteId: DeliveryNoteId, materials: List[MaterialRecord]): Map[MaterialId, AppliedDeliveryNote] =
    materials.flatMap(material => material.ledger.deliveryNotes.get(noteId).map(material.info.id -> _)).toMap

  /**
   * The commands that make `applied` match `desired`, one per material whose purchases (or the number they are recorded under) differ: the lines the
   * note wants, or an empty list for a material the note no longer names. Sorted by material, so the plan is deterministic.
   */
  def plan(
      desired: DesiredDeliveryNote,
      applied: Map[MaterialId, AppliedDeliveryNote],
  ): List[(MaterialId, MaterialService.Command.SetDeliveryNotePurchases)] =
    (desired.lines.keySet ++ applied.keySet).toList
      .sortBy(_.value.toString)
      .flatMap { materialId =>
        val target = desired.lines.getOrElse(materialId, Nil)
        Option.unless(AppliedDeliveryNote.holds(applied.get(materialId), desired.ref, target.map(_.priced)))(
          materialId -> MaterialService.Command.SetDeliveryNotePurchases(desired.ref, target),
        )
      }

  /** The materials of the note whose purchases are not yet what the note wants. */
  def pending(desired: DesiredDeliveryNote, applied: Map[MaterialId, AppliedDeliveryNote]): Set[MaterialId] = plan(desired, applied).map(_._1).toSet
end DeliveryNoteReconciliation
