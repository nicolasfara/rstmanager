package io.github.nicolasfara.rstmanager.warehouse.domain.material

import io.github.nicolasfara.rstmanager.warehouse.domain.{ BusinessCalendar, Quantity, SignedQuantity, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.Monad
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.Decision
import io.github.iltotore.iron.:|

/** Edomata service for the event-sourced [[MaterialAggregate]]. */
object MaterialService extends MaterialAggregate.Service[MaterialService.Command, MaterialService.Notification]:
  /**
   * Commands of a material. Movement commands carry the id the service generated for the movement; their `on` day defaults to the business day of the
   * command time.
   */
  enum Command derives CanEqual:
    case Create(info: MaterialInfo)
    case Update(info: MaterialInfo)
    case Archive
    case RecordPurchase(
        movementId: MovementId,
        mpnCode: String :| MpnCode,
        supplierId: SupplierId,
        quantity: Quantity,
        unitCost: UnitPrice,
        note: Option[String],
        on: Option[LocalDate],
    )
    case Unload(movementId: MovementId, quantity: Quantity, reason: String :| MovementReason, on: Option[LocalDate])
    case Adjust(movementId: MovementId, delta: SignedQuantity, unitCost: Option[UnitPrice], reason: String :| MovementReason, on: Option[LocalDate])
    case VoidMovement(movementId: MovementId, reason: String :| MovementReason)

    /**
     * Sets the quantity the order manufacturing `ref` consumes to `quantity`, from day `on` (absolute: nothing happens when it already is that
     * quantity). Sent by the warehouse reconciliation with the orders, never by hand.
     */
    case SetConsumption(ref: ConsumptionRef, quantity: Quantity, on: LocalDate)

    /**
     * Sets the purchases delivery note `ref` brings to the material to exactly `lines` (absolute: nothing happens when it already brings them; an
     * empty list removes the note from the material). Sent by the warehouse reconciliation with the delivery notes, never by hand.
     */
    case SetDeliveryNotePurchases(ref: DeliveryNoteRef, lines: List[DeliveryNotePurchaseLine])
  end Command

  enum Notification derives CanEqual:
    /** The material was created, updated, or archived, or its stock changed. */
    case MaterialChanged(materialId: MaterialId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(info) =>
      mustTargetOwnStream(info.id) >> decideOnDay(_.create(info, _)).void >> App.publish(Notification.MaterialChanged(info.id))
    case Command.Update(info) =>
      mustTargetOwnStream(info.id) >> decideOnDay(_.update(info, _)).void >> App.publish(Notification.MaterialChanged(info.id))
    case Command.Archive =>
      decideOnDay(_.archive(_)).flatMap(publishChanged)
    case Command.RecordPurchase(movementId, mpnCode, supplierId, quantity, unitCost, note, on) =>
      decideOnDay((state, today) => state.recordPurchase(movementId, mpnCode, supplierId, quantity, unitCost, note, on.getOrElse(today), today))
        .flatMap(publishChanged)
    case Command.Unload(movementId, quantity, reason, on) =>
      decideOnDay((state, today) => state.unload(movementId, quantity, reason, on.getOrElse(today), today)).flatMap(publishChanged)
    case Command.Adjust(movementId, delta, unitCost, reason, on) =>
      decideOnDay((state, today) => state.adjust(movementId, delta, unitCost, reason, on.getOrElse(today), today)).flatMap(publishChanged)
    case Command.VoidMovement(movementId, reason) =>
      decideOnDay(_.voidMovement(movementId, reason, _)).flatMap(publishChanged)
    case Command.SetConsumption(ref, quantity, on) =>
      App.state.flatMap(before =>
        decideOnDay(_.setConsumption(ref, quantity, on, _)).flatMap(after => if after == before then App.unit else publishChanged(after)),
      )
    case Command.SetDeliveryNotePurchases(ref, lines) =>
      App.state.flatMap(before =>
        decideOnDay(_.setDeliveryNotePurchases(ref, lines, _)).flatMap(after => if after == before then App.unit else publishChanged(after)),
      )
  }

  /** Decides on the current state for the business day of the command time, so events never read the clock. */
  private def decideOnDay[F[_]: Monad](
      decision: (MaterialAggregate, LocalDate) => Decision[MaterialError, MaterialEvent, MaterialAggregate],
  ): App[F, MaterialAggregate] =
    App.read.flatMap(context => App.decide(decision(context.state, BusinessCalendar.day(context.command.time))))

  /** Publishes that the material in `state` changed. */
  private def publishChanged[F[_]: Monad](state: MaterialAggregate): App[F, Unit] = state match
    case MaterialAggregate.Active(material) => App.publish(Notification.MaterialChanged(material.info.id))
    case MaterialAggregate.Archived(material, _) => App.publish(Notification.MaterialChanged(material.info.id))
    case MaterialAggregate.Empty => App.unit

  /** Rejects a command whose material belongs to another stream, so the stream id and the stored material id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](materialId: MaterialId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == materialId.toString then App.unit else App.reject(MaterialError.MaterialIdMismatch))
end MaterialService
