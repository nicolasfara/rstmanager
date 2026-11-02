package io.github.nicolasfara.rstmanager.warehouse.domain.product

import io.github.nicolasfara.rstmanager.warehouse.domain.{ BusinessCalendar, Quantity, SignedQuantity, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.Monad
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.Decision
import io.github.iltotore.iron.:|

/** Edomata service for the event-sourced [[ProductAggregate]]. */
object ProductService extends ProductAggregate.Service[ProductService.Command, ProductService.Notification]:
  /**
   * Commands of a product. Movement commands carry the id the service generated for the movement; their `on` day defaults to the business day of the
   * command time.
   */
  enum Command derives CanEqual:
    case Create(info: ProductInfo)
    case Update(info: ProductInfo)
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
  end Command

  enum Notification derives CanEqual:
    /** The product was created, updated, or archived, or its stock changed. */
    case ProductChanged(productId: ProductId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(info) =>
      mustTargetOwnStream(info.id) >> decideOnDay(_.create(info, _)).void >> App.publish(Notification.ProductChanged(info.id))
    case Command.Update(info) =>
      mustTargetOwnStream(info.id) >> decideOnDay(_.update(info, _)).void >> App.publish(Notification.ProductChanged(info.id))
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
  }

  /** Decides on the current state for the business day of the command time, so events never read the clock. */
  private def decideOnDay[F[_]: Monad](
      decision: (ProductAggregate, LocalDate) => Decision[ProductError, ProductEvent, ProductAggregate],
  ): App[F, ProductAggregate] =
    App.read.flatMap(context => App.decide(decision(context.state, BusinessCalendar.day(context.command.time))))

  /** Publishes that the product in `state` changed. */
  private def publishChanged[F[_]: Monad](state: ProductAggregate): App[F, Unit] = state match
    case ProductAggregate.Active(product) => App.publish(Notification.ProductChanged(product.info.id))
    case ProductAggregate.Archived(product, _) => App.publish(Notification.ProductChanged(product.info.id))
    case ProductAggregate.Empty => App.unit

  /** Rejects a command whose product belongs to another stream, so the stream id and the stored product id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](productId: ProductId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == productId.toString then App.unit else App.reject(ProductError.ProductIdMismatch))
end ProductService
