package io.github.nicolasfara.rstmanager.warehouse.domain.supplier

import cats.Monad

/** Edomata service for the event-sourced [[SupplierAggregate]]. */
object SupplierService extends SupplierAggregate.Service[SupplierService.Command, SupplierService.Notification]:
  enum Command derives CanEqual:
    case Create(supplier: Supplier)
    case Update(supplier: Supplier)
    case Delete

  enum Notification derives CanEqual:
    /** The supplier record was created, updated, or deleted. */
    case SupplierChanged(supplierId: SupplierId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(supplier) =>
      mustTargetOwnStream(supplier.id) >> App.state.decide(_.create(supplier)).void >> App.publish(Notification.SupplierChanged(supplier.id))
    case Command.Update(supplier) =>
      mustTargetOwnStream(supplier.id) >> App.state.decide(_.update(supplier)).void >> App.publish(Notification.SupplierChanged(supplier.id))
    case Command.Delete =>
      App.state.decide(_.delete).flatMap {
        case SupplierAggregate.Deleted(supplier) => App.publish(Notification.SupplierChanged(supplier.id))
        case SupplierAggregate.Empty | SupplierAggregate.Active(_) => App.unit
      }
  }

  /** Rejects a command whose supplier belongs to another stream, so the stream id and the stored supplier id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](supplierId: SupplierId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == supplierId.toString then App.unit else App.reject(SupplierError.SupplierIdMismatch))
end SupplierService
