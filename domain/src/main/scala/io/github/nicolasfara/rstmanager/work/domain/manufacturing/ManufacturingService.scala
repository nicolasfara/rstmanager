package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import cats.Monad

/** Edomata service for the event-sourced manufacturing catalog aggregate. */
object ManufacturingService extends ManufacturingAggregate.Service[ManufacturingService.Command, ManufacturingService.Notification]:
  enum Command derives CanEqual:
    case Create(manufacturing: Manufacturing)
    case Update(manufacturing: Manufacturing)
    case Delete

  enum Notification derives CanEqual:
    /** The manufacturing template was created, updated, or deleted. */
    case ManufacturingChanged(manufacturingId: ManufacturingId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(manufacturing) =>
      mustTargetOwnStream(manufacturing.id) >> App.state.decide(_.create(manufacturing)).void >> App.publish(
        Notification.ManufacturingChanged(manufacturing.id),
      )
    case Command.Update(manufacturing) =>
      mustTargetOwnStream(manufacturing.id) >> App.state.decide(_.update(manufacturing)).void >> App.publish(
        Notification.ManufacturingChanged(manufacturing.id),
      )
    case Command.Delete =>
      App.state.decide(_.delete).flatMap {
        case ManufacturingAggregate.Deleted(manufacturing) => App.publish(Notification.ManufacturingChanged(manufacturing.id))
        case ManufacturingAggregate.Empty | ManufacturingAggregate.Active(_) => App.unit
      }
  }

  /** Rejects a command whose manufacturing belongs to another stream, so the stream id and the stored manufacturing id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](id: ManufacturingId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == id.toString then App.unit else App.reject(ManufacturingError.ManufacturingIdMismatch))
end ManufacturingService
