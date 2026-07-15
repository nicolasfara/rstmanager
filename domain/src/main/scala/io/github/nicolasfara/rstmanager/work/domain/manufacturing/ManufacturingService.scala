package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import cats.Monad

/** Edomata service for the event-sourced manufacturing catalog aggregate. */
object ManufacturingService extends ManufacturingAggregate.Service[ManufacturingService.Command, ManufacturingService.Notification]:
  enum Command derives CanEqual:
    case Create(manufacturing: Manufacturing)
    case Update(manufacturing: Manufacturing)
    case Delete

  enum Notification derives CanEqual:
    /** The manufacturing template changed. */
    case ManufacturingChanged(manufacturingId: ManufacturingId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(manufacturing) =>
      App.state.decide(_.create(manufacturing)).void >> App.publish(Notification.ManufacturingChanged(manufacturing.id))
    case Command.Update(manufacturing) =>
      App.state.decide(_.update(manufacturing)).void >> App.publish(Notification.ManufacturingChanged(manufacturing.id))
    case Command.Delete =>
      App.state.decide(_.delete).void
  }
end ManufacturingService
