package io.github.nicolasfara.rstmanager.service.registry

import java.util.UUID

import cats.Monad

/** Edomata service for the event-sourced [[EntityRegistry]], reused across entity types via distinct stream namespaces. */
object EntityRegistryService extends EntityRegistry.Service[EntityRegistryService.Command, EntityRegistryService.Notification]:
  enum Command derives CanEqual:
    case Register(id: UUID)
    case Deregister(id: UUID)

  enum Notification derives CanEqual:
    case RegistryChanged

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Register(id) => App.state.decide(_.register(id)).void
    case Command.Deregister(id) => App.state.decide(_.deregister(id)).void
  }
end EntityRegistryService
