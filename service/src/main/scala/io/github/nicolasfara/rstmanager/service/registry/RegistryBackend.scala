package io.github.nicolasfara.rstmanager.service.registry

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.RegistryCodecs.given

import cats.effect.{ IO, Resource }
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import skunk.Session

/**
 * Durable index backend for one entity type.
 *
 * The registry is a singleton stream (address [[RegistryBackend.address]]) inside its own journal namespace, so different entity types keep
 * independent indexes while sharing the same [[EntityRegistry]] model.
 */
object RegistryBackend:
  given BackendCodec[EntityRegistryEvent] = CirceCodec.jsonb
  given BackendCodec[EntityRegistryService.Notification] = CirceCodec.jsonb

  type Backend = EventSourcedBackend[IO, EntityRegistry, EntityRegistryEvent, EntityRegistryError, EntityRegistryService.Notification]

  /** Singleton stream id holding the id set for one entity type. */
  val address: String = "registry"

  /** Builds a registry backend on the given journal `namespace` (must be a string literal, required by `SkunkDriver`). */
  inline def build(inline namespace: String, pool: Resource[IO, Session[IO]]): Resource[IO, Backend] =
    Backend
      .builder(EntityRegistryService)
      .use(SkunkDriver(namespace, pool))
      .inMemSnapshot(200)
      .withRetryConfig(retryInitialDelay = 2.seconds)
      .build

  /** Records that `id` exists (idempotent: a duplicate registration is ignored). */
  def register(backend: Backend, id: UUID): IO[Unit] = dispatch(backend, EntityRegistryService.Command.Register(id))

  /** Records that `id` no longer exists (idempotent: deregistering an unknown id is ignored). */
  def deregister(backend: Backend, id: UUID): IO[Unit] = dispatch(backend, EntityRegistryService.Command.Deregister(id))

  /** Reads the current set of registered ids. */
  def ids(backend: Backend): IO[Set[UUID]] =
    backend.repository.get(address).map {
      case AggregateState.Valid(registry, _) => registry.ids
      case _ => Set.empty
    }

  private def dispatch(backend: Backend, command: EntityRegistryService.Command): IO[Unit] =
    val service = backend.compile(EntityRegistryService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      _ <- service(CommandMessage(commandId, now, address, command))
    yield ()
end RegistryBackend
