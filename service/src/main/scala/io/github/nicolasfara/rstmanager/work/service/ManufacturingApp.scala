package io.github.nicolasfara.rstmanager.work.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.{ RegistryBackend, StreamReader }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.Backend
import edomata.backend.eventsourcing.Backend as EventSourcedBackend
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import skunk.Session

/** Application backend for the manufacturing catalog CRUD API. */
object ManufacturingApp:
  given BackendCodec[ManufacturingEvent] = CirceCodec.jsonb
  given BackendCodec[ManufacturingService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, ManufacturingAggregate, ManufacturingEvent, ManufacturingError, ManufacturingService.Notification]

  final case class Store(
      entity: EntityBackend,
      registry: RegistryBackend.Backend,
      reader: StreamReader[ManufacturingAggregate, ManufacturingEvent, ManufacturingError],
  )

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(ManufacturingService)
        .use(SkunkDriver("manufacturings", pool))
        .inMemSnapshot(200)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("manufacturing_index", pool)
      reader <- Resource.eval(
        StreamReader[ManufacturingAggregate, ManufacturingEvent, ManufacturingError]("manufacturings", pool, StreamReader.entityCacheSize),
      )
    yield Store(entity, registry, reader)

  def create(store: Store, manufacturing: Manufacturing): IO[Either[ManufacturingError, Unit]] =
    dispatch(store, manufacturing.id, ManufacturingService.Command.Create(manufacturing)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, manufacturing.id.value)
      case Left(_) => IO.unit
    }

  def update(store: Store, manufacturing: Manufacturing): IO[Either[ManufacturingError, Unit]] =
    dispatch(store, manufacturing.id, ManufacturingService.Command.Update(manufacturing))

  def delete(store: Store, id: ManufacturingId): IO[Either[ManufacturingError, Unit]] =
    dispatch(store, id, ManufacturingService.Command.Delete).flatTap {
      case Right(_) => RegistryBackend.deregister(store.registry, id.value)
      case Left(_) => IO.unit
    }

  def get(store: Store, id: ManufacturingId): IO[Option[Manufacturing]] = getMany(store, List(id)).map(_.get(id))

  /** The active entities among `ids`, read in a single query. */
  def getMany(store: Store, ids: List[ManufacturingId]): IO[Map[ManufacturingId, Manufacturing]] =
    store.reader.valid(ids)(_.toString).map(_.collect { case (id, ManufacturingAggregate.Active(manufacturing)) => id -> manufacturing })

  def exists(store: Store, id: ManufacturingId): IO[Boolean] = get(store, id).map(_.isDefined)

  def list(store: Store): IO[List[Manufacturing]] =
    RegistryBackend.ids(store.registry).map(_.toList.map(ManufacturingId(_))).flatMap(ids => getMany(store, ids).map(found => ids.flatMap(found.get)))

  def findByTaskId(store: Store, taskId: TaskId): IO[List[Manufacturing]] =
    list(store).map(_.filter(_.taskIds.exists(_.equals(taskId))))

  private def dispatch(store: Store, id: ManufacturingId, command: ManufacturingService.Command): IO[Either[ManufacturingError, Unit]] =
    val service = store.entity.compile(ManufacturingService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end ManufacturingApp
