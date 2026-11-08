package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.*
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events.DeliveryNoteEvent

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import io.github.iltotore.iron.:|
import skunk.Session

/**
 * Application backend for the delivery note (DDT) API: a [[DeliveryNoteAggregate]] stream per id plus a durable id registry.
 *
 * Cancelling keeps the id registered: cancelled notes stay readable ([[find]], [[listAll]]) for the history. The purchases a note brings are recorded
 * on the materials by [[WarehouseDeliveryNoteReconciler]], never here.
 */
object DeliveryNoteApp:
  given BackendCodec[DeliveryNoteEvent] = CirceCodec.jsonb
  given BackendCodec[DeliveryNoteService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, DeliveryNoteAggregate, DeliveryNoteEvent, DeliveryNoteError, DeliveryNoteService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(DeliveryNoteService)
        .use(SkunkDriver("delivery_notes", pool))
        .inMemSnapshot(500)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("delivery_note_index", pool)
    yield Store(entity, registry)

  def register(store: Store, info: DeliveryNoteInfo): IO[Either[DeliveryNoteError, Unit]] =
    dispatch(store, info.id, DeliveryNoteService.Command.Register(info)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, info.id.value)
      case Left(_) => IO.unit
    }

  def change(store: Store, info: DeliveryNoteInfo): IO[Either[DeliveryNoteError, Unit]] =
    dispatch(store, info.id, DeliveryNoteService.Command.Change(info))

  /** Cancels the note; its id stays in the registry. */
  def cancel(store: Store, id: DeliveryNoteId, reason: String :| CancellationReason): IO[Either[DeliveryNoteError, Unit]] =
    dispatch(store, id, DeliveryNoteService.Command.Cancel(reason))

  /** The note, registered or cancelled. */
  def find(store: Store, id: DeliveryNoteId): IO[Option[DeliveryNoteAggregate]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(note @ (DeliveryNoteAggregate.Registered(_) | DeliveryNoteAggregate.Cancelled(_, _, _)), _) => Some(note)
      case _ => None
    }

  /** Every note id of the registry, in a stable order. */
  def ids(store: Store): IO[List[DeliveryNoteId]] = RegistryBackend.ids(store.registry).map(_.toList.sortBy(_.toString).map(DeliveryNoteId(_)))

  /** Every note, cancelled ones included. */
  def listAll(store: Store): IO[List[DeliveryNoteAggregate]] = ids(store).flatMap(_.traverse(find(store, _))).map(_.flatten)

  private def dispatch(store: Store, id: DeliveryNoteId, command: DeliveryNoteService.Command): IO[Either[DeliveryNoteError, Unit]] =
    val service = store.entity.compile(DeliveryNoteService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end DeliveryNoteApp
