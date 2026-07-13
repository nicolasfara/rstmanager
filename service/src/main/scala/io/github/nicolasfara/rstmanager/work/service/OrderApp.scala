package io.github.nicolasfara.rstmanager.work.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import io.github.iltotore.iron.*
import skunk.Session

/**
 * Application backend for the order CRUD API, wired to the existing event-sourced [[OrderService]] plus a durable id registry. "Delete" maps to the
 * domain `Cancel` transition and removes the order from the collection index.
 */
object OrderApp:
  given BackendCodec[OrderEvent] = CirceCodec.jsonb
  given BackendCodec[OrderService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, Order, OrderEvent, OrderError, OrderService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(OrderService)
        .use(SkunkDriver("orders", pool))
        .inMemSnapshot(200)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("order_index", pool)
    yield Store(entity, registry)

  def create(store: Store, data: OrderData, promisedDeliveryDate: DateTime): IO[Either[OrderError, Unit]] =
    dispatch(store, data.id, OrderService.Command.Create(data, promisedDeliveryDate)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, data.id)
      case Left(_) => IO.unit
    }

  /** Dispatches any order lifecycle command (suspend, complete, deliver, change priority, …). */
  def command(store: Store, id: OrderId, command: OrderService.Command): IO[Either[OrderError, Unit]] =
    dispatch(store, id, command)

  /** Cancels the order and removes it from the collection index. */
  def delete(store: Store, id: OrderId, reason: Option[String :| CancellationReason]): IO[Either[OrderError, Unit]] =
    dispatch(store, id, OrderService.Command.Cancel(reason)).flatTap {
      case Right(_) => RegistryBackend.deregister(store.registry, id)
      case Left(_) => IO.unit
    }

  def get(store: Store, id: OrderId): IO[Option[Order]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(Order.NewOrder, _) => None
      case AggregateState.Valid(order, _) => Some(order)
      case _ => None
    }

  def exists(store: Store, id: OrderId): IO[Boolean] = get(store, id).map(_.isDefined)

  def list(store: Store): IO[List[Order]] =
    RegistryBackend.ids(store.registry).flatMap(_.toList.traverse(get(store, _))).map(_.flatten)

  private def dispatch(store: Store, id: OrderId, command: OrderService.Command): IO[Either[OrderError, Unit]] =
    val service = store.entity.compile(OrderService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end OrderApp
