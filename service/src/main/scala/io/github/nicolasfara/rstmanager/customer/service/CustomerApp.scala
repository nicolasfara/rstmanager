package io.github.nicolasfara.rstmanager.customer.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.customer.domain.*
import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent
import io.github.nicolasfara.rstmanager.service.codecs.CustomerCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import skunk.Session

/** Application backend for the customer CRUD API: a [[CustomerAggregate]] stream per id plus a durable id registry. */
object CustomerApp:
  given BackendCodec[CustomerEvent] = CirceCodec.jsonb
  given BackendCodec[CustomerService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, CustomerAggregate, CustomerEvent, CustomerError, CustomerService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(CustomerService)
        .use(SkunkDriver("customers", pool))
        .inMemSnapshot(200)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("customer_index", pool)
    yield Store(entity, registry)

  def create(store: Store, customer: Customer): IO[Either[CustomerError, Unit]] =
    dispatch(store, customer.id, CustomerService.Command.Create(customer)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, customer.id)
      case Left(_) => IO.unit
    }

  def update(store: Store, customer: Customer): IO[Either[CustomerError, Unit]] =
    dispatch(store, customer.id, CustomerService.Command.Update(customer))

  def delete(store: Store, id: CustomerId): IO[Either[CustomerError, Unit]] =
    dispatch(store, id, CustomerService.Command.Delete).flatTap {
      case Right(_) => RegistryBackend.deregister(store.registry, id)
      case Left(_) => IO.unit
    }

  def get(store: Store, id: CustomerId): IO[Option[Customer]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(CustomerAggregate.Active(customer), _) => Some(customer)
      case _ => None
    }

  def exists(store: Store, id: CustomerId): IO[Boolean] = get(store, id).map(_.isDefined)

  def list(store: Store): IO[List[Customer]] =
    RegistryBackend.ids(store.registry).flatMap(_.toList.traverse(get(store, _))).map(_.flatten)

  private def dispatch(store: Store, id: CustomerId, command: CustomerService.Command): IO[Either[CustomerError, Unit]] =
    val service = store.entity.compile(CustomerService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end CustomerApp
