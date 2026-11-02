package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import skunk.Session

/**
 * Application backend for the supplier CRUD API: a [[SupplierAggregate]] stream per id plus a durable id registry.
 *
 * Deletion is soft: a deleted supplier stays registered, so products and purchases referencing it can still show its name ([[getAny]]), while [[get]]
 * and [[list]] only see active suppliers.
 */
object SupplierApp:
  given BackendCodec[SupplierEvent] = CirceCodec.jsonb
  given BackendCodec[SupplierService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, SupplierAggregate, SupplierEvent, SupplierError, SupplierService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  /** A stored supplier, active or (soft) deleted. */
  final case class StoredSupplier(supplier: Supplier, deleted: Boolean)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(SupplierService)
        .use(SkunkDriver("suppliers", pool))
        .inMemSnapshot(200)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("supplier_index", pool)
    yield Store(entity, registry)

  def create(store: Store, supplier: Supplier): IO[Either[SupplierError, Unit]] =
    dispatch(store, supplier.id, SupplierService.Command.Create(supplier)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, supplier.id.value)
      case Left(_) => IO.unit
    }

  def update(store: Store, supplier: Supplier): IO[Either[SupplierError, Unit]] =
    dispatch(store, supplier.id, SupplierService.Command.Update(supplier))

  /** Soft-deletes the supplier; its id stays in the registry. */
  def delete(store: Store, id: SupplierId): IO[Either[SupplierError, Unit]] =
    dispatch(store, id, SupplierService.Command.Delete)

  /** The supplier, active or deleted. */
  def getAny(store: Store, id: SupplierId): IO[Option[StoredSupplier]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(SupplierAggregate.Active(supplier), _) => Some(StoredSupplier(supplier, deleted = false))
      case AggregateState.Valid(SupplierAggregate.Deleted(supplier), _) => Some(StoredSupplier(supplier, deleted = true))
      case _ => None
    }

  /** The supplier, if it is active. */
  def get(store: Store, id: SupplierId): IO[Option[Supplier]] =
    getAny(store, id).map(_.collect { case StoredSupplier(supplier, false) => supplier })

  def exists(store: Store, id: SupplierId): IO[Boolean] = get(store, id).map(_.isDefined)

  /** Every active supplier. */
  def list(store: Store): IO[List[Supplier]] =
    RegistryBackend.ids(store.registry).flatMap(_.toList.traverse(id => get(store, SupplierId(id)))).map(_.flatten)

  private def dispatch(store: Store, id: SupplierId, command: SupplierService.Command): IO[Either[SupplierError, Unit]] =
    val service = store.entity.compile(SupplierService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end SupplierApp
