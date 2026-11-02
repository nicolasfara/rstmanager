package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.warehouse.domain.ReportedProduct
import io.github.nicolasfara.rstmanager.warehouse.domain.product.*
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import io.github.iltotore.iron.:|
import skunk.Session

/**
 * Application backend for the warehouse product API: a [[ProductAggregate]] stream per id plus a durable id registry.
 *
 * Archiving keeps the id registered: archived products stay readable ([[find]], [[listAll]]) for the history, while [[get]] and [[list]] only see
 * active ones.
 */
object ProductApp:
  given BackendCodec[ProductEvent] = CirceCodec.jsonb
  given BackendCodec[ProductService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, ProductAggregate, ProductEvent, ProductError, ProductService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  /** A stored product, active (`archivedOn` empty) or archived. */
  final case class StoredProduct(product: ProductRecord, archivedOn: Option[LocalDate]):
    def info: ProductInfo = product.info
    def archived: Boolean = archivedOn.isDefined

    /** The product as the warehouse reports see it. */
    def reported: ReportedProduct = ReportedProduct(product, archivedOn)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(ProductService)
        .use(SkunkDriver("products", pool))
        .inMemSnapshot(2000)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("product_index", pool)
    yield Store(entity, registry)

  def create(store: Store, info: ProductInfo): IO[Either[ProductError, Unit]] =
    dispatch(store, info.id, ProductService.Command.Create(info)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, info.id.value)
      case Left(_) => IO.unit
    }

  def update(store: Store, info: ProductInfo): IO[Either[ProductError, Unit]] =
    dispatch(store, info.id, ProductService.Command.Update(info))

  /** Archives the product; its id stays in the registry. */
  def archive(store: Store, id: ProductId): IO[Either[ProductError, Unit]] =
    dispatch(store, id, ProductService.Command.Archive)

  /**
   * Records a stock movement of product `id`: `command` receives a fresh movement id (the domain never draws random numbers) and builds one of the
   * movement commands ([[ProductService.Command.RecordPurchase]], [[ProductService.Command.Unload]], [[ProductService.Command.Adjust]]).
   */
  def recordMovement(store: Store, id: ProductId, command: MovementId => ProductService.Command): IO[Either[ProductError, MovementId]] =
    IO(MovementId(UUID.randomUUID().nn)).flatMap(movementId => dispatch(store, id, command(movementId)).map(_.as(movementId)))

  /** Voids movement `movementId` of product `id` for `reason`. */
  def voidMovement(store: Store, id: ProductId, movementId: MovementId, reason: String :| MovementReason): IO[Either[ProductError, Unit]] =
    dispatch(store, id, ProductService.Command.VoidMovement(movementId, reason))

  /**
   * Sets the consumption of an order manufacturing on product `id` (see [[ProductService.Command.SetConsumption]]) under `commandId`: the
   * reconciliation derives it from what it applies, so the same step sent twice (a retry, two reconciliations racing) is handled once.
   */
  def setConsumption(
      store: Store,
      id: ProductId,
      command: ProductService.Command.SetConsumption,
      commandId: String,
  ): IO[Either[ProductError, Unit]] =
    dispatchAs(store, id, command, commandId)

  /** The product, active or archived. */
  def find(store: Store, id: ProductId): IO[Option[StoredProduct]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(ProductAggregate.Active(product), _) => Some(StoredProduct(product, None))
      case AggregateState.Valid(ProductAggregate.Archived(product, archivedOn), _) => Some(StoredProduct(product, Some(archivedOn)))
      case _ => None
    }

  /** The product, if it is active. */
  def get(store: Store, id: ProductId): IO[Option[ProductRecord]] =
    find(store, id).map(_.collect { case StoredProduct(product, None) => product })

  /** Every product, archived ones included. */
  def listAll(store: Store): IO[List[StoredProduct]] =
    RegistryBackend.ids(store.registry).flatMap(_.toList.traverse(id => find(store, ProductId(id)))).map(_.flatten)

  /** Every active product. */
  def list(store: Store): IO[List[ProductRecord]] =
    listAll(store).map(_.collect { case StoredProduct(product, None) => product })

  /** Every product, archived ones included, with an MPN of `supplierId`. */
  def referencingSupplier(store: Store, supplierId: SupplierId): IO[List[StoredProduct]] =
    listAll(store).map(_.filter(_.info.suppliers.contains(supplierId)))

  private def dispatch(store: Store, id: ProductId, command: ProductService.Command): IO[Either[ProductError, Unit]] =
    IO(UUID.randomUUID().nn.toString).flatMap(dispatchAs(store, id, command, _))

  private def dispatchAs(store: Store, id: ProductId, command: ProductService.Command, commandId: String): IO[Either[ProductError, Unit]] =
    val service = store.entity.compile(ProductService[IO])
    for
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end ProductApp
