package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.warehouse.domain.ReportedMaterial
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent
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
 * Application backend for the warehouse material API: a [[MaterialAggregate]] stream per id plus a durable id registry.
 *
 * Archiving keeps the id registered: archived materials stay readable ([[find]], [[listAll]]) for the history, while [[get]] and [[list]] only see
 * active ones.
 */
object MaterialApp:
  given BackendCodec[MaterialEvent] = CirceCodec.jsonb
  given BackendCodec[MaterialService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, MaterialAggregate, MaterialEvent, MaterialError, MaterialService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  /** A stored material, active (`archivedOn` empty) or archived. */
  final case class StoredMaterial(material: MaterialRecord, archivedOn: Option[LocalDate]):
    def info: MaterialInfo = material.info
    def archived: Boolean = archivedOn.isDefined

    /** The material as the warehouse reports see it. */
    def reported: ReportedMaterial = ReportedMaterial(material, archivedOn)

  /** The journal and the registry keep the names they were created with, when the materials were products: renaming them would orphan the data. */
  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(MaterialService)
        .use(SkunkDriver("products", pool))
        .inMemSnapshot(2000)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("product_index", pool)
    yield Store(entity, registry)

  def create(store: Store, info: MaterialInfo): IO[Either[MaterialError, Unit]] =
    dispatch(store, info.id, MaterialService.Command.Create(info)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, info.id.value)
      case Left(_) => IO.unit
    }

  def update(store: Store, info: MaterialInfo): IO[Either[MaterialError, Unit]] =
    dispatch(store, info.id, MaterialService.Command.Update(info))

  /** Archives the material; its id stays in the registry. */
  def archive(store: Store, id: MaterialId): IO[Either[MaterialError, Unit]] =
    dispatch(store, id, MaterialService.Command.Archive)

  /**
   * Records a stock movement of material `id`: `command` receives a fresh movement id (the domain never draws random numbers) and builds one of the
   * movement commands ([[MaterialService.Command.RecordPurchase]], [[MaterialService.Command.Unload]], [[MaterialService.Command.Adjust]]).
   */
  def recordMovement(store: Store, id: MaterialId, command: MovementId => MaterialService.Command): IO[Either[MaterialError, MovementId]] =
    IO(MovementId(UUID.randomUUID().nn)).flatMap(movementId => dispatch(store, id, command(movementId)).map(_.as(movementId)))

  /** Voids movement `movementId` of material `id` for `reason`. */
  def voidMovement(store: Store, id: MaterialId, movementId: MovementId, reason: String :| MovementReason): IO[Either[MaterialError, Unit]] =
    dispatch(store, id, MaterialService.Command.VoidMovement(movementId, reason))

  /**
   * Sets the consumption of an order manufacturing on material `id` (see [[MaterialService.Command.SetConsumption]]) under `commandId`: the
   * reconciliation derives it from what it applies, so the same step sent twice (a retry, two reconciliations racing) is handled once.
   */
  def setConsumption(
      store: Store,
      id: MaterialId,
      command: MaterialService.Command.SetConsumption,
      commandId: String,
  ): IO[Either[MaterialError, Unit]] =
    dispatchAs(store, id, command, commandId)

  /** The material, active or archived. */
  def find(store: Store, id: MaterialId): IO[Option[StoredMaterial]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(MaterialAggregate.Active(material), _) => Some(StoredMaterial(material, None))
      case AggregateState.Valid(MaterialAggregate.Archived(material, archivedOn), _) => Some(StoredMaterial(material, Some(archivedOn)))
      case _ => None
    }

  /** The material, if it is active. */
  def get(store: Store, id: MaterialId): IO[Option[MaterialRecord]] =
    find(store, id).map(_.collect { case StoredMaterial(material, None) => material })

  /** Every material, archived ones included. */
  def listAll(store: Store): IO[List[StoredMaterial]] =
    RegistryBackend.ids(store.registry).flatMap(_.toList.traverse(id => find(store, MaterialId(id)))).map(_.flatten)

  /** Every active material. */
  def list(store: Store): IO[List[MaterialRecord]] =
    listAll(store).map(_.collect { case StoredMaterial(material, None) => material })

  /** Every material, archived ones included, with an MPN of `supplierId`. */
  def referencingSupplier(store: Store, supplierId: SupplierId): IO[List[StoredMaterial]] =
    listAll(store).map(_.filter(_.info.suppliers.contains(supplierId)))

  private def dispatch(store: Store, id: MaterialId, command: MaterialService.Command): IO[Either[MaterialError, Unit]] =
    IO(UUID.randomUUID().nn.toString).flatMap(dispatchAs(store, id, command, _))

  private def dispatchAs(store: Store, id: MaterialId, command: MaterialService.Command, commandId: String): IO[Either[MaterialError, Unit]] =
    val service = store.entity.compile(MaterialService[IO])
    for
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end MaterialApp
