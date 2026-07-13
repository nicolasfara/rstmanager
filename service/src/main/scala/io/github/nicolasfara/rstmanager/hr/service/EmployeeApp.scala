package io.github.nicolasfara.rstmanager.hr.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent
import io.github.nicolasfara.rstmanager.service.codecs.HrCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import skunk.Session

/**
 * Application backend for the employee CRUD API: an [[EmployeeAggregate]] stream per id plus a durable id registry.
 *
 * `create`/`delete` keep the registry in sync after a successful entity command; `list` reads the registry then loads each active record.
 */
object EmployeeApp:
  given BackendCodec[EmployeeEvent] = CirceCodec.jsonb
  given BackendCodec[EmployeeService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, EmployeeAggregate, EmployeeEvent, EmployeeError, EmployeeService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(EmployeeService)
        .use(SkunkDriver("employees", pool))
        .inMemSnapshot(200)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("employee_index", pool)
    yield Store(entity, registry)

  def create(store: Store, employee: Employee): IO[Either[EmployeeError, Unit]] =
    dispatch(store, employee.id, EmployeeService.Command.Create(employee)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, employee.id)
      case Left(_) => IO.unit
    }

  def update(store: Store, employee: Employee): IO[Either[EmployeeError, Unit]] =
    dispatch(store, employee.id, EmployeeService.Command.Update(employee))

  def delete(store: Store, id: EmployeeId): IO[Either[EmployeeError, Unit]] =
    dispatch(store, id, EmployeeService.Command.Delete).flatTap {
      case Right(_) => RegistryBackend.deregister(store.registry, id)
      case Left(_) => IO.unit
    }

  def get(store: Store, id: EmployeeId): IO[Option[Employee]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(EmployeeAggregate.Active(employee), _) => Some(employee)
      case _ => None
    }

  def exists(store: Store, id: EmployeeId): IO[Boolean] = get(store, id).map(_.isDefined)

  def list(store: Store): IO[List[Employee]] =
    RegistryBackend.ids(store.registry).flatMap(_.toList.traverse(get(store, _))).map(_.flatten)

  private def dispatch(store: Store, id: EmployeeId, command: EmployeeService.Command): IO[Either[EmployeeError, Unit]] =
    val service = store.entity.compile(EmployeeService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end EmployeeApp
