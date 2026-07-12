package io.github.nicolasfara.rstmanager.work.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.work.domain.task.*
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import skunk.Session

/** Application backend for the task catalog CRUD API: a [[TaskAggregate]] stream per id plus a durable id registry. */
object TaskApp:
  given BackendCodec[TaskEvent] = CirceCodec.jsonb
  given BackendCodec[TaskService.Notification] = CirceCodec.jsonb

  type EntityBackend = EventSourcedBackend[IO, TaskAggregate, TaskEvent, TaskError, TaskService.Notification]

  final case class Store(entity: EntityBackend, registry: RegistryBackend.Backend)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    for
      entity <- Backend
        .builder(TaskService)
        .use(SkunkDriver("tasks", pool))
        .inMemSnapshot(200)
        .withRetryConfig(retryInitialDelay = 2.seconds)
        .build
      registry <- RegistryBackend.build("task_index", pool)
    yield Store(entity, registry)

  def create(store: Store, task: Task): IO[Either[TaskError, Unit]] =
    dispatch(store, task.id, TaskService.Command.Create(task)).flatTap {
      case Right(_) => RegistryBackend.register(store.registry, task.id)
      case Left(_) => IO.unit
    }

  def update(store: Store, task: Task): IO[Either[TaskError, Unit]] =
    dispatch(store, task.id, TaskService.Command.Update(task))

  def delete(store: Store, id: TaskId): IO[Either[TaskError, Unit]] =
    dispatch(store, id, TaskService.Command.Delete).flatTap {
      case Right(_) => RegistryBackend.deregister(store.registry, id)
      case Left(_) => IO.unit
    }

  def get(store: Store, id: TaskId): IO[Option[Task]] =
    store.entity.repository.get(id.toString).map {
      case AggregateState.Valid(TaskAggregate.Active(task), _) => Some(task)
      case _ => None
    }

  def exists(store: Store, id: TaskId): IO[Boolean] = get(store, id).map(_.isDefined)

  def list(store: Store): IO[List[Task]] =
    RegistryBackend.ids(store.registry).flatMap(_.toList.traverse(get(store, _))).map(_.flatten)

  private def dispatch(store: Store, id: TaskId, command: TaskService.Command): IO[Either[TaskError, Unit]] =
    val service = store.entity.compile(TaskService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, id.toString, command))
    yield result.bimap(_.head, _ => ())
end TaskApp
