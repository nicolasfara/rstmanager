package io.github.nicolasfara.rstmanager.planning.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.planning.*
import io.github.nicolasfara.rstmanager.planning.PlanningService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingInfo }
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask

import cats.data.{ NonEmptyChain, NonEmptyList }
import cats.effect.{ IO, IOApp, Resource }
import com.github.nscala_time.time.Imports.DateTime
import edomata.backend.Backend
import edomata.backend.eventsourcing.Backend as EventSourcedBackend
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import io.github.iltotore.iron.*
import org.slf4j.LoggerFactory
import skunk.Session

/**
 * Cats-effect backend for the planning service, following the edomata event-sourcing examples.
 *
 * The backend persists planning events in a Postgres journal (schema `planning`) through skunk, keeps snapshots in memory, and exposes the outbox
 * where [[PlanningService.Notification]] values are stored for downstream consumers.
 */
object PlanningApp:
  import PlanningCodecs.given

  given BackendCodec[PlanningEvent] = CirceCodec.jsonb
  given BackendCodec[Notification] = CirceCodec.jsonb

  /** Address of the singleton planning aggregate: planning attempts are serialized on this stream. */
  val planningAddress: String = "production-planning"

  /** Event-sourced planning backend type. */
  type PlanningBackend = EventSourcedBackend[IO, Planning, PlanningEvent, PlanningError, Notification]

  /** Builds the event-sourced planning backend on top of a skunk session pool. */
  def backend(pool: Resource[IO, Session[IO]]): Resource[IO, PlanningBackend] =
    Backend
      .builder(PlanningService)
      .use(SkunkDriver("planning", pool))
      .inMemSnapshot(200)
      .withRetryConfig(retryInitialDelay = 2.seconds)
      .build

  /** Sends one `ComputePlan` command to the compiled service. */
  def computePlan(
      backend: PlanningBackend,
      request: PlanningRequest,
      orders: List[Order],
      employees: List[Employee],
  ): IO[Either[NonEmptyChain[PlanningError], String]] =
    IO(UUID.randomUUID().nn.toString).flatMap(commandId => computePlanWithCommandId(backend, commandId, request, orders, employees))

  /** Sends one `ComputePlan` command with a caller-provided command id, useful for idempotent outbox-driven retries. */
  def computePlanWithCommandId(
      backend: PlanningBackend,
      commandId: String,
      request: PlanningRequest,
      orders: List[Order],
      employees: List[Employee],
  ): IO[Either[NonEmptyChain[PlanningError], String]] =
    val service = backend.compile(PlanningService[IO])
    service(CommandMessage(commandId, Instant.now().nn, planningAddress, Command.ComputePlan(request, orders, employees))).map(_.map(_ => commandId))
end PlanningApp

/**
 * Runnable HTTP service wiring the planning service to Postgres.
 *
 * Start a database with: `docker run --rm -p 5432:5432 -e POSTGRES_PASSWORD=postgres postgres:16`, then run `sbt service/run`. Swagger UI is exposed
 * at `/docs` and the OpenAPI document at `/docs/docs.yaml`.
 */
object Main extends IOApp.Simple:
  private val logger = LoggerFactory.getLogger(getClass).nn

  def run: IO[Unit] =
    for
      config <- PlanningServerConfig.load
      _ <- PlanningHttpServer.resource(config).use { _ =>
        IO(logger.info(s"RST Manager planning API listening on ${config.http.baseUrl}; Swagger UI: ${config.http.baseUrl}/docs")) >> IO.never
      }
    yield ()

/** Sample orders, workforce, and request used by the [[Main]] demo. */
object DemoScenario:
  final case class Scenario(request: PlanningRequest, orders: List[Order], employees: List[Employee])

  def create: Scenario =
    val today = DateTime.now().withTimeAtStartOfDay().nn
    val orderId: OrderId = UUID.randomUUID().nn

    val cutting: TaskId = UUID.randomUUID().nn
    val assembly: TaskId = UUID.randomUUID().nn
    val finishing: TaskId = UUID.randomUUID().nn
    val dependencies = ManufacturingDependencies()
      .addTaskDependencies(assembly, Set(cutting))
      .addTaskDependencies(finishing, Set(assembly))
    val tasks = NonEmptyList.of(
      PendingTask(UUID.randomUUID().nn, cutting, TaskHours(16)),
      PendingTask(UUID.randomUUID().nn, assembly, TaskHours(24)),
      PendingTask(UUID.randomUUID().nn, finishing, TaskHours(8)),
    )
    val manufacturing = ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        UUID.randomUUID().nn,
        "MFG-2026-001".refineUnsafe[ManufacturingCode],
        today.plusDays(7).nn,
        tasks,
        dependencies,
      ),
    )
    val order = InProgressOrder(
      OrderData(
        orderId,
        "ORD-2026-001".refineUnsafe[OrderNumber],
        UUID.randomUUID().nn: CustomerId,
        today.minusDays(3).nn,
        today.plusDays(10).nn,
        OrderPriority.Urgent,
        NonEmptyList.one(manufacturing),
      ),
      today.plusDays(10).nn,
    )

    val employees = List(
      employee("Anna", "Bianchi", weeklyHours = 40),
      employee("Luca", "Verdi", weeklyHours = 24),
    )
    val request = PlanningRequest(UUID.randomUUID().nn, today, PlanningTrigger.DailyPlanning, today, List(orderId))
    Scenario(request, List(order), employees)
  end create

  private def employee(name: String, surname: String, weeklyHours: Int): Employee =
    Employee(
      UUID.randomUUID().nn,
      EmployeeInfo(name.refineUnsafe[Name], surname.refineUnsafe[Surname]),
      Contract.FullTime(DateTime.now().minusYears(1).nn),
      BudgetHours(WeeklyHours.applyUnsafe(weeklyHours), Nil),
    )
end DemoScenario
