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
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingInfo }
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask

import cats.data.{ NonEmptyChain, NonEmptyList }
import cats.effect.{ IO, IOApp, Resource }
import com.github.nscala_time.time.Imports.DateTime
import edomata.backend.{ Backend, OutboxConsumer }
import edomata.backend.eventsourcing.Backend as EventSourcedBackend
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.Empty
import io.github.iltotore.iron.constraint.any.{ DescribedAs, Not }
import org.typelevel.otel4s.metrics.Meter.Implicits.noop
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
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
  ): IO[Either[NonEmptyChain[PlanningError], Unit]] =
    val service = backend.compile(PlanningService[IO])
    IO(UUID.randomUUID().nn.toString).flatMap { commandId =>
      service(CommandMessage(commandId, Instant.now().nn, planningAddress, Command.ComputePlan(request, orders, employees)))
    }
end PlanningApp

/**
 * Runnable demo wiring the planning service to a local Postgres instance.
 *
 * Start a database with: `docker run --rm -p 5432:5432 -e POSTGRES_PASSWORD=postgres postgres:16`, then run `sbt service/run`. The demo plans one
 * urgent order whose manufacturing has three dependent tasks over a two-week window, prints the resulting aggregate state, and streams the outbox
 * notifications produced by the run.
 */
object Main extends IOApp.Simple:
  private def sessionPool: Resource[IO, Resource[IO, Session[IO]]] =
    Session
      .Builder[IO]
      .withHost("localhost")
      .withPort(5432)
      .withDatabase("postgres")
      .withUserAndPassword("postgres", "postgres")
      .pooled(4)

  def run: IO[Unit] =
    sessionPool.flatMap(PlanningApp.backend).use { backend =>
      val consumer = OutboxConsumer(backend)(item => IO.println(s"[outbox] ${item.data}"))
      consumer.compile.drain.background.surround {
        for
          scenario <- IO(DemoScenario.create)
          result <- PlanningApp.computePlan(backend, scenario.request, scenario.orders, scenario.employees)
          _ <- result.fold(
            errors => IO.println(s"Planning command rejected: ${errors.toChain.toList.mkString(", ")}"),
            _ => IO.println("Planning command accepted"),
          )
          state <- backend.repository.get(PlanningApp.planningAddress)
          _ <- IO.println(s"Planning aggregate state: $state")
          _ <- IO.sleep(2.seconds)
        yield ()
      }
    }

/** Sample orders, workforce, and request used by the [[Main]] demo. */
object DemoScenario:
  final case class Scenario(request: PlanningRequest, orders: List[Order], employees: List[Employee])

  def create: Scenario =
    val today = DateTime.now().withTimeAtStartOfDay().nn
    val windowEnd = today.plusDays(13).nn
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
        new DescribedAs[Not[Empty], "The code manufacturing should be not empty"](): ManufacturingCode,
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
    val request = PlanningRequest(UUID.randomUUID().nn, PlanningWindow(today, windowEnd), PlanningTrigger.DailyPlanning, today, List(orderId))
    Scenario(request, List(order), employees)

  private def employee(name: String, surname: String, weeklyHours: Int): Employee =
    Employee(
      UUID.randomUUID().nn,
      EmployeeInfo(name.refineUnsafe[Name], surname.refineUnsafe[Surname]),
      Contract.FullTime(DateTime.now().minusYears(1).nn),
      BudgetHours(WeeklyHours.applyUnsafe(weeklyHours), Nil),
    )
end DemoScenario
