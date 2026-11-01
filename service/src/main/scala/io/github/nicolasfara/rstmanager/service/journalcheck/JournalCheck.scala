package io.github.nicolasfara.rstmanager.service.journalcheck

import io.github.nicolasfara.rstmanager.customer.domain.{ CustomerAggregate, CustomerService }
import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent
import io.github.nicolasfara.rstmanager.hr.domain.{ EmployeeAggregate, EmployeeService }
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent
import io.github.nicolasfara.rstmanager.planning.{ Planning, PlanningService }
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.planning.service.PlanningCodecs.given
import io.github.nicolasfara.rstmanager.planning.service.PlanningServerConfig
import io.github.nicolasfara.rstmanager.service.codecs.CustomerCodecs.given
import io.github.nicolasfara.rstmanager.service.codecs.HrCodecs.given
import io.github.nicolasfara.rstmanager.service.codecs.RegistryCodecs.given
import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.service.registry.{ EntityRegistry, EntityRegistryEvent, EntityRegistryService }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingAggregate, ManufacturingService }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderService }
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskAggregate, TaskService }
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent

import cats.effect.{ ExitCode, IO, IOApp }
import cats.syntax.all.*
import io.circe.Decoder
import org.typelevel.otel4s.metrics.Meter.Implicits.noop
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import skunk.{ Query, Session, Void }
import skunk.codec.all.{ bool, int8, text }
import skunk.implicits.*

/** A journal namespace (a Postgres schema holding `journal` and `outbox`) and how its tables are checked. */
final case class CheckedNamespace(name: String, journal: RowCheck, outbox: RowCheck)

/**
 * Pre-deploy check: reads every journal row and every pending outbox row of a database with the codecs and aggregates of this release, and
 * exits with an error if any of them could not be read back, or could not be replayed into its aggregate. Run it with the release about to be
 * deployed, against production or a copy of it (`RSTMANAGER_DB_*`, as for the service): it only reads.
 */
object JournalCheck extends IOApp:
  private val failuresShownPerTable = 20

  val namespaces: List[CheckedNamespace] =
    def registry(name: String) =
      CheckedNamespace(name, RowCheck.journal(EntityRegistry, Decoder[EntityRegistryEvent]), RowCheck.outbox(Decoder[EntityRegistryService.Notification]))
    List(
      CheckedNamespace("customers", RowCheck.journal(CustomerAggregate, Decoder[CustomerEvent]), RowCheck.outbox(Decoder[CustomerService.Notification])),
      CheckedNamespace("employees", RowCheck.journal(EmployeeAggregate, Decoder[EmployeeEvent]), RowCheck.outbox(Decoder[EmployeeService.Notification])),
      CheckedNamespace("tasks", RowCheck.journal(TaskAggregate, Decoder[TaskEvent]), RowCheck.outbox(Decoder[TaskService.Notification])),
      CheckedNamespace(
        "manufacturings",
        RowCheck.journal(ManufacturingAggregate, Decoder[ManufacturingEvent]),
        RowCheck.outbox(Decoder[ManufacturingService.Notification]),
      ),
      CheckedNamespace("orders", RowCheck.journal(Order, Decoder[OrderEvent]), RowCheck.outbox(Decoder[OrderService.Notification])),
      CheckedNamespace("planning", RowCheck.journal(Planning, Decoder[PlanningEvent]), RowCheck.outbox(Decoder[PlanningService.Notification])),
      registry("customer_index"),
      registry("employee_index"),
      registry("task_index"),
      registry("manufacturing_index"),
      registry("order_index"),
    )
  end namespaces

  def run(args: List[String]): IO[ExitCode] =
    for
      database <- IO.fromEither(PlanningServerConfig.fromEnv(sys.env).map(_.database).left.map(new IllegalArgumentException(_)))
      _ <- IO.println(s"Checking the journals of ${database.database} on ${database.host}:${database.port}")
      reports <- Session
        .Builder[IO]
        .withHost(database.host)
        .withPort(database.port)
        .withDatabase(database.database)
        .withUserAndPassword(database.user, database.password)
        .single
        .use(session => namespaces.flatTraverse(namespace => checkNamespace(session, namespace)))
      failed = reports.count((_, report) => report.exists(!_.ok))
      _ <- IO.println(if failed == 0 then "All journals can be read by this release." else s"$failed table(s) cannot be read by this release.")
    yield if failed == 0 then ExitCode.Success else ExitCode.Error

  private def checkNamespace(session: Session[IO], namespace: CheckedNamespace): IO[List[(String, Option[RowReport])]] =
    List(
      "journal" -> (namespace.journal, journalRows(namespace.name)),
      "outbox" -> (namespace.outbox, pendingOutboxRows(namespace.name)),
    ).traverse { case (table, (check, rows)) =>
      val name = s"${namespace.name}.$table"
      for
        present <- session.unique(tableExists)((namespace.name, table))
        report <-
          if present then check.run(session.stream(rows)(Void, 256)).compile.lastOrError.map(Some(_))
          else IO.pure(None)
        _ <- IO.println(describe(name, report))
      yield name -> report
    }

  private def describe(name: String, report: Option[RowReport]): String =
    report.fold(f"$name%-32s absent") { report =>
      val summary = f"$name%-32s ${report.rows}%8d rows ${report.streams}%6d streams  "
      if report.ok then summary + "OK"
      else
        val shown = report.failures.take(failuresShownPerTable).map(failure => s"  ${failure.stream} @ ${failure.position}: ${failure.reason}")
        val hidden = report.failures.size - shown.size
        (Vector(summary + s"FAILED (${report.failures.size})") ++ shown ++ Option.when(hidden > 0)(s"  … and $hidden more")).mkString("\n")
    }

  private val tableExists: Query[(String, String), Boolean] =
    sql"select exists (select 1 from information_schema.tables where table_schema = $text and table_name = $text)".query(bool)

  private def journalRows(namespace: String): Query[Void, StoredRow] =
    sql"""select stream, version, payload::text from "#$namespace".journal order by stream, version""".query((text *: int8 *: text).to[StoredRow])

  private def pendingOutboxRows(namespace: String): Query[Void, StoredRow] =
    sql"""select stream, seqnr, payload::text from "#$namespace".outbox where published is null order by stream, seqnr"""
      .query((text *: int8 *: text).to[StoredRow])
end JournalCheck
