package io.github.nicolasfara.rstmanager.planning.service

import io.github.nicolasfara.rstmanager.customer.service.CustomerApp
import io.github.nicolasfara.rstmanager.hr.service.EmployeeApp
import io.github.nicolasfara.rstmanager.service.ApiServer
import io.github.nicolasfara.rstmanager.work.service.{ ManufacturingApp, OrderApp, TaskApp }

import cats.effect.{ IO, Resource }
import com.comcast.ip4s.{ Host, Port }
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import org.http4s.server.middleware.{ CORS, Logger as HttpLogger }
import org.slf4j.LoggerFactory
import org.typelevel.otel4s.metrics.Meter.Implicits.noop
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import skunk.Session

final case class PlanningHttpConfig(host: String, port: Int):
  def baseUrl: String = s"http://$host:$port"

final case class PlanningDatabaseConfig(host: String, port: Int, database: String, user: String, password: String, poolSize: Int)

final case class PlanningServerConfig(http: PlanningHttpConfig, database: PlanningDatabaseConfig)

object PlanningServerConfig:
  def load: IO[PlanningServerConfig] =
    IO.fromEither(fromEnv(sys.env).left.map(new IllegalArgumentException(_)))

  def fromEnv(env: Map[String, String]): Either[String, PlanningServerConfig] =
    for
      httpPort <- intValue(env, "RSTMANAGER_HTTP_PORT", 8080)
      databasePort <- intValue(env, "RSTMANAGER_DB_PORT", 5432)
      poolSize <- intValue(env, "RSTMANAGER_DB_POOL_SIZE", 4)
    yield PlanningServerConfig(
      PlanningHttpConfig(
        env.getOrElse("RSTMANAGER_HTTP_HOST", "0.0.0.0"),
        httpPort,
      ),
      PlanningDatabaseConfig(
        env.getOrElse("RSTMANAGER_DB_HOST", "localhost"),
        databasePort,
        env.getOrElse("RSTMANAGER_DB_NAME", "postgres"),
        env.getOrElse("RSTMANAGER_DB_USER", "postgres"),
        env.getOrElse("RSTMANAGER_DB_PASSWORD", "postgres"),
        poolSize,
      ),
    )

  private def intValue(env: Map[String, String], key: String, default: Int): Either[String, Int] =
    env.get(key).fold(Right(default)) { raw =>
      raw.toIntOption.toRight(s"$key must be an integer, got '$raw'.")
    }
end PlanningServerConfig

object PlanningHttpServer:
  private val logger = LoggerFactory.getLogger(getClass).nn

  def resource(config: PlanningServerConfig): Resource[IO, Server] =
    for
      httpHost <- Resource.eval(parseHost(config.http.host))
      httpPort <- Resource.eval(parsePort(config.http.port))
      pool <- sessionPool(config.database)
      planningBackend <- PlanningApp.backend(pool)
      employees <- EmployeeApp.build(pool)
      customers <- CustomerApp.build(pool)
      tasks <- TaskApp.build(pool)
      manufacturings <- ManufacturingApp.build(pool)
      orders <- OrderApp.build(pool)
      planningGateway = PlanningEntityGateway.fromStores(orders, employees)
      planningRecalculator = PlanningRecalculationService(planningBackend, planningGateway)
      _ <- PlanningDependencyConsumer.resource(orders, employees, planningRecalculator)
      routes = ApiServer.routes(planningBackend, employees, customers, tasks, manufacturings, orders)
      httpApp = CORS.policy.withAllowOriginAll(routes).orNotFound
      loggedHttpApp = HttpLogger.httpApp[IO](
        logHeaders = false,
        logBody = false,
        logAction = Some(message => IO(logger.info(message))),
      )(httpApp)
      server <- EmberServerBuilder
        .default[IO]
        .withHost(httpHost)
        .withPort(httpPort)
        .withHttpApp(loggedHttpApp)
        .build
    yield server

  private def sessionPool(config: PlanningDatabaseConfig): Resource[IO, Resource[IO, Session[IO]]] =
    Session
      .Builder[IO]
      .withHost(config.host)
      .withPort(config.port)
      .withDatabase(config.database)
      .withUserAndPassword(config.user, config.password)
      .pooled(config.poolSize)

  private def parseHost(value: String): IO[Host] =
    IO.fromOption(Host.fromString(value))(new IllegalArgumentException(s"RSTMANAGER_HTTP_HOST is not a valid host: $value"))

  private def parsePort(value: Int): IO[Port] =
    IO.fromOption(Port.fromInt(value))(new IllegalArgumentException(s"RSTMANAGER_HTTP_PORT is not a valid port: $value"))
end PlanningHttpServer
