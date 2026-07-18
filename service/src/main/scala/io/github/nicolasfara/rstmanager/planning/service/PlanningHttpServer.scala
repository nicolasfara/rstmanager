package io.github.nicolasfara.rstmanager.planning.service

import io.github.nicolasfara.rstmanager.customer.service.CustomerApp
import io.github.nicolasfara.rstmanager.hr.service.EmployeeApp
import io.github.nicolasfara.rstmanager.service.ApiServer
import io.github.nicolasfara.rstmanager.service.auth.{ AuthConfig, JwksClient, JwtValidator }
import io.github.nicolasfara.rstmanager.service.http.ApiSecurity
import io.github.nicolasfara.rstmanager.work.service.{ ManufacturingApp, OrderApp, TaskApp }

import scala.concurrent.duration.DurationInt

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import com.comcast.ip4s.{ Host, Port }
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
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

/**
 * Keycloak validation settings. `internalBaseUrl` is where the service fetches the JWKS (docker-network address), while `issuer` is the external
 * realm URL the browser sees: tokens carry the external issuer regardless of which interface served the request (KC_HOSTNAME).
 */
final case class KeycloakConfig(internalBaseUrl: String, realm: String, issuer: String, clientId: String):
  def jwksUrl: String = s"$internalBaseUrl/realms/$realm/protocol/openid-connect/certs"

final case class PlanningServerConfig(
    http: PlanningHttpConfig,
    database: PlanningDatabaseConfig,
    keycloak: KeycloakConfig,
    corsAllowedOrigins: List[String],
)

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
      KeycloakConfig(
        env.getOrElse("RSTMANAGER_KEYCLOAK_INTERNAL_URL", "http://localhost:8081/auth"),
        env.getOrElse("RSTMANAGER_KEYCLOAK_REALM", "rstmanager"),
        env.getOrElse("RSTMANAGER_KEYCLOAK_ISSUER", "http://localhost:3333/auth/realms/rstmanager"),
        env.getOrElse("RSTMANAGER_KEYCLOAK_CLIENT_ID", "rstmanager-frontend"),
      ),
      env.get("RSTMANAGER_CORS_ALLOWED_ORIGINS").fold(List.empty[String])(_.split(',').toList.map(_.trim.nn).filter(_.nonEmpty)),
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
      security <- apiSecurity(config.keycloak)
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
      routes = ApiServer.routes(planningBackend, employees, customers, tasks, manufacturings, orders, security)
      httpApp = corsPolicy(config.corsAllowedOrigins)(routes).orNotFound
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

  /** Browser traffic is same-origin through the nginx/vite proxy, so CORS stays off unless origins are explicitly configured. */
  private def corsPolicy(allowedOrigins: List[String]): org.http4s.HttpRoutes[IO] => org.http4s.HttpRoutes[IO] =
    if allowedOrigins.isEmpty then identity
    else CORS.policy.withAllowOriginHost(allowedOrigins.flatMap(origin => Uri.fromString(origin).toOption.flatMap(originFromUri)).toSet).apply

  private def originFromUri(uri: Uri): Option[org.http4s.headers.Origin.Host] =
    (uri.scheme, uri.host).mapN((scheme, host) => org.http4s.headers.Origin.Host(scheme, host, uri.port))

  /** Builds the JWKS-backed validator, retrying the initial key fetch so a race with Keycloak startup does not kill the service. */
  private def apiSecurity(keycloak: KeycloakConfig): Resource[IO, ApiSecurity] =
    for
      client <- EmberClientBuilder.default[IO].build
      jwksUri <- Resource.eval(IO.fromEither(Uri.fromString(keycloak.jwksUrl)))
      jwks <- Resource.eval(JwksClient.build(client, jwksUri))
      _ <- Resource.eval(initialJwksFetch(jwks, attempts = 30))
    yield ApiSecurity(new JwtValidator(jwks.keyFor, AuthConfig(keycloak.issuer, keycloak.clientId)))

  private def initialJwksFetch(jwks: JwksClient, attempts: Int): IO[Unit] =
    jwks.refresh.flatTap(_ => IO(logger.info("Fetched Keycloak JWKS."))).handleErrorWith { error =>
      if attempts <= 1 then IO(logger.warn(s"Could not fetch Keycloak JWKS at startup, continuing with lazy fetch: $error"))
      else IO(logger.info(s"Keycloak JWKS not available yet, retrying: $error")) *> IO.sleep(2.seconds) *> initialJwksFetch(jwks, attempts - 1)
    }

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
