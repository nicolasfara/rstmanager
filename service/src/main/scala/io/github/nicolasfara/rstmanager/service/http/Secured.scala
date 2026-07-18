package io.github.nicolasfara.rstmanager.service.http

import io.github.nicolasfara.rstmanager.service.auth.{ AuthedUser, JwtValidator, Role }

import cats.effect.IO
import sttp.model.StatusCode
import sttp.model.headers.WWWAuthenticateChallenge
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint

/** Bearer-secured variant of [[ApiError.base]]: every protected endpoint under `/api/v1` derives from [[Secured.base]]. */
object Secured:
  import ApiError.ApiFailure

  /** An endpoint whose security input is the raw bearer token. */
  type SecuredEndpoint[I, R] = Endpoint[String, I, ApiFailure, R, Any]

  val base: SecuredEndpoint[Unit, Unit] =
    ApiError.base.securityIn(auth.bearer[String](WWWAuthenticateChallenge.bearer).securitySchemeName("bearerAuth"))

  def unauthorized(reason: String): ApiFailure =
    StatusCode.Unauthorized -> ApiError("unauthorized", "A valid bearer token is required.", List(reason))

  def forbidden(required: Role): ApiFailure =
    StatusCode.Forbidden -> ApiError("forbidden", s"This operation requires the '${required.name}' role.", Nil)
end Secured

/** Applies token validation and the per-endpoint required role, leaving the business logic untouched. */
final class ApiSecurity(validator: JwtValidator):
  import ApiError.ApiFailure

  def secure[I, R](e: Secured.SecuredEndpoint[I, R], required: Role)(
      logic: I => IO[Either[ApiFailure, R]],
  ): ServerEndpoint[Any, IO] =
    e.serverSecurityLogic(token => validator.authorize(token, required)).serverLogic((_: AuthedUser) => logic)

  /** Variant exposing the caller identity to the handler (audit logging, per-user behaviour). */
  def secureWithUser[I, R](e: Secured.SecuredEndpoint[I, R], required: Role)(
      logic: AuthedUser => I => IO[Either[ApiFailure, R]],
  ): ServerEndpoint[Any, IO] =
    e.serverSecurityLogic(token => validator.authorize(token, required)).serverLogic(logic)
end ApiSecurity
