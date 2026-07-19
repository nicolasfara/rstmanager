package io.github.nicolasfara.rstmanager.service.http

import cats.data.NonEmptyChain
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*

/** Uniform error payload returned by every REST endpoint. */
final case class ApiError(code: String, message: String, details: List[String])

object ApiError:
  /** A status code paired with the error body sent to the client. */
  type ApiFailure = (StatusCode, ApiError)

  given Codec[ApiError] = deriveCodec
  given Schema[ApiError] = Schema.derived

  /** Shared base endpoint under `/api/v1` returning [[ApiError]] on failure. */
  val base: PublicEndpoint[Unit, ApiFailure, Unit, Any] =
    endpoint.in("api" / "v1").errorOut(statusCode.and(jsonBody[ApiError]))

  def validation(errors: NonEmptyChain[String]): ApiFailure =
    StatusCode.UnprocessableEntity -> ApiError("invalid-request", "The request payload is not valid.", errors.toChain.toList)

  def notFound(what: String, id: String): ApiFailure =
    StatusCode.NotFound -> ApiError("not-found", s"$what $id was not found.", Nil)

  def conflict(code: String, message: String, details: List[String]): ApiFailure =
    StatusCode.Conflict -> ApiError(code, message, details)

  def internal(error: Throwable): ApiFailure =
    StatusCode.InternalServerError -> ApiError("internal-error", "Unexpected service error.", List(error.toString))
end ApiError
