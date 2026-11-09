package io.github.nicolasfara.rstmanager.hr.service

import java.util.{ Locale, UUID }

import io.github.nicolasfara.rstmanager.hr.domain.calendar.*
import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.codecs.CommonCodecs
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** JSON DTOs, tapir endpoints, and http4s server logic for the company calendar closures API. */
object CompanyCalendarHttpApi:
  import ApiError.ApiFailure

  /**
   * A closure as the client writes it, flat like the employee hours overrides: `kind` selects which fields are read.
   *   - `period`: `startDate` and `endDate`, both included; `month` and `day` are ignored.
   *   - `annual`: `month` (1-12) and `day` (valid for that month, 29/02 allowed); the dates are ignored.
   */
  final case class ClosureRequest(
      kind: String,
      reason: String,
      startDate: Option[String],
      endDate: Option[String],
      month: Option[Int],
      day: Option[Int],
  ):
    def toDomain(id: UUID): ValidatedNec[String, CompanyClosure] =
      normalizeKind(kind) match
        case "period" =>
          (
            required(startDate, "startDate").andThen(parseDate(_, "startDate")),
            required(endDate, "endDate").andThen(parseDate(_, "endDate")),
          ).mapN((_, _)).andThen { case (first, last) => CompanyClosure.createPeriod(ClosureId(id), first, last, reason) }
        case "annual" =>
          (required(month, "month"), required(day, "day")).mapN((_, _)).andThen { case (closureMonth, closureDay) =>
            CompanyClosure.createAnnual(ClosureId(id), closureMonth, closureDay, reason)
          }
        case other => s"kind '$other' is not supported. Use period or annual.".invalidNec
  end ClosureRequest

  object ClosureRequest:
    val example: ClosureRequest =
      ClosureRequest("period", "Chiusura estiva", Some("2026-08-10T00:00:00.000Z"), Some("2026-08-21T00:00:00.000Z"), None, None)

  final case class ClosureResponse(
      id: UUID,
      kind: String,
      reason: String,
      startDate: Option[String],
      endDate: Option[String],
      month: Option[Int],
      day: Option[Int],
  )

  object ClosureResponse:
    def fromDomain(closure: CompanyClosure): ClosureResponse = closure match
      case CompanyClosure.Period(id, firstDay, lastDay, reason) =>
        ClosureResponse(id.value, "period", reason, Some(formatDate(firstDay)), Some(formatDate(lastDay)), None, None)
      case CompanyClosure.Annual(id, month, dayOfMonth, reason) =>
        ClosureResponse(id.value, "annual", reason, None, None, Some(month), Some(dayOfMonth))

  private def failure(error: CompanyCalendarError, id: UUID): ApiFailure = error match
    case CompanyCalendarError.ClosureNotFound => ApiError.notFound("Closure", id.toString)
    case CompanyCalendarError.ClosureAlreadyExists => ApiError.conflict("closure-already-exists", "A closure with this id already exists.", Nil)
    case CompanyCalendarError.OverlappingClosures =>
      ApiError.conflict(
        "overlapping-closures",
        "The closure overlaps another one: two periods share a day, or two annual closures fall on the same day and month.",
        Nil,
      )

  private def normalizeKind(value: String): String = value.trim.nn.toLowerCase(Locale.ROOT).nn

  /** Accepts `YYYY-MM-DD` or an ISO-8601 date-time (as the frontend sends), keeping only its calendar date. */
  private def parseDate(value: String, path: String): ValidatedNec[String, LocalDate] =
    CommonCodecs.parseCalendarDay(value).leftMap(_ => s"$path must be an ISO-8601 date.").toValidatedNec

  private def required[A](value: Option[A], path: String): ValidatedNec[String, A] = value.toValidNec(s"$path is required.")

  /** Calendar days use the `YYYY-MM-DDT00:00:00.000Z` wire format of the employee API, so clients parse them the same way. */
  private def formatDate(value: LocalDate): String = s"${value}T00:00:00.000Z"

  given Codec[ClosureRequest] = deriveCodec
  given Codec[ClosureResponse] = deriveCodec
  given Schema[ClosureRequest] = Schema.derived
  given Schema[ClosureResponse] = Schema.derived

  private val collection = "company-calendar" / "closures"

  val create: Secured.SecuredEndpoint[ClosureRequest, ClosureResponse] =
    Secured.base.post
      .in(collection)
      .tag("Company calendar")
      .summary("Add a company closure")
      .in(jsonBody[ClosureRequest].example(ClosureRequest.example))
      .out(jsonBody[ClosureResponse])

  val list: Secured.SecuredEndpoint[Unit, List[ClosureResponse]] =
    Secured.base.get.in(collection).tag("Company calendar").summary("List company closures").out(jsonBody[List[ClosureResponse]])

  val update: Secured.SecuredEndpoint[(UUID, ClosureRequest), ClosureResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Company calendar")
      .summary("Replace a company closure")
      .in(jsonBody[ClosureRequest].example(ClosureRequest.example))
      .out(jsonBody[ClosureResponse])

  val delete: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete
      .in(collection / path[UUID]("id"))
      .tag("Company calendar")
      .summary("Remove a company closure")
      .out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, update, delete)

  def routes(store: CompanyCalendarApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Admin)(createLogic(store)),
    security.secure(list, Role.Viewer)(_ => listLogic(store)),
    security.secure(update, Role.Admin)(updateLogic(store)),
    security.secure(delete, Role.Admin)(deleteLogic(store)),
  )

  private def createLogic(store: CompanyCalendarApp.Store)(request: ClosureRequest): IO[Either[ApiFailure, ClosureResponse]] =
    IO(UUID.randomUUID().nn).flatMap(id => write(request.toDomain(id), id)(CompanyCalendarApp.add(store, _)))

  private def listLogic(store: CompanyCalendarApp.Store): IO[Either[ApiFailure, List[ClosureResponse]]] =
    CompanyCalendarApp.get(store).attempt.map(_.bimap(ApiError.internal, _.closures.map(ClosureResponse.fromDomain)))

  private def updateLogic(store: CompanyCalendarApp.Store)(id: UUID, request: ClosureRequest): IO[Either[ApiFailure, ClosureResponse]] =
    write(request.toDomain(id), id)(CompanyCalendarApp.update(store, _))

  private def deleteLogic(store: CompanyCalendarApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    CompanyCalendarApp.remove(store, ClosureId(id)).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Left(error)) => failure(error, id).asLeft
      case Right(Right(())) => ().asRight
    }

  /** Validates the closure, sends it with `command`, and answers with the stored closure. */
  private def write(closure: ValidatedNec[String, CompanyClosure], id: UUID)(
      command: CompanyClosure => IO[Either[CompanyCalendarError, Unit]],
  ): IO[Either[ApiFailure, ClosureResponse]] =
    closure.toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(valid) =>
        command(valid).attempt.map {
          case Left(error) => ApiError.internal(error).asLeft
          case Right(Left(error)) => failure(error, id).asLeft
          case Right(Right(())) => ClosureResponse.fromDomain(valid).asRight
        }
end CompanyCalendarHttpApi
