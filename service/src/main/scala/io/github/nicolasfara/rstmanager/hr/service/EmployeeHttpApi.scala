package io.github.nicolasfara.rstmanager.hr.service

import java.util.{ Locale, UUID }

import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** JSON DTOs, tapir endpoints, and http4s server logic for the employee CRUD API. */
object EmployeeHttpApi:
  import ApiError.ApiFailure

  final case class EmployeeContractDto(kind: String, startDate: String, endDate: Option[String], weeklyHours: Option[Int]):
    def toDomain(path: String): ValidatedNec[String, Contract] =
      normalizeKind(kind) match
        case "full_time" | "fulltime" => parseDate(startDate, s"$path.startDate").map(Contract.createFullTime)
        case "fixed_term" | "fixedterm" =>
          (parseDate(startDate, s"$path.startDate"), required(endDate, s"$path.endDate").andThen(parseDate(_, s"$path.endDate")))
            .mapN((_, _))
            .andThen { case (start, end) => Contract.createFixedTerm(start, end) }
        case "part_time" | "parttime" =>
          (parseDate(startDate, s"$path.startDate"), required(weeklyHours, s"$path.weeklyHours"))
            .mapN((_, _))
            .andThen { case (start, hours) => Contract.createPartTime(start, hours) }
        case other => s"$path.kind '$other' is not supported. Use full_time, fixed_term, or part_time.".invalidNec

  object EmployeeContractDto:
    val example: EmployeeContractDto =
      EmployeeContractDto("full_time", "2026-01-01T00:00:00.000Z", None, None)

    def fromDomain(contract: Contract): EmployeeContractDto = contract match
      case Contract.FullTime(startDate) => EmployeeContractDto("full_time", formatDate(startDate), None, None)
      case Contract.FixedTerm(startDate, endDate) => EmployeeContractDto("fixed_term", formatDate(startDate), Some(formatDate(endDate)), None)
      case Contract.PartTime(startDate, weeklyHours) => EmployeeContractDto("part_time", formatDate(startDate), None, Some(weeklyHours.value))

  final case class HoursOverrideDto(
      kind: String,
      hours: Option[Int],
      reason: Option[String],
      day: Option[String],
      startDate: Option[String],
      endDate: Option[String],
  ):
    def toDomain(path: String): ValidatedNec[String, HoursOverride] =
      normalizeKind(kind) match
        case "working_day" | "workingday" =>
          (
            required(hours, s"$path.hours").andThen(DailyHours.validatedNec),
            reason.traverse(_.refineValidatedNec[OverrideReason]),
            required(day, s"$path.day").andThen(parseDate(_, s"$path.day")),
          ).mapN(WorkingDayOverride.apply)
        case "vacation" =>
          (
            required(startDate, s"$path.startDate").andThen(parseDate(_, s"$path.startDate")),
            required(endDate, s"$path.endDate").andThen(parseDate(_, s"$path.endDate")),
          ).mapN((_, _)).andThen { case (start, end) => VacationOverride.createVacationOverride(start, end) }
        case other => s"$path.kind '$other' is not supported. Use working_day or vacation.".invalidNec
  end HoursOverrideDto

  object HoursOverrideDto:
    val example: HoursOverrideDto =
      HoursOverrideDto("working_day", Some(6), Some("Reduced shift"), Some("2026-06-15T00:00:00.000Z"), None, None)

    def fromDomain(hoursOverride: HoursOverride): HoursOverrideDto = hoursOverride match
      case WorkingDayOverride(hours, reason, day) =>
        HoursOverrideDto("working_day", Some(hours.value), reason.map(r => r: String), Some(formatDate(day)), None, None)
      case VacationOverride(interval) =>
        HoursOverrideDto("vacation", None, None, None, Some(formatDate(interval.getStart.nn)), Some(formatDate(interval.getEnd.nn)))

  final case class EmployeeRequest(
      name: String,
      surname: String,
      contract: EmployeeContractDto,
      budgetWeeklyHours: Int,
      overrides: List[HoursOverrideDto],
  ):
    def toDomain(id: UUID): ValidatedNec[String, Employee] =
      (
        contract.toDomain("contract"),
        overrides.zipWithIndex.traverse { case (dto, index) => dto.toDomain(s"overrides[$index]") },
      ).mapN((_, _)).andThen { case (employmentContract, hoursOverrides) =>
        Employee.createEmployee(id, name, surname, employmentContract, budgetWeeklyHours, hoursOverrides)
      }

  object EmployeeRequest:
    val example: EmployeeRequest =
      EmployeeRequest("Mario", "Rossi", EmployeeContractDto.example, 40, List(HoursOverrideDto.example))

  final case class EmployeeResponse(
      id: UUID,
      name: String,
      surname: String,
      contract: EmployeeContractDto,
      budgetWeeklyHours: Int,
      overrides: List[HoursOverrideDto],
  )

  object EmployeeResponse:
    def fromDomain(employee: Employee): EmployeeResponse =
      EmployeeResponse(
        employee.id,
        employee.info.name,
        employee.info.surname,
        EmployeeContractDto.fromDomain(employee.contract),
        employee.budgetHours.default.value,
        employee.budgetHours.overrides.map(HoursOverrideDto.fromDomain),
      )

  private def conflict(error: EmployeeError): ApiFailure = error match
    case EmployeeError.EmployeeAlreadyExists => ApiError.conflict("employee-already-exists", "An employee with this id already exists.")
    case EmployeeError.EmployeeNotFound => ApiError.notFound("Employee", "")

  private def normalizeKind(value: String): String = value.trim.nn.toLowerCase(Locale.ROOT).nn.replace('-', '_').nn

  private def parseDate(value: String, path: String): ValidatedNec[String, DateTime] =
    Either.catchNonFatal(DateTime.parse(value).nn).leftMap(_ => s"$path must be an ISO-8601 date-time.").toValidatedNec

  private def required[A](value: Option[A], path: String): ValidatedNec[String, A] = value.toValidNec(s"$path is required.")

  private def formatDate(value: DateTime): String = value.toString

  given Codec[EmployeeContractDto] = deriveCodec
  given Codec[HoursOverrideDto] = deriveCodec
  given Codec[EmployeeRequest] = deriveCodec
  given Codec[EmployeeResponse] = deriveCodec
  given Schema[EmployeeContractDto] = Schema.derived
  given Schema[HoursOverrideDto] = Schema.derived
  given Schema[EmployeeRequest] = Schema.derived
  given Schema[EmployeeResponse] = Schema.derived

  private val collection = "employees"

  val create: Secured.SecuredEndpoint[EmployeeRequest, EmployeeResponse] =
    Secured.base.post
      .in(collection)
      .tag("Employees")
      .summary("Create an employee")
      .in(jsonBody[EmployeeRequest].example(EmployeeRequest.example))
      .out(jsonBody[EmployeeResponse])

  val list: Secured.SecuredEndpoint[Unit, List[EmployeeResponse]] =
    Secured.base.get.in(collection).tag("Employees").summary("List employees").out(jsonBody[List[EmployeeResponse]])

  val read: Secured.SecuredEndpoint[UUID, EmployeeResponse] =
    Secured.base.get.in(collection / path[UUID]("id")).tag("Employees").summary("Read an employee").out(jsonBody[EmployeeResponse])

  val update: Secured.SecuredEndpoint[(UUID, EmployeeRequest), EmployeeResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Employees")
      .summary("Replace an employee")
      .in(jsonBody[EmployeeRequest].example(EmployeeRequest.example))
      .out(jsonBody[EmployeeResponse])

  val delete: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete.in(collection / path[UUID]("id")).tag("Employees").summary("Delete an employee").out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, delete)

  def routes(store: EmployeeApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Admin)(createLogic(store)),
    security.secure(list, Role.Viewer)(_ => listLogic(store)),
    security.secure(read, Role.Viewer)(readLogic(store)),
    security.secure(update, Role.Admin)(updateLogic(store)),
    security.secure(delete, Role.Admin)(deleteLogic(store)),
  )

  private def createLogic(store: EmployeeApp.Store)(request: EmployeeRequest): IO[Either[ApiFailure, EmployeeResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      request.toDomain(id).toEither match
        case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
        case Right(employee) =>
          EmployeeApp.create(store, employee).attempt.map {
            case Left(error) => ApiError.internal(error).asLeft
            case Right(Left(error)) => conflict(error).asLeft
            case Right(Right(())) => EmployeeResponse.fromDomain(employee).asRight
          }
    }

  private def listLogic(store: EmployeeApp.Store): IO[Either[ApiFailure, List[EmployeeResponse]]] =
    EmployeeApp.list(store).attempt.map(_.bimap(ApiError.internal, _.map(EmployeeResponse.fromDomain)))

  private def readLogic(store: EmployeeApp.Store)(id: UUID): IO[Either[ApiFailure, EmployeeResponse]] =
    EmployeeApp.get(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Some(employee)) => EmployeeResponse.fromDomain(employee).asRight
      case Right(None) => ApiError.notFound("Employee", id.toString).asLeft
    }

  private def updateLogic(store: EmployeeApp.Store)(id: UUID, request: EmployeeRequest): IO[Either[ApiFailure, EmployeeResponse]] =
    request.toDomain(id).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(employee) =>
        EmployeeApp.update(store, employee).attempt.map {
          case Left(error) => ApiError.internal(error).asLeft
          case Right(Left(EmployeeError.EmployeeNotFound)) => ApiError.notFound("Employee", id.toString).asLeft
          case Right(Left(error)) => conflict(error).asLeft
          case Right(Right(())) => EmployeeResponse.fromDomain(employee).asRight
        }

  private def deleteLogic(store: EmployeeApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    EmployeeApp.delete(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Left(EmployeeError.EmployeeNotFound)) => ApiError.notFound("Employee", id.toString).asLeft
      case Right(Left(error)) => conflict(error).asLeft
      case Right(Right(())) => ().asRight
    }
end EmployeeHttpApi
