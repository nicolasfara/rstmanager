package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.PlanningError
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.*
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.given
import io.github.nicolasfara.rstmanager.service.http.ApiError
import io.github.nicolasfara.rstmanager.service.http.ApiError.ApiFailure

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** Tapir endpoint descriptions for the planning API. */
object PlanningEndpoints:
  val health: PublicEndpoint[Unit, Unit, HealthResponse, Any] =
    endpoint.get
      .in("api" / "v1" / "health")
      .tag("System")
      .summary("Check planning API health")
      .out(jsonBody[HealthResponse])

  val createPlanningAttempt: PublicEndpoint[PlanningAttemptRequest, ApiFailure, PlanningAttemptResponse, Any] =
    ApiError.base.post
      .in("planning" / "attempts")
      .tag("Planning")
      .summary("Compute and persist a production planning attempt from stored orders and employees")
      .in(jsonBody[PlanningAttemptRequest].example(PlanningAttemptRequest.example))
      .out(jsonBody[PlanningAttemptResponse])

  val currentPlanningAttempt: PublicEndpoint[Unit, ApiFailure, PlanningStateDto, Any] =
    ApiError.base.get
      .in("planning" / "attempts" / "current")
      .tag("Planning")
      .summary("Read the current production planning aggregate state")
      .out(jsonBody[PlanningStateDto])

  def all: List[AnyEndpoint] = List(health, createPlanningAttempt, currentPlanningAttempt)
end PlanningEndpoints

/** HTTP server logic for the planning API. */
object PlanningRoutes:
  def serverEndpoints(
      backend: PlanningApp.PlanningBackend,
      gateway: PlanningEntityGateway,
  ): List[ServerEndpoint[Any, IO]] = List(
    PlanningEndpoints.health.serverLogicSuccess[IO](_ => IO.pure(HealthResponse("ok", "rstmanager-planning", "/docs"))),
    PlanningEndpoints.createPlanningAttempt.serverLogic(createPlanningAttempt(backend, gateway)),
    PlanningEndpoints.currentPlanningAttempt.serverLogic(_ => currentPlanningAttempt(backend)),
  )

  private def createPlanningAttempt(
      backend: PlanningApp.PlanningBackend,
      gateway: PlanningEntityGateway,
  )(request: PlanningAttemptRequest): IO[Either[ApiFailure, PlanningAttemptResponse]] =
    for
      nowAndRequestId <- IO((DateTime.now().nn, UUID.randomUUID().nn))
      response <- request.toDomain(nowAndRequestId._1, nowAndRequestId._2).toEither match
        case Left(errors) => IO.pure(validationFailure(errors).asLeft)
        case Right(input) => dispatchPlanningCommand(backend, gateway, input)
    yield response

  private def dispatchPlanningCommand(
      backend: PlanningApp.PlanningBackend,
      gateway: PlanningEntityGateway,
      input: PlanningAttemptInput,
  ): IO[Either[ApiFailure, PlanningAttemptResponse]] =
    gateway.snapshot(input.orderIds, input.employeeIds).flatMap {
      case Left(error) => IO.pure(entityLoadFailure(error).asLeft)
      case Right(snapshot) =>
        val planningRequest = input.toPlanningRequest(snapshot.orders.map(_.data.id))
        PlanningApp.computePlan(backend, planningRequest, snapshot.orders, snapshot.employees).attempt.flatMap {
          case Left(error) => IO.pure(unexpectedFailure(error).asLeft)
          case Right(Left(errors)) => IO.pure(commandRejected(errors).asLeft)
          case Right(Right(commandId)) => currentPlanningAttempt(backend).map(_.map(PlanningAttemptResponse(commandId, _)))
        }
    }

  private def currentPlanningAttempt(backend: PlanningApp.PlanningBackend): IO[Either[ApiFailure, PlanningStateDto]] =
    backend.repository.get(PlanningApp.planningAddress).attempt.map {
      case Left(error) => unexpectedFailure(error).asLeft
      case Right(aggregate) =>
        PlanningStateDto
          .fromAggregateState(aggregate)
          .leftMap(error => StatusCode.InternalServerError -> error)
    }

  private def validationFailure(errors: NonEmptyChain[String]): ApiFailure =
    StatusCode.UnprocessableEntity -> ApiError("invalid-planning-request", "The planning request is not valid.", errors.toChain.toList)

  private def entityLoadFailure(error: PlanningEntityGateway.LoadError): ApiFailure =
    error match
      case PlanningEntityGateway.LoadError.UnknownOrders(ids) =>
        StatusCode.UnprocessableEntity -> ApiError("unknown-orders", "Some referenced orders do not exist or are not open.", ids.map(_.toString))
      case PlanningEntityGateway.LoadError.UnknownEmployees(ids) =>
        StatusCode.UnprocessableEntity -> ApiError("unknown-employees", "Some referenced employees do not exist.", ids.map(_.toString))

  private def commandRejected(errors: NonEmptyChain[PlanningError]): ApiFailure =
    StatusCode.Conflict -> ApiError(
      "planning-command-rejected",
      "The planning command was rejected by the domain model.",
      errors.toChain.toList.map(PlanningDomainErrorDto.fromDomain(_).message),
    )

  private def unexpectedFailure(error: Throwable): ApiFailure =
    StatusCode.InternalServerError -> ApiError("internal-error", "Unexpected service error.", List(error.toString))
end PlanningRoutes
