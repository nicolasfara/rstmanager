package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.{ OrderSimulationService, PlanningError }
import io.github.nicolasfara.rstmanager.planning.OrderSimulationService.SimulationDemand
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.*
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.given
import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.service.http.ApiError.ApiFailure
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.service.{ ManufacturingApp, TaskApp }

import cats.data.{ NonEmptyChain, NonEmptyList }
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

  val createPlanningAttempt: Secured.SecuredEndpoint[PlanningAttemptRequest, PlanningAttemptResponse] =
    Secured.base.post
      .in("planning" / "attempts")
      .tag("Planning")
      .summary("Compute and persist a production planning attempt from stored orders and employees")
      .in(jsonBody[PlanningAttemptRequest].example(PlanningAttemptRequest.example))
      .out(jsonBody[PlanningAttemptResponse])

  val currentPlanningAttempt: Secured.SecuredEndpoint[Unit, PlanningStateDto] =
    Secured.base.get
      .in("planning" / "attempts" / "current")
      .tag("Planning")
      .summary("Read the current production planning aggregate state")
      .out(jsonBody[PlanningStateDto])

  val simulateOrder: Secured.SecuredEndpoint[OrderSimulationRequest, OrderSimulationResponse] =
    Secured.base.post
      .in("planning" / "simulate")
      .tag("Planning")
      .summary("Estimate the first feasible completion date of a hypothetical order without persisting anything")
      .in(jsonBody[OrderSimulationRequest].example(OrderSimulationRequest.example))
      .out(jsonBody[OrderSimulationResponse])

  def all: List[AnyEndpoint] = List(health, createPlanningAttempt, currentPlanningAttempt, simulateOrder)
end PlanningEndpoints

/** HTTP server logic for the planning API. */
object PlanningRoutes:
  def serverEndpoints(
      backend: PlanningApp.PlanningBackend,
      gateway: PlanningEntityGateway,
      manufacturings: ManufacturingApp.Store,
      tasks: TaskApp.Store,
      security: ApiSecurity,
  ): List[ServerEndpoint[Any, IO]] = List(
    PlanningEndpoints.health.serverLogicSuccess[IO](_ => IO.pure(HealthResponse("ok", "rstmanager-planning", "/docs"))),
    security.secure(PlanningEndpoints.createPlanningAttempt, Role.Operator)(createPlanningAttempt(backend, gateway)),
    security.secure(PlanningEndpoints.currentPlanningAttempt, Role.Viewer)(_ => currentPlanningAttempt(backend)),
    security.secure(PlanningEndpoints.simulateOrder, Role.Viewer)(simulateOrder(gateway, manufacturings, tasks)),
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

  private def simulateOrder(
      gateway: PlanningEntityGateway,
      manufacturings: ManufacturingApp.Store,
      tasks: TaskApp.Store,
  )(request: OrderSimulationRequest): IO[Either[ApiFailure, OrderSimulationResponse]] =
    resolveDemand(manufacturings, tasks, request).flatMap {
      case Left(failure) => IO.pure(failure.asLeft)
      case Right(demand) =>
        gateway.snapshot(None, None).flatMap {
          case Left(error) => IO.pure(entityLoadFailure(error).asLeft)
          case Right(snapshot) =>
            IO(DateTime.now().nn).map { now =>
              OrderSimulationService.simulate(now, snapshot.orders, snapshot.employees, demand) match
                case Left(errors) => simulationRejected(errors).asLeft
                case Right(result) => OrderSimulationResponse.fromDomain(result).asRight
            }
        }
    }

  /** Turns the request into a simulation demand, enforcing that exactly one of the two input modes is used. */
  private def resolveDemand(
      manufacturings: ManufacturingApp.Store,
      tasks: TaskApp.Store,
      request: OrderSimulationRequest,
  ): IO[Either[ApiFailure, SimulationDemand]] =
    (request.totalHours, request.manufacturingIds) match
      case (Some(hours), None) =>
        IO.pure(
          if hours < 1 then validationFailure(NonEmptyChain.one("totalHours must be at least 1.")).asLeft
          else SimulationDemand.TotalHours(TaskHours.applyUnsafe(hours)).asRight,
        )
      case (None, Some(ids)) =>
        NonEmptyList.fromList(ids) match
          case None => IO.pure(validationFailure(NonEmptyChain.one("manufacturingIds must contain at least one manufacturing.")).asLeft)
          case Some(selection) => resolveManufacturings(manufacturings, tasks, selection)
      case _ =>
        IO.pure(validationFailure(NonEmptyChain.one("Provide exactly one of totalHours or manufacturingIds.")).asLeft)

  /** Loads the selected catalog templates and their catalog tasks; duplicate ids are allowed and instantiate the template multiple times. */
  private def resolveManufacturings(
      manufacturings: ManufacturingApp.Store,
      tasks: TaskApp.Store,
      ids: NonEmptyList[UUID],
  ): IO[Either[ApiFailure, SimulationDemand]] =
    ids.traverse(id => ManufacturingApp.get(manufacturings, id).map(id -> _)).flatMap { loaded =>
      val missing = loaded.toList.collect { case (id, None) => id }.distinct
      if missing.nonEmpty then
        IO.pure(
          (StatusCode.UnprocessableEntity ->
            ApiError("unknown-manufacturings", "Some referenced catalog manufacturings do not exist.", missing.map(_.toString))).asLeft,
        )
      else
        val templates = loaded.map { case (_, template) => template }.toList.flatten
        templates.traverse(template => template.taskIds.traverse(taskId => TaskApp.get(tasks, taskId).map(taskId -> _)).map(template -> _)).map {
          resolved =>
            val missingTasks = resolved.flatMap { case (_, taskList) => taskList.toList }.collect { case (id, None) => id }.distinct
            if missingTasks.nonEmpty then
              (StatusCode.UnprocessableEntity ->
                ApiError(
                  "unknown-tasks",
                  "Some catalog tasks referenced by the selected manufacturings do not exist.",
                  missingTasks.map(_.toString),
                )).asLeft
            else
              val selection = resolved.map { case (template, taskList) =>
                template -> taskList.map { case (_, task) => task.getOrElse(sys.error("unreachable: missing tasks already rejected")) }
              }
              // `templates` mirrors the non-empty `ids`, so the selection is provably non-empty.
              SimulationDemand.FromManufacturings(NonEmptyList.fromListUnsafe(selection)).asRight
        }
      end if
    }

  private def simulationRejected(errors: NonEmptyList[PlanningError]): ApiFailure =
    StatusCode.Conflict -> ApiError(
      "planning-simulation-rejected",
      "The simulation could not be computed.",
      errors.toList.map(PlanningDomainErrorDto.fromDomain(_).message),
    )

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
