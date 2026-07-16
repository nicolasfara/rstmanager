package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.http.ApiError
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ Manufacturing, ManufacturingDependencies, ManufacturingError }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies.*
import io.github.nicolasfara.rstmanager.work.domain.task.Task
import io.github.nicolasfara.rstmanager.work.service.TaskHttpApi.TaskResponse

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** JSON DTOs, tapir endpoints, and http4s server logic for the manufacturing catalog CRUD API. */
object ManufacturingHttpApi:
  import ApiError.ApiFailure
  import TaskHttpApi.given

  final case class ManufacturingDependencyDto(taskId: UUID, dependsOn: List[UUID])

  object ManufacturingDependencyDto:
    val example: ManufacturingDependencyDto =
      ManufacturingDependencyDto(
        UUID.fromString("00000000-0000-0000-0000-000000000302").nn,
        List(UUID.fromString("00000000-0000-0000-0000-000000000301").nn),
      )

  final case class ManufacturingRequest(
      code: String,
      name: String,
      description: Option[String],
      taskIds: List[UUID],
      dependencies: List[ManufacturingDependencyDto],
  ):
    def toDomain(id: UUID): ValidatedNec[String, Manufacturing] =
      val dependencyGraph = dependencies.foldLeft(ManufacturingDependencies()) { (current, dependency) =>
        current.addTaskDependencies(dependency.taskId, dependency.dependsOn.toSet)
      }
      Manufacturing.createManufacturing(id, code, name, description, taskIds, dependencyGraph)

  object ManufacturingRequest:
    val example: ManufacturingRequest =
      ManufacturingRequest(
        "MFG-2026-001",
        "Serramento standard",
        Some("Ciclo standard per serramenti in alluminio"),
        List(
          UUID.fromString("00000000-0000-0000-0000-000000000301").nn,
          UUID.fromString("00000000-0000-0000-0000-000000000302").nn,
        ),
        List(ManufacturingDependencyDto.example),
      )

  final case class ManufacturingResponse(
      id: UUID,
      code: String,
      name: String,
      description: Option[String],
      taskIds: List[UUID],
      tasks: List[TaskResponse],
      dependencies: List[ManufacturingDependencyDto],
      totalRequiredHours: Int,
  )

  object ManufacturingResponse:
    def fromDomain(manufacturing: Manufacturing, tasks: List[Task]): ManufacturingResponse =
      val taskResponses = tasks.map(TaskResponse.fromDomain)
      ManufacturingResponse(
        manufacturing.id,
        manufacturing.code,
        manufacturing.name,
        manufacturing.description,
        manufacturing.taskIds.toList,
        taskResponses,
        manufacturing.dependencies.toEdgePairs.groupMap(_._1)(_._2).toList.map((taskId, dependsOn) => ManufacturingDependencyDto(taskId, dependsOn)),
        taskResponses.map(_.requiredHours).sum,
      )

  private def conflict(error: ManufacturingError): ApiFailure = error match
    case ManufacturingError.ManufacturingAlreadyExists =>
      ApiError.conflict("manufacturing-already-exists", "A manufacturing with this id already exists.")
    case ManufacturingError.ManufacturingNotFound => ApiError.notFound("Manufacturing", "")

  given Codec[ManufacturingDependencyDto] = deriveCodec
  given Codec[ManufacturingRequest] = deriveCodec
  given Codec[ManufacturingResponse] = deriveCodec
  given Schema[ManufacturingDependencyDto] = Schema.derived
  given Schema[ManufacturingRequest] = Schema.derived
  given Schema[ManufacturingResponse] = Schema.derived

  private val collection = "manufacturings"

  val create: PublicEndpoint[ManufacturingRequest, ApiFailure, ManufacturingResponse, Any] =
    ApiError.base.post
      .in(collection)
      .tag("Manufacturings")
      .summary("Create a manufacturing catalog template")
      .in(jsonBody[ManufacturingRequest].example(ManufacturingRequest.example))
      .out(jsonBody[ManufacturingResponse])

  val list: PublicEndpoint[Unit, ApiFailure, List[ManufacturingResponse], Any] =
    ApiError.base.get.in(collection).tag("Manufacturings").summary("List manufacturing catalog templates").out(jsonBody[List[ManufacturingResponse]])

  val read: PublicEndpoint[UUID, ApiFailure, ManufacturingResponse, Any] =
    ApiError.base.get
      .in(collection / path[UUID]("id"))
      .tag("Manufacturings")
      .summary("Read a manufacturing template")
      .out(jsonBody[ManufacturingResponse])

  val update: PublicEndpoint[(UUID, ManufacturingRequest), ApiFailure, ManufacturingResponse, Any] =
    ApiError.base.put
      .in(collection / path[UUID]("id"))
      .tag("Manufacturings")
      .summary("Replace a manufacturing catalog template")
      .in(jsonBody[ManufacturingRequest].example(ManufacturingRequest.example))
      .out(jsonBody[ManufacturingResponse])

  val delete: PublicEndpoint[UUID, ApiFailure, Unit, Any] =
    ApiError.base.delete
      .in(collection / path[UUID]("id"))
      .tag("Manufacturings")
      .summary("Delete a manufacturing template")
      .out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, delete)

  def routes(store: ManufacturingApp.Store, tasks: TaskApp.Store): List[ServerEndpoint[Any, IO]] = List(
    create.serverLogic(createLogic(store, tasks)),
    list.serverLogic(_ => listLogic(store, tasks)),
    read.serverLogic(readLogic(store, tasks)),
    update.serverLogic(updateLogic(store, tasks)),
    delete.serverLogic(deleteLogic(store)),
  )

  private def createLogic(store: ManufacturingApp.Store, tasks: TaskApp.Store)(
      request: ManufacturingRequest,
  ): IO[Either[ApiFailure, ManufacturingResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      request.toDomain(id).toEither match
        case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
        case Right(manufacturing) =>
          validateTaskIds(tasks, manufacturing.taskIds.toList).flatMap {
            case Some(failure) => IO.pure(failure.asLeft)
            case None =>
              ManufacturingApp.create(store, manufacturing).attempt.flatMap {
                case Left(error) => IO.pure(ApiError.internal(error).asLeft)
                case Right(Left(error)) => IO.pure(conflict(error).asLeft)
                case Right(Right(())) => hydrate(tasks, manufacturing)
              }
          }
    }

  private def listLogic(store: ManufacturingApp.Store, tasks: TaskApp.Store): IO[Either[ApiFailure, List[ManufacturingResponse]]] =
    ManufacturingApp.list(store).attempt.flatMap {
      case Left(error) => IO.pure(ApiError.internal(error).asLeft)
      case Right(manufacturings) => manufacturings.traverse(hydrate(tasks, _)).map(_.sequence)
    }

  private def readLogic(store: ManufacturingApp.Store, tasks: TaskApp.Store)(id: UUID): IO[Either[ApiFailure, ManufacturingResponse]] =
    ManufacturingApp.get(store, id).attempt.flatMap {
      case Left(error) => IO.pure(ApiError.internal(error).asLeft)
      case Right(Some(manufacturing)) => hydrate(tasks, manufacturing)
      case Right(None) => IO.pure(ApiError.notFound("Manufacturing", id.toString).asLeft)
    }

  private def updateLogic(
      store: ManufacturingApp.Store,
      tasks: TaskApp.Store,
  )(id: UUID, request: ManufacturingRequest): IO[Either[ApiFailure, ManufacturingResponse]] =
    request.toDomain(id).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(manufacturing) =>
        validateTaskIds(tasks, manufacturing.taskIds.toList).flatMap {
          case Some(failure) => IO.pure(failure.asLeft)
          case None =>
            ManufacturingApp.update(store, manufacturing).attempt.flatMap {
              case Left(error) => IO.pure(ApiError.internal(error).asLeft)
              case Right(Left(ManufacturingError.ManufacturingNotFound)) => IO.pure(ApiError.notFound("Manufacturing", id.toString).asLeft)
              case Right(Left(error)) => IO.pure(conflict(error).asLeft)
              case Right(Right(())) => hydrate(tasks, manufacturing)
            }
        }

  private def deleteLogic(store: ManufacturingApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    ManufacturingApp.delete(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Left(ManufacturingError.ManufacturingNotFound)) => ApiError.notFound("Manufacturing", id.toString).asLeft
      case Right(Left(error)) => conflict(error).asLeft
      case Right(Right(())) => ().asRight
    }

  private def hydrate(tasks: TaskApp.Store, manufacturing: Manufacturing): IO[Either[ApiFailure, ManufacturingResponse]] =
    val ids = manufacturing.taskIds.toList
    ids.traverse(id => TaskApp.get(tasks, id).map(id -> _)).map { pairs =>
      val missing = pairs.collect { case (id, None) => id }
      if missing.nonEmpty then ApiError.notFound("Task", missing.mkString(", ")).asLeft
      else ManufacturingResponse.fromDomain(manufacturing, pairs.collect { case (_, Some(task)) => task }).asRight
    }

  private def validateTaskIds(tasks: TaskApp.Store, taskIds: List[UUID]): IO[Option[ApiFailure]] =
    taskIds.distinct.filterA(taskId => TaskApp.exists(tasks, taskId).map(!_)).map { missing =>
      if missing.nonEmpty then Some(ApiError.notFound("Task", missing.mkString(", "))) else None
    }
end ManufacturingHttpApi
