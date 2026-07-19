package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskError }

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** JSON DTOs, tapir endpoints, and http4s server logic for the task catalog CRUD API. */
object TaskHttpApi:
  import ApiError.ApiFailure

  final case class TaskRequest(name: String, description: Option[String], requiredHours: Int):
    def toDomain(id: UUID): ValidatedNec[String, Task] = Task.createTask(id, name, description, requiredHours)

  object TaskRequest:
    val example: TaskRequest = TaskRequest("Cutting", Some("Cut raw material to size"), 8)

  final case class TaskResponse(id: UUID, name: String, description: Option[String], requiredHours: Int)

  object TaskResponse:
    def fromDomain(task: Task): TaskResponse = TaskResponse(task.id, task.name, task.taskDescription, task.requiredHours.value)

  private def conflict(error: TaskError): ApiFailure = error match
    case TaskError.TaskAlreadyExists => ApiError.conflict("task-already-exists", "A task with this id already exists.", Nil)
    case TaskError.TaskNotFound => ApiError.notFound("Task", "")

  given Codec[TaskRequest] = deriveCodec
  given Codec[TaskResponse] = deriveCodec
  given Schema[TaskRequest] = Schema.derived
  given Schema[TaskResponse] = Schema.derived

  private val collection = "tasks"

  val create: Secured.SecuredEndpoint[TaskRequest, TaskResponse] =
    Secured.base.post
      .in(collection)
      .tag("Tasks")
      .summary("Create a task")
      .in(jsonBody[TaskRequest].example(TaskRequest.example))
      .out(jsonBody[TaskResponse])

  val list: Secured.SecuredEndpoint[Unit, List[TaskResponse]] =
    Secured.base.get.in(collection).tag("Tasks").summary("List tasks").out(jsonBody[List[TaskResponse]])

  val read: Secured.SecuredEndpoint[UUID, TaskResponse] =
    Secured.base.get.in(collection / path[UUID]("id")).tag("Tasks").summary("Read a task").out(jsonBody[TaskResponse])

  val update: Secured.SecuredEndpoint[(UUID, TaskRequest), TaskResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Tasks")
      .summary("Replace a task")
      .in(jsonBody[TaskRequest].example(TaskRequest.example))
      .out(jsonBody[TaskResponse])

  val delete: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete.in(collection / path[UUID]("id")).tag("Tasks").summary("Delete a task").out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, delete)

  def routes(store: TaskApp.Store, manufacturings: ManufacturingApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Admin)(createLogic(store)),
    security.secure(list, Role.Viewer)(_ => listLogic(store)),
    security.secure(read, Role.Viewer)(readLogic(store)),
    security.secure(update, Role.Admin)(updateLogic(store)),
    security.secure(delete, Role.Admin)(deleteLogic(store, manufacturings)),
  )

  private def createLogic(store: TaskApp.Store)(request: TaskRequest): IO[Either[ApiFailure, TaskResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      request.toDomain(id).toEither match
        case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
        case Right(task) =>
          TaskApp.create(store, task).attempt.map {
            case Left(error) => ApiError.internal(error).asLeft
            case Right(Left(error)) => conflict(error).asLeft
            case Right(Right(())) => TaskResponse.fromDomain(task).asRight
          }
    }

  private def listLogic(store: TaskApp.Store): IO[Either[ApiFailure, List[TaskResponse]]] =
    TaskApp.list(store).attempt.map(_.bimap(ApiError.internal, _.map(TaskResponse.fromDomain)))

  private def readLogic(store: TaskApp.Store)(id: UUID): IO[Either[ApiFailure, TaskResponse]] =
    TaskApp.get(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Some(task)) => TaskResponse.fromDomain(task).asRight
      case Right(None) => ApiError.notFound("Task", id.toString).asLeft
    }

  private def updateLogic(store: TaskApp.Store)(id: UUID, request: TaskRequest): IO[Either[ApiFailure, TaskResponse]] =
    request.toDomain(id).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(task) =>
        TaskApp.update(store, task).attempt.map {
          case Left(error) => ApiError.internal(error).asLeft
          case Right(Left(TaskError.TaskNotFound)) => ApiError.notFound("Task", id.toString).asLeft
          case Right(Left(error)) => conflict(error).asLeft
          case Right(Right(())) => TaskResponse.fromDomain(task).asRight
        }

  private def deleteLogic(store: TaskApp.Store, manufacturings: ManufacturingApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    ManufacturingApp.findByTaskId(manufacturings, id).flatMap {
      case usedBy if usedBy.nonEmpty =>
        val details = usedBy.map(manufacturing => s"${manufacturing.code} - ${manufacturing.name}")
        IO.pure(ApiError.conflict("task-in-use", "Il task è usato da una o più lavorazioni a catalogo.", details).asLeft)
      case _ =>
        TaskApp.delete(store, id).attempt.map {
          case Left(error) => ApiError.internal(error).asLeft
          case Right(Left(TaskError.TaskNotFound)) => ApiError.notFound("Task", id.toString).asLeft
          case Right(Left(error)) => conflict(error).asLeft
          case Right(Right(())) => ().asRight
        }
    }
end TaskHttpApi
