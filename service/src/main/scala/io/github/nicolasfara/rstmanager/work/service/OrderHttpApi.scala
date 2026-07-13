package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.service.CustomerApp
import io.github.nicolasfara.rstmanager.service.http.ApiError
import io.github.nicolasfara.rstmanager.work.domain.order.{ CancellationReason, OrderData, OrderError, OrderService }

import cats.effect.IO
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** JSON DTOs, tapir endpoints, and http4s server logic for the order CRUD API, wired to the event-sourced order aggregate. */
object OrderHttpApi:
  import ApiError.ApiFailure
  import OrderDtos.*
  import OrderDtos.given

  private def conflict(error: OrderError): ApiFailure =
    ApiError.conflict("order-command-rejected", s"The order command was rejected: ${error.toString}.")

  private val collection = "orders"

  val create: PublicEndpoint[OrderRequest, ApiFailure, OrderResponse, Any] =
    ApiError.base.post
      .in(collection)
      .tag("Orders")
      .summary("Create an order")
      .in(jsonBody[OrderRequest].example(OrderRequest.example))
      .out(jsonBody[OrderResponse])

  val list: PublicEndpoint[Unit, ApiFailure, List[OrderResponse], Any] =
    ApiError.base.get.in(collection).tag("Orders").summary("List orders").out(jsonBody[List[OrderResponse]])

  val read: PublicEndpoint[UUID, ApiFailure, OrderResponse, Any] =
    ApiError.base.get.in(collection / path[UUID]("id")).tag("Orders").summary("Read an order").out(jsonBody[OrderResponse])

  val update: PublicEndpoint[(UUID, OrderUpdateRequest), ApiFailure, OrderResponse, Any] =
    ApiError.base.put
      .in(collection / path[UUID]("id"))
      .tag("Orders")
      .summary("Update an order priority and/or promised delivery date")
      .in(jsonBody[OrderUpdateRequest].example(OrderUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val transition: PublicEndpoint[(UUID, TransitionRequest), ApiFailure, OrderResponse, Any] =
    ApiError.base.post
      .in(collection / path[UUID]("id") / "transitions")
      .tag("Orders")
      .summary("Apply an order lifecycle transition (suspend, reactivate, complete, deliver, reopen)")
      .in(jsonBody[TransitionRequest].example(TransitionRequest.example))
      .out(jsonBody[OrderResponse])

  val updateTask: PublicEndpoint[(UUID, UUID, UUID, TaskProgressUpdateRequest), ApiFailure, OrderResponse, Any] =
    ApiError.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "tasks" / path[UUID]("taskId"))
      .tag("Orders")
      .summary("Update a scheduled task progress (completed hours) and/or its total expected hours")
      .in(jsonBody[TaskProgressUpdateRequest].example(TaskProgressUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val addManufacturing: PublicEndpoint[(UUID, ManufacturingDto), ApiFailure, OrderResponse, Any] =
    ApiError.base.post
      .in(collection / path[UUID]("id") / "manufacturings")
      .tag("Orders")
      .summary("Add a manufacturing to an order")
      .in(jsonBody[ManufacturingDto].example(ManufacturingDto.example))
      .out(jsonBody[OrderResponse])

  val removeManufacturing: PublicEndpoint[(UUID, UUID), ApiFailure, OrderResponse, Any] =
    ApiError.base.delete
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId"))
      .tag("Orders")
      .summary("Remove a manufacturing from an order")
      .out(jsonBody[OrderResponse])

  val updateManufacturing: PublicEndpoint[(UUID, UUID, ManufacturingUpdateRequest), ApiFailure, OrderResponse, Any] =
    ApiError.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId"))
      .tag("Orders")
      .summary("Update a manufacturing description and/or lifecycle status")
      .in(jsonBody[ManufacturingUpdateRequest].example(ManufacturingUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val addTask: PublicEndpoint[(UUID, UUID, AddTaskRequest), ApiFailure, OrderResponse, Any] =
    ApiError.base.post
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "tasks")
      .tag("Orders")
      .summary("Add a task to a manufacturing")
      .in(jsonBody[AddTaskRequest].example(AddTaskRequest.example))
      .out(jsonBody[OrderResponse])

  val removeTask: PublicEndpoint[(UUID, UUID, UUID), ApiFailure, OrderResponse, Any] =
    ApiError.base.delete
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "tasks" / path[UUID]("taskId"))
      .tag("Orders")
      .summary("Remove a task from a manufacturing")
      .out(jsonBody[OrderResponse])

  val delete: PublicEndpoint[(UUID, Option[String]), ApiFailure, Unit, Any] =
    ApiError.base.delete
      .in(collection / path[UUID]("id"))
      .in(query[Option[String]]("reason"))
      .tag("Orders")
      .summary("Cancel and remove an order")
      .out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] =
    List(create, list, read, update, transition, updateTask, addManufacturing, removeManufacturing, updateManufacturing, addTask, removeTask, delete)

  def routes(
      store: OrderApp.Store,
      customers: CustomerApp.Store,
      tasks: TaskApp.Store,
  ): List[ServerEndpoint[Any, IO]] = List(
    create.serverLogic(createLogic(store, customers, tasks)),
    list.serverLogic(_ => listLogic(store)),
    read.serverLogic(readLogic(store)),
    update.serverLogic(updateLogic(store)),
    transition.serverLogic(transitionLogic(store)),
    updateTask.serverLogic(updateTaskLogic(store)),
    addManufacturing.serverLogic(addManufacturingLogic(store, tasks)),
    removeManufacturing.serverLogic(removeManufacturingLogic(store)),
    updateManufacturing.serverLogic(updateManufacturingLogic(store)),
    addTask.serverLogic(addTaskLogic(store, tasks)),
    removeTask.serverLogic(removeTaskLogic(store)),
    delete.serverLogic(deleteLogic(store)),
  )

  private def createLogic(
      store: OrderApp.Store,
      customers: CustomerApp.Store,
      tasks: TaskApp.Store,
  )(request: OrderRequest): IO[Either[ApiFailure, OrderResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      request.toDomain(id).toEither match
        case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
        case Right((data, promised)) =>
          validateReferences(customers, tasks, data).flatMap {
            case Some(failure) => IO.pure(failure.asLeft)
            case None =>
              OrderApp.create(store, data, promised).attempt.flatMap {
                case Left(error) => IO.pure(ApiError.internal(error).asLeft)
                case Right(Left(error)) => IO.pure(conflict(error).asLeft)
                case Right(Right(())) => respondWith(store, id)
              }
          }
    }

  /** Verifies the referenced customer and catalog task ids exist before creating the order. */
  private def validateReferences(customers: CustomerApp.Store, tasks: TaskApp.Store, data: OrderData): IO[Option[ApiFailure]] =
    val taskIds = data.setOfManufacturing.toList.flatMap(_.info.tasks.toList.map(_.taskId)).distinct
    for
      customerOk <- CustomerApp.exists(customers, data.customerId)
      missingTasks <- taskIds.filterA(taskId => TaskApp.exists(tasks, taskId).map(!_))
    yield
      if !customerOk then Some(ApiError.notFound("Customer", data.customerId.toString))
      else if missingTasks.nonEmpty then Some(ApiError.notFound("Task", missingTasks.mkString(", ")))
      else None

  private def listLogic(store: OrderApp.Store): IO[Either[ApiFailure, List[OrderResponse]]] =
    OrderApp.list(store).attempt.map(_.bimap(ApiError.internal, _.flatMap(OrderResponse.fromDomain(_).toList)))

  private def readLogic(store: OrderApp.Store)(id: UUID): IO[Either[ApiFailure, OrderResponse]] =
    OrderApp.get(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Some(order)) => OrderResponse.fromDomain(order).toRight(ApiError.notFound("Order", id.toString))
      case Right(None) => ApiError.notFound("Order", id.toString).asLeft
    }

  private def updateLogic(store: OrderApp.Store)(id: UUID, request: OrderUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommands.toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(commands) => runCommands(store, id, commands)

  private def transitionLogic(store: OrderApp.Store)(id: UUID, request: TransitionRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommand.toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(command) => runCommands(store, id, List(command))

  private def updateTaskLogic(
      store: OrderApp.Store,
  )(id: UUID, manufacturingId: UUID, taskId: UUID, request: TaskProgressUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommands(manufacturingId, taskId).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(commands) => runCommands(store, id, commands)

  private def addManufacturingLogic(
      store: OrderApp.Store,
      tasks: TaskApp.Store,
  )(id: UUID, request: ManufacturingDto): IO[Either[ApiFailure, OrderResponse]] =
    IO(UUID.randomUUID().nn).flatMap { manufacturingId =>
      request.toDomain("manufacturing", manufacturingId).toEither match
        case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
        case Right(manufacturing) =>
          validateTaskIds(tasks, manufacturing.info.tasks.toList.map(_.taskId)).flatMap {
            case Some(failure) => IO.pure(failure.asLeft)
            case None => runCommands(store, id, List(OrderService.Command.AddManufacturing(manufacturing)))
          }
    }

  private def removeManufacturingLogic(store: OrderApp.Store)(id: UUID, manufacturingId: UUID): IO[Either[ApiFailure, OrderResponse]] =
    runCommands(store, id, List(OrderService.Command.RemoveManufacturing(manufacturingId)))

  private def updateManufacturingLogic(
      store: OrderApp.Store,
  )(id: UUID, manufacturingId: UUID, request: ManufacturingUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommands(manufacturingId).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(commands) => runCommands(store, id, commands)

  private def addTaskLogic(
      store: OrderApp.Store,
      tasks: TaskApp.Store,
  )(id: UUID, manufacturingId: UUID, request: AddTaskRequest): IO[Either[ApiFailure, OrderResponse]] =
    validateTaskIds(tasks, request.taskId :: request.dependsOn).flatMap {
      case Some(failure) => IO.pure(failure.asLeft)
      case None =>
        IO(UUID.randomUUID().nn).flatMap { taskInstanceId =>
          request.toCommand(manufacturingId, taskInstanceId).toEither match
            case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
            case Right(command) => runCommands(store, id, List(command))
        }
    }

  private def removeTaskLogic(store: OrderApp.Store)(id: UUID, manufacturingId: UUID, taskId: UUID): IO[Either[ApiFailure, OrderResponse]] =
    runCommands(store, id, List(OrderService.Command.RemoveManufacturingTask(manufacturingId, taskId)))

  /** Verifies every referenced catalog task id exists, returning a not-found failure listing the missing ones. */
  private def validateTaskIds(tasks: TaskApp.Store, taskIds: List[UUID]): IO[Option[ApiFailure]] =
    taskIds.distinct.filterA(taskId => TaskApp.exists(tasks, taskId).map(!_)).map { missing =>
      if missing.nonEmpty then Some(ApiError.notFound("Task", missing.mkString(", "))) else None
    }

  private def deleteLogic(store: OrderApp.Store)(id: UUID, reason: Option[String]): IO[Either[ApiFailure, Unit]] =
    reason.traverse(_.refineValidatedNec[CancellationReason]).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(cancellationReason) =>
        OrderApp.delete(store, id, cancellationReason).attempt.map {
          case Left(error) => ApiError.internal(error).asLeft
          case Right(Left(OrderError.NoSuchOrder)) => ApiError.notFound("Order", id.toString).asLeft
          case Right(Left(error)) => conflict(error).asLeft
          case Right(Right(())) => ().asRight
        }

  /** Applies commands in order, stopping at the first rejection, then returns the resulting order. */
  private def runCommands(
      store: OrderApp.Store,
      id: UUID,
      commands: List[OrderService.Command],
  ): IO[Either[ApiFailure, OrderResponse]] =
    val applied = commands.foldLeft(IO.pure(().asRight[ApiFailure])) { (acc, command) =>
      acc.flatMap {
        case Left(failure) => IO.pure(failure.asLeft)
        case Right(()) =>
          OrderApp.command(store, id, command).attempt.map {
            case Left(error) => ApiError.internal(error).asLeft
            case Right(Left(error)) => conflict(error).asLeft
            case Right(Right(())) => ().asRight
          }
      }
    }
    applied.flatMap {
      case Left(failure) => IO.pure(failure.asLeft)
      case Right(()) => respondWith(store, id)
    }
  end runCommands

  private def respondWith(store: OrderApp.Store, id: UUID): IO[Either[ApiFailure, OrderResponse]] =
    OrderApp.get(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Some(order)) => OrderResponse.fromDomain(order).toRight(ApiError.notFound("Order", id.toString))
      case Right(None) => ApiError.notFound("Order", id.toString).asLeft
    }
end OrderHttpApi
