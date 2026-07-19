package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.service.CustomerApp
import io.github.nicolasfara.rstmanager.hr.service.EmployeeApp
import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
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
    ApiError.conflict("order-command-rejected", s"The order command was rejected: ${error.toString}.", Nil)

  private val collection = "orders"

  val create: Secured.SecuredEndpoint[OrderRequest, OrderResponse] =
    Secured.base.post
      .in(collection)
      .tag("Orders")
      .summary("Create an order")
      .in(jsonBody[OrderRequest].example(OrderRequest.example))
      .out(jsonBody[OrderResponse])

  val list: Secured.SecuredEndpoint[Unit, List[OrderResponse]] =
    Secured.base.get.in(collection).tag("Orders").summary("List orders").out(jsonBody[List[OrderResponse]])

  val read: Secured.SecuredEndpoint[UUID, OrderResponse] =
    Secured.base.get.in(collection / path[UUID]("id")).tag("Orders").summary("Read an order").out(jsonBody[OrderResponse])

  val update: Secured.SecuredEndpoint[(UUID, OrderUpdateRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Orders")
      .summary("Update an order priority and/or work-completion deadline")
      .in(jsonBody[OrderUpdateRequest].example(OrderUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val transition: Secured.SecuredEndpoint[(UUID, TransitionRequest), OrderResponse] =
    Secured.base.post
      .in(collection / path[UUID]("id") / "transitions")
      .tag("Orders")
      .summary("Apply an order lifecycle transition (suspend, reactivate, complete, deliver, reopen)")
      .in(jsonBody[TransitionRequest].example(TransitionRequest.example))
      .out(jsonBody[OrderResponse])

  val updateTask: Secured.SecuredEndpoint[(UUID, UUID, UUID, TaskProgressUpdateRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "tasks" / path[UUID]("taskId"))
      .tag("Orders")
      .summary("Update a scheduled task progress (completed hours) and/or its total expected hours")
      .in(jsonBody[TaskProgressUpdateRequest].example(TaskProgressUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val addManufacturing: Secured.SecuredEndpoint[(UUID, ManufacturingDto), OrderResponse] =
    Secured.base.post
      .in(collection / path[UUID]("id") / "manufacturings")
      .tag("Orders")
      .summary("Add a manufacturing to an order")
      .in(jsonBody[ManufacturingDto].example(ManufacturingDto.example))
      .out(jsonBody[OrderResponse])

  val removeManufacturing: Secured.SecuredEndpoint[(UUID, UUID), OrderResponse] =
    Secured.base.delete
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId"))
      .tag("Orders")
      .summary("Remove a manufacturing from an order")
      .out(jsonBody[OrderResponse])

  val updateManufacturing: Secured.SecuredEndpoint[(UUID, UUID, ManufacturingUpdateRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId"))
      .tag("Orders")
      .summary("Update a manufacturing description, work deadline and/or lifecycle status")
      .in(jsonBody[ManufacturingUpdateRequest].example(ManufacturingUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val addTask: Secured.SecuredEndpoint[(UUID, UUID, AddTaskRequest), OrderResponse] =
    Secured.base.post
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "tasks")
      .tag("Orders")
      .summary("Add a task to a manufacturing")
      .in(jsonBody[AddTaskRequest].example(AddTaskRequest.example))
      .out(jsonBody[OrderResponse])

  val removeTask: Secured.SecuredEndpoint[(UUID, UUID, UUID), OrderResponse] =
    Secured.base.delete
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "tasks" / path[UUID]("taskId"))
      .tag("Orders")
      .summary("Remove a task from a manufacturing")
      .out(jsonBody[OrderResponse])

  val updateDependencies: Secured.SecuredEndpoint[(UUID, OrderDependenciesUpdateRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id") / "dependencies")
      .tag("Orders")
      .summary("Replace the dependency graph between the order manufacturings")
      .in(jsonBody[OrderDependenciesUpdateRequest].example(OrderDependenciesUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val updateTaskDependencies: Secured.SecuredEndpoint[(UUID, UUID, TaskDependenciesUpdateRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "dependencies")
      .tag("Orders")
      .summary("Replace the task dependency graph of a manufacturing")
      .in(jsonBody[TaskDependenciesUpdateRequest].example(TaskDependenciesUpdateRequest.example))
      .out(jsonBody[OrderResponse])

  val setPreferredEmployee: Secured.SecuredEndpoint[(UUID, UUID, SetPreferredEmployeeRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "employee")
      .tag("Orders")
      .summary("Set or clear the preferred employee for a manufacturing")
      .in(jsonBody[SetPreferredEmployeeRequest].example(SetPreferredEmployeeRequest.example))
      .out(jsonBody[OrderResponse])

  val setTaskPreferredEmployee: Secured.SecuredEndpoint[(UUID, UUID, UUID, SetPreferredEmployeeRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "tasks" / path[UUID]("taskId") / "employee")
      .tag("Orders")
      .summary("Set or clear the preferred employee of a scheduled task")
      .in(jsonBody[SetPreferredEmployeeRequest].example(SetPreferredEmployeeRequest.example))
      .out(jsonBody[OrderResponse])

  val delete: Secured.SecuredEndpoint[(UUID, Option[String]), Unit] =
    Secured.base.delete
      .in(collection / path[UUID]("id"))
      .in(query[Option[String]]("reason"))
      .tag("Orders")
      .summary("Cancel and remove an order")
      .out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] =
    List(
      create,
      list,
      read,
      update,
      transition,
      updateTask,
      addManufacturing,
      removeManufacturing,
      updateManufacturing,
      updateDependencies,
      updateTaskDependencies,
      addTask,
      removeTask,
      setPreferredEmployee,
      setTaskPreferredEmployee,
      delete,
    )

  def routes(
      store: OrderApp.Store,
      customers: CustomerApp.Store,
      tasks: TaskApp.Store,
      employees: EmployeeApp.Store,
      security: ApiSecurity,
  ): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Operator)(createLogic(store, customers, tasks)),
    security.secure(list, Role.Viewer)(_ => listLogic(store)),
    security.secure(read, Role.Viewer)(readLogic(store)),
    security.secure(update, Role.Operator)(updateLogic(store)),
    security.secure(transition, Role.Operator)(transitionLogic(store)),
    security.secure(updateTask, Role.Operator)(updateTaskLogic(store)),
    security.secure(addManufacturing, Role.Operator)(addManufacturingLogic(store, tasks)),
    security.secure(removeManufacturing, Role.Operator)(removeManufacturingLogic(store)),
    security.secure(updateManufacturing, Role.Operator)(updateManufacturingLogic(store)),
    security.secure(updateDependencies, Role.Operator)(updateDependenciesLogic(store)),
    security.secure(updateTaskDependencies, Role.Operator)(updateTaskDependenciesLogic(store)),
    security.secure(addTask, Role.Operator)(addTaskLogic(store, tasks)),
    security.secure(removeTask, Role.Operator)(removeTaskLogic(store)),
    security.secure(setPreferredEmployee, Role.Operator)(setPreferredEmployeeLogic(store, employees)),
    security.secure(setTaskPreferredEmployee, Role.Operator)(setTaskPreferredEmployeeLogic(store, employees)),
    security.secure(delete, Role.Admin)(deleteLogic(store)),
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

  private def updateDependenciesLogic(
      store: OrderApp.Store,
  )(id: UUID, request: OrderDependenciesUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommand.toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(command) => runCommands(store, id, List(command))

  private def updateTaskDependenciesLogic(
      store: OrderApp.Store,
  )(id: UUID, manufacturingId: UUID, request: TaskDependenciesUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommand(manufacturingId).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(command) => runCommands(store, id, List(command))

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

  private def setPreferredEmployeeLogic(
      store: OrderApp.Store,
      employees: EmployeeApp.Store,
  )(id: UUID, manufacturingId: UUID, request: SetPreferredEmployeeRequest): IO[Either[ApiFailure, OrderResponse]] =
    withExistingEmployee(employees, request.employeeId) {
      runCommands(store, id, List(request.toCommand(manufacturingId)))
    }

  private def setTaskPreferredEmployeeLogic(
      store: OrderApp.Store,
      employees: EmployeeApp.Store,
  )(id: UUID, manufacturingId: UUID, taskId: UUID, request: SetPreferredEmployeeRequest): IO[Either[ApiFailure, OrderResponse]] =
    withExistingEmployee(employees, request.employeeId) {
      runCommands(store, id, List(request.toTaskCommand(manufacturingId, taskId)))
    }

  /** Runs `body` after verifying the referenced employee exists; a `None` employee (clearing the preference) needs no check. */
  private def withExistingEmployee(
      employees: EmployeeApp.Store,
      employeeId: Option[UUID],
  )(body: IO[Either[ApiFailure, OrderResponse]]): IO[Either[ApiFailure, OrderResponse]] =
    employeeId match
      case None => body
      case Some(empId) =>
        EmployeeApp.exists(employees, empId).flatMap {
          case false => IO.pure(ApiError.notFound("Employee", empId.toString).asLeft)
          case true => body
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
