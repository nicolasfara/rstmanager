package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.service.CustomerApp
import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.hr.service.EmployeeApp
import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialId
import io.github.nicolasfara.rstmanager.warehouse.service.{ MaterialApp, WarehouseHttpSupport }
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp.StoredMaterial
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.{ CancellationReason, OrderData, OrderError, OrderId, OrderService }
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

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

  private def conflict(error: OrderError): ApiFailure = error match
    case OrderError.DuplicateMaterials(_, materialIds) =>
      ApiError.conflict("duplicate-materials", "Un materiale compare in più righe della lavorazione.", materialList(materialIds))
    case OrderError.NonPositiveMaterialQuantity(_, materialIds) =>
      ApiError.conflict("non-positive-material-quantity", "La quantità di ogni materiale deve essere maggiore di zero.", materialList(materialIds))
    case other => ApiError.conflict("order-command-rejected", s"The order command was rejected: ${other.toString}.", Nil)

  /** Like [[conflict]], saying in Italian why the materials of the manufacturing cannot change. */
  private def materialsConflict(error: OrderError): ApiFailure = error match
    case OrderError.OrderMustBeInProgressOrPaused =>
      ApiError.conflict(
        "order-not-editable",
        "I materiali si possono modificare solo per un ordine in corso o sospeso: riapri l'ordine per modificarli.",
        Nil,
      )
    case OrderError.ManufacturingNotFound(manufacturingId) => manufacturingNotFound(manufacturingId.value)
    case other => conflict(other)

  private def materialList(materialIds: Set[MaterialId]): List[String] = materialIds.toList.map(_.value.toString).sorted

  private def manufacturingNotFound(manufacturingId: UUID): ApiFailure =
    StatusCode.NotFound -> ApiError("not-found", "La lavorazione richiesta non esiste nell'ordine.", List(manufacturingId.toString))

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

  val setMaterials: Secured.SecuredEndpoint[(UUID, UUID, MaterialsUpdateRequest), OrderResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id") / "manufacturings" / path[UUID]("manufacturingId") / "materials")
      .tag("Orders")
      .summary("Replace the materials of a manufacturing (a missing unit price keeps the frozen one, or freezes the material's customer price)")
      .in(jsonBody[MaterialsUpdateRequest].example(MaterialsUpdateRequest.example))
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
      setMaterials,
      delete,
    )

  def routes(
      store: OrderApp.Store,
      customers: CustomerApp.Store,
      tasks: TaskApp.Store,
      employees: EmployeeApp.Store,
      materials: MaterialApp.Store,
      security: ApiSecurity,
  ): List[ServerEndpoint[Any, IO]] =
    val orders = Orders(store, materials)
    List(
      security.secure(create, Role.Operator)(createLogic(orders, customers, tasks)),
      security.secure(list, Role.Viewer)(_ => listLogic(orders)),
      security.secure(read, Role.Viewer)(respondWith(orders, _)),
      security.secure(update, Role.Operator)(updateLogic(orders)),
      security.secure(transition, Role.Operator)(transitionLogic(orders)),
      security.secure(updateTask, Role.Operator)(updateTaskLogic(orders)),
      security.secure(addManufacturing, Role.Operator)(addManufacturingLogic(orders, tasks)),
      security.secure(removeManufacturing, Role.Operator)(removeManufacturingLogic(orders)),
      security.secure(updateManufacturing, Role.Operator)(updateManufacturingLogic(orders)),
      security.secure(updateDependencies, Role.Operator)(updateDependenciesLogic(orders)),
      security.secure(updateTaskDependencies, Role.Operator)(updateTaskDependenciesLogic(orders)),
      security.secure(addTask, Role.Operator)(addTaskLogic(orders, tasks)),
      security.secure(removeTask, Role.Operator)(removeTaskLogic(orders)),
      security.secure(setPreferredEmployee, Role.Operator)(setPreferredEmployeeLogic(orders, employees)),
      security.secure(setTaskPreferredEmployee, Role.Operator)(setTaskPreferredEmployeeLogic(orders, employees)),
      security.secure(setMaterials, Role.Operator)(setMaterialsLogic(orders)),
      security.secure(delete, Role.Admin)(deleteLogic(orders)),
    )
  end routes

  /** The order backend plus the warehouse materials, whose master data the responses show and against which material lines are checked. */
  private final case class Orders(store: OrderApp.Store, materials: MaterialApp.Store)

  private def createLogic(
      orders: Orders,
      customers: CustomerApp.Store,
      tasks: TaskApp.Store,
  )(request: OrderRequest): IO[Either[ApiFailure, OrderResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      withCatalog(orders, request.materialIds) { catalog =>
        request.toDomain(id, catalog).toEither match
          case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
          case Right((data, promised)) =>
            validateReferences(customers, tasks, data).flatMap {
              case Some(failure) => IO.pure(failure.asLeft)
              case None =>
                OrderApp.create(orders.store, data, promised).attempt.flatMap {
                  case Left(error) => IO.pure(ApiError.internal(error).asLeft)
                  case Right(Left(error)) => IO.pure(conflict(error).asLeft)
                  case Right(Right(())) => respondWith(orders, id)
                }
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

  /** The warehouse materials with these ids, active or archived; ids of unknown materials are left out. */
  private def loadMaterials(materials: MaterialApp.Store, ids: List[MaterialId]): IO[Map[MaterialId, StoredMaterial]] =
    ids.distinct.traverse(id => MaterialApp.find(materials, id).map(_.map(id -> _))).map(_.flatten.toMap)

  /** Runs `body` with the catalog of the warehouse materials a request names. */
  private def withCatalog(orders: Orders, ids: List[MaterialId])(
      body: MaterialCatalog => IO[Either[ApiFailure, OrderResponse]],
  ): IO[Either[ApiFailure, OrderResponse]] =
    loadMaterials(orders.materials, ids).attempt.flatMap {
      case Left(error) => IO.pure(ApiError.internal(error).asLeft)
      case Right(found) => body(MaterialCatalog(found))
    }

  private def listLogic(orders: Orders): IO[Either[ApiFailure, List[OrderResponse]]] =
    val listed =
      for
        all <- OrderApp.list(orders.store)
        materials <- loadMaterials(orders.materials, all.flatMap(OrderResponse.materialIds))
      yield all.flatMap(OrderResponse.fromDomain(_, materials).toList)
    listed.attempt.map(_.leftMap(ApiError.internal))

  private def updateLogic(orders: Orders)(id: UUID, request: OrderUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommands.toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(commands) => runCommands(orders, id, commands)

  private def transitionLogic(orders: Orders)(id: UUID, request: TransitionRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommand.toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(command) => runCommands(orders, id, List(command))

  private def updateTaskLogic(
      orders: Orders,
  )(id: UUID, manufacturingId: UUID, taskId: UUID, request: TaskProgressUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommands(manufacturingId, taskId).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(commands) => runCommands(orders, id, commands)

  private def addManufacturingLogic(
      orders: Orders,
      tasks: TaskApp.Store,
  )(id: UUID, request: ManufacturingDto): IO[Either[ApiFailure, OrderResponse]] =
    IO(UUID.randomUUID().nn).flatMap { manufacturingId =>
      withCatalog(orders, request.materialIds) { catalog =>
        request.toDomain("manufacturing", manufacturingId, catalog).toEither match
          case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
          case Right(manufacturing) =>
            validateTaskIds(tasks, manufacturing.info.tasks.toList.map(_.taskId)).flatMap {
              case Some(failure) => IO.pure(failure.asLeft)
              case None => runCommands(orders, id, List(OrderService.Command.AddManufacturing(manufacturing)))
            }
      }
    }

  private def removeManufacturingLogic(orders: Orders)(id: UUID, manufacturingId: UUID): IO[Either[ApiFailure, OrderResponse]] =
    runCommands(orders, id, List(OrderService.Command.RemoveManufacturing(ScheduledManufacturingId(manufacturingId))))

  private def updateManufacturingLogic(
      orders: Orders,
  )(id: UUID, manufacturingId: UUID, request: ManufacturingUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommands(manufacturingId).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(commands) => runCommands(orders, id, commands)

  /**
   * Replaces the materials of a manufacturing. The lines are checked against the warehouse (see [[MaterialCatalog]]) and against the materials the
   * manufacturing already has: those may keep an archived material, and a line without a price keeps its frozen one.
   */
  private def setMaterialsLogic(
      orders: Orders,
  )(id: UUID, manufacturingId: UUID, request: MaterialsUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    OrderApp.get(orders.store, OrderId(id)).attempt.flatMap {
      case Left(error) => IO.pure(ApiError.internal(error).asLeft)
      case Right(None) => IO.pure(ApiError.notFound("Order", id.toString).asLeft)
      case Right(Some(order)) =>
        order.orderData.flatMap(_.setOfManufacturing.find(_.info.id == ScheduledManufacturingId(manufacturingId))) match
          case None => IO.pure(manufacturingNotFound(manufacturingId).asLeft)
          case Some(manufacturing) =>
            withCatalog(orders, request.materialIds) { catalog =>
              request.toCommand(manufacturingId, catalog, manufacturing.info.materials).toEither match
                case Left(errors) => IO.pure(WarehouseHttpSupport.validation(errors).asLeft)
                case Right(command) => runCommandsWith(orders, id, List(command), materialsConflict)
            }
    }

  private def updateDependenciesLogic(
      orders: Orders,
  )(id: UUID, request: OrderDependenciesUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommand.toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(command) => runCommands(orders, id, List(command))

  private def updateTaskDependenciesLogic(
      orders: Orders,
  )(id: UUID, manufacturingId: UUID, request: TaskDependenciesUpdateRequest): IO[Either[ApiFailure, OrderResponse]] =
    request.toCommand(manufacturingId).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(command) => runCommands(orders, id, List(command))

  private def addTaskLogic(
      orders: Orders,
      tasks: TaskApp.Store,
  )(id: UUID, manufacturingId: UUID, request: AddTaskRequest): IO[Either[ApiFailure, OrderResponse]] =
    validateTaskIds(tasks, (request.taskId :: request.dependsOn).map(TaskId(_))).flatMap {
      case Some(failure) => IO.pure(failure.asLeft)
      case None =>
        IO(UUID.randomUUID().nn).flatMap { taskInstanceId =>
          request.toCommand(manufacturingId, taskInstanceId).toEither match
            case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
            case Right(command) => runCommands(orders, id, List(command))
        }
    }

  private def removeTaskLogic(orders: Orders)(id: UUID, manufacturingId: UUID, taskId: UUID): IO[Either[ApiFailure, OrderResponse]] =
    runCommands(orders, id, List(OrderService.Command.RemoveManufacturingTask(ScheduledManufacturingId(manufacturingId), ScheduledTaskId(taskId))))

  /** Verifies every referenced catalog task id exists, returning a not-found failure listing the missing ones. */
  private def validateTaskIds(tasks: TaskApp.Store, taskIds: List[TaskId]): IO[Option[ApiFailure]] =
    taskIds.distinct.filterA(taskId => TaskApp.exists(tasks, taskId).map(!_)).map { missing =>
      if missing.nonEmpty then Some(ApiError.notFound("Task", missing.mkString(", "))) else None
    }

  private def setPreferredEmployeeLogic(
      orders: Orders,
      employees: EmployeeApp.Store,
  )(id: UUID, manufacturingId: UUID, request: SetPreferredEmployeeRequest): IO[Either[ApiFailure, OrderResponse]] =
    withExistingEmployee(employees, request.employeeId) {
      runCommands(orders, id, List(request.toCommand(manufacturingId)))
    }

  private def setTaskPreferredEmployeeLogic(
      orders: Orders,
      employees: EmployeeApp.Store,
  )(id: UUID, manufacturingId: UUID, taskId: UUID, request: SetPreferredEmployeeRequest): IO[Either[ApiFailure, OrderResponse]] =
    withExistingEmployee(employees, request.employeeId) {
      runCommands(orders, id, List(request.toTaskCommand(manufacturingId, taskId)))
    }

  /** Runs `body` after verifying the referenced employee exists; a `None` employee (clearing the preference) needs no check. */
  private def withExistingEmployee(
      employees: EmployeeApp.Store,
      employeeId: Option[UUID],
  )(body: IO[Either[ApiFailure, OrderResponse]]): IO[Either[ApiFailure, OrderResponse]] =
    employeeId match
      case None => body
      case Some(empId) =>
        EmployeeApp.exists(employees, EmployeeId(empId)).flatMap {
          case false => IO.pure(ApiError.notFound("Employee", empId.toString).asLeft)
          case true => body
        }

  private def deleteLogic(orders: Orders)(id: UUID, reason: Option[String]): IO[Either[ApiFailure, Unit]] =
    reason.traverse(_.refineValidatedNec[CancellationReason]).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(cancellationReason) =>
        OrderApp.delete(orders.store, OrderId(id), cancellationReason).attempt.map {
          case Left(error) => ApiError.internal(error).asLeft
          case Right(Left(OrderError.NoSuchOrder)) => ApiError.notFound("Order", id.toString).asLeft
          case Right(Left(error)) => conflict(error).asLeft
          case Right(Right(())) => ().asRight
        }

  /** Applies commands in order, stopping at the first rejection, then returns the resulting order. */
  private def runCommands(orders: Orders, id: UUID, commands: List[OrderService.Command]): IO[Either[ApiFailure, OrderResponse]] =
    runCommandsWith(orders, id, commands, conflict)

  /** Like [[runCommands]], answering a rejection with `rejected`. */
  private def runCommandsWith(
      orders: Orders,
      id: UUID,
      commands: List[OrderService.Command],
      rejected: OrderError => ApiFailure,
  ): IO[Either[ApiFailure, OrderResponse]] =
    val applied = commands.foldLeft(IO.pure(().asRight[ApiFailure])) { (acc, command) =>
      acc.flatMap {
        case Left(failure) => IO.pure(failure.asLeft)
        case Right(()) =>
          OrderApp.command(orders.store, OrderId(id), command).attempt.map {
            case Left(error) => ApiError.internal(error).asLeft
            case Right(Left(error)) => rejected(error).asLeft
            case Right(Right(())) => ().asRight
          }
      }
    }
    applied.flatMap {
      case Left(failure) => IO.pure(failure.asLeft)
      case Right(()) => respondWith(orders, id)
    }
  end runCommandsWith

  /** The current order, with the master data of the warehouse materials its materials name. */
  private def respondWith(orders: Orders, id: UUID): IO[Either[ApiFailure, OrderResponse]] =
    val loaded = OrderApp
      .get(orders.store, OrderId(id))
      .flatMap(_.traverse(order => loadMaterials(orders.materials, OrderResponse.materialIds(order)).tupleLeft(order)))
    loaded.attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Some((order, materials))) => OrderResponse.fromDomain(order, materials).toRight(ApiError.notFound("Order", id.toString))
      case Right(None) => ApiError.notFound("Order", id.toString).asLeft
    }
end OrderHttpApi
