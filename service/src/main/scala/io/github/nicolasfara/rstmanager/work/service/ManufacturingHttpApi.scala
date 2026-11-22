package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialId
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp.StoredMaterial
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{
  Manufacturing,
  ManufacturingDependencies,
  ManufacturingError,
  ManufacturingId,
  ManufacturingMaterial,
}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskId }
import io.github.nicolasfara.rstmanager.work.service.OrderDtos.MaterialCatalog
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

  /** Default employee proposed for one task of the template when the manufacturing is scheduled inside an order. */
  final case class TaskDefaultEmployeeDto(taskId: UUID, employeeId: UUID)

  object TaskDefaultEmployeeDto:
    val example: TaskDefaultEmployeeDto =
      TaskDefaultEmployeeDto(
        UUID.fromString("00000000-0000-0000-0000-000000000301").nn,
        UUID.fromString("00000000-0000-0000-0000-000000000101").nn,
      )

  /**
   * Per-task override of the effort (in minutes) proposed when the template is scheduled inside an order; absent tasks fall back to the task-catalog
   * duration.
   */
  final case class TaskDurationOverrideDto(taskId: UUID, minutes: Int)

  object TaskDurationOverrideDto:
    val example: TaskDurationOverrideDto =
      TaskDurationOverrideDto(UUID.fromString("00000000-0000-0000-0000-000000000301").nn, 360)

  /**
   * A warehouse material consumed by **one** unit of the template: `quantity` is a decimal string with at most 3 decimals, in the material's unit of
   * measure (whole for pieces and packs).
   */
  final case class CatalogMaterialDto(materialId: UUID, quantity: String)

  object CatalogMaterialDto:
    val example: CatalogMaterialDto = CatalogMaterialDto(UUID.fromString("00000000-0000-0000-0000-000000000c01").nn, "2.500")

  /** A material of the template, with the material master data as the warehouse knows it now (none when it is not known). */
  final case class CatalogMaterialResponse(
      materialId: UUID,
      internalCode: Option[String],
      description: Option[String],
      unit: Option[String],
      archived: Boolean,
      quantity: String,
  )

  object CatalogMaterialResponse:
    def fromDomain(material: ManufacturingMaterial, materials: Map[MaterialId, StoredMaterial]): CatalogMaterialResponse =
      val stored = materials.get(material.materialId)
      CatalogMaterialResponse(
        material.materialId.value,
        stored.map(_.info.internalCode),
        stored.map(_.info.description),
        stored.map(_.info.unit.wireName),
        stored.exists(_.archived),
        material.quantity.toDecimalString,
      )

  final case class ManufacturingRequest(
      code: String,
      name: String,
      description: Option[String],
      taskIds: List[UUID],
      dependencies: List[ManufacturingDependencyDto],
      defaultEmployees: Option[List[TaskDefaultEmployeeDto]] = None,
      taskDurations: Option[List[TaskDurationOverrideDto]] = None,
      materials: Option[List[CatalogMaterialDto]] = None,
  ):
    /** The warehouse materials named by the material lines. */
    def materialIds: List[MaterialId] = materials.getOrElse(Nil).map(line => MaterialId(line.materialId)).distinct

    /**
     * The template, with its materials checked against the warehouse `catalog` (see [[MaterialCatalog.resolveQuantities]]): a material of the
     * `current` template may stay archived, a new one must be active. Absent `materials` keep the `current` ones as they are (none on a create); an
     * empty list clears them.
     */
    def toDomain(id: UUID, catalog: MaterialCatalog, current: List[ManufacturingMaterial]): ValidatedNec[String, Manufacturing] =
      val dependencyGraph = dependencies.foldLeft(ManufacturingDependencies()) { (current, dependency) =>
        current.addTaskDependencies(TaskId(dependency.taskId), dependency.dependsOn.map(TaskId(_)).toSet)
      }
      val resolvedMaterials = materials.fold(current.validNec[String]) { lines =>
        catalog
          .resolveQuantities("materials", lines.map(line => line.materialId -> line.quantity), current.map(_.materialId).toSet)
          .map(_.map(ManufacturingMaterial.apply))
      }
      resolvedMaterials.andThen { lines =>
        Manufacturing.createManufacturing(
          ManufacturingId(id),
          code,
          name,
          description,
          taskIds.map(TaskId(_)),
          dependencyGraph,
          defaultEmployees.getOrElse(Nil).map(entry => TaskId(entry.taskId) -> EmployeeId(entry.employeeId)).toMap,
          taskDurations.getOrElse(Nil).map(entry => TaskId(entry.taskId) -> entry.minutes).toMap,
          lines,
        )
      }
    end toDomain
  end ManufacturingRequest

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
        Some(List(TaskDefaultEmployeeDto.example)),
        Some(List(TaskDurationOverrideDto.example)),
        Some(List(CatalogMaterialDto.example)),
      )

  final case class ManufacturingResponse(
      id: UUID,
      code: String,
      name: String,
      description: Option[String],
      taskIds: List[UUID],
      tasks: List[TaskResponse],
      dependencies: List[ManufacturingDependencyDto],
      totalRequiredMinutes: Int,
      defaultEmployees: List[TaskDefaultEmployeeDto] = Nil,
      taskDurations: List[TaskDurationOverrideDto] = Nil,
      materials: List[CatalogMaterialResponse] = Nil,
  )

  object ManufacturingResponse:
    def fromDomain(manufacturing: Manufacturing, tasks: List[Task], materials: Map[MaterialId, StoredMaterial]): ManufacturingResponse =
      val taskResponses = tasks.map(TaskResponse.fromDomain)
      // Effective per-task minutes, applying the manufacturing's override where present, falling back to the task-catalog duration.
      val minutesByTask = tasks.map(task => task.id -> manufacturing.durationForTask(task.id, task.requiredDuration).value).toMap
      ManufacturingResponse(
        manufacturing.id.value,
        manufacturing.code,
        manufacturing.name,
        manufacturing.description,
        manufacturing.taskIds.toList.map(_.value),
        taskResponses,
        manufacturing.dependencies.toEdgePairs
          .groupMap(_._1)(_._2)
          .toList
          .map((taskId, dependsOn) => ManufacturingDependencyDto(taskId.value, dependsOn.map(_.value))),
        manufacturing.taskIds.toList.flatMap(minutesByTask.get).sum,
        manufacturing.taskIds.toList.flatMap(taskId =>
          manufacturing.defaultEmployees.get(taskId).map(employeeId => TaskDefaultEmployeeDto(taskId.value, employeeId.value)),
        ),
        manufacturing.taskIds.toList.flatMap(taskId =>
          manufacturing.taskDurations.get(taskId).map(duration => TaskDurationOverrideDto(taskId.value, duration.value)),
        ),
        manufacturing.materials.map(CatalogMaterialResponse.fromDomain(_, materials)),
      )
    end fromDomain
  end ManufacturingResponse

  private def conflict(error: ManufacturingError): ApiFailure = error match
    case ManufacturingError.ManufacturingAlreadyExists =>
      ApiError.conflict("manufacturing-already-exists", "A manufacturing with this id already exists.", Nil)
    case ManufacturingError.ManufacturingNotFound => ApiError.notFound("Manufacturing", "")
    case ManufacturingError.ManufacturingIdMismatch =>
      ApiError.conflict("manufacturing-id-mismatch", "The manufacturing id does not match the addressed manufacturing.", Nil)

  given Codec[ManufacturingDependencyDto] = deriveCodec
  given Codec[TaskDefaultEmployeeDto] = deriveCodec
  given Codec[TaskDurationOverrideDto] = deriveCodec
  given Codec[CatalogMaterialDto] = deriveCodec
  given Codec[CatalogMaterialResponse] = deriveCodec
  given Codec[ManufacturingRequest] = deriveCodec
  given Codec[ManufacturingResponse] = deriveCodec
  given Schema[ManufacturingDependencyDto] = Schema.derived
  given Schema[TaskDefaultEmployeeDto] = Schema.derived
  given Schema[TaskDurationOverrideDto] = Schema.derived
  given Schema[CatalogMaterialDto] = Schema.derived
  given Schema[CatalogMaterialResponse] = Schema.derived
  given Schema[ManufacturingRequest] = Schema.derived
  given Schema[ManufacturingResponse] = Schema.derived

  private val collection = "manufacturings"

  val create: Secured.SecuredEndpoint[ManufacturingRequest, ManufacturingResponse] =
    Secured.base.post
      .in(collection)
      .tag("Manufacturings")
      .summary("Create a manufacturing catalog template")
      .in(jsonBody[ManufacturingRequest].example(ManufacturingRequest.example))
      .out(jsonBody[ManufacturingResponse])

  val list: Secured.SecuredEndpoint[Unit, List[ManufacturingResponse]] =
    Secured.base.get.in(collection).tag("Manufacturings").summary("List manufacturing catalog templates").out(jsonBody[List[ManufacturingResponse]])

  val read: Secured.SecuredEndpoint[UUID, ManufacturingResponse] =
    Secured.base.get
      .in(collection / path[UUID]("id"))
      .tag("Manufacturings")
      .summary("Read a manufacturing template")
      .out(jsonBody[ManufacturingResponse])

  val update: Secured.SecuredEndpoint[(UUID, ManufacturingRequest), ManufacturingResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Manufacturings")
      .summary("Replace a manufacturing catalog template")
      .in(jsonBody[ManufacturingRequest].example(ManufacturingRequest.example))
      .out(jsonBody[ManufacturingResponse])

  val delete: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete
      .in(collection / path[UUID]("id"))
      .tag("Manufacturings")
      .summary("Delete a manufacturing template")
      .out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, delete)

  /** The catalog backend plus the tasks and warehouse materials the templates reference. */
  final case class Catalog(store: ManufacturingApp.Store, tasks: TaskApp.Store, materials: MaterialApp.Store)

  def routes(
      store: ManufacturingApp.Store,
      tasks: TaskApp.Store,
      materials: MaterialApp.Store,
      security: ApiSecurity,
  ): List[ServerEndpoint[Any, IO]] =
    val catalog = Catalog(store, tasks, materials)
    List(
      security.secure(create, Role.Admin)(createLogic(catalog)),
      security.secure(list, Role.Viewer)(_ => listLogic(catalog)),
      security.secure(read, Role.Viewer)(readLogic(catalog)),
      security.secure(update, Role.Admin)(updateLogic(catalog)),
      security.secure(delete, Role.Admin)(deleteLogic(store)),
    )

  private def createLogic(catalog: Catalog)(request: ManufacturingRequest): IO[Either[ApiFailure, ManufacturingResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      validated(catalog, request, id, Nil) { manufacturing =>
        ManufacturingApp.create(catalog.store, manufacturing).attempt.flatMap {
          case Left(error) => IO.pure(ApiError.internal(error).asLeft)
          case Right(Left(error)) => IO.pure(conflict(error).asLeft)
          case Right(Right(())) => hydrate(catalog, manufacturing)
        }
      }
    }

  private def listLogic(catalog: Catalog): IO[Either[ApiFailure, List[ManufacturingResponse]]] =
    ManufacturingApp.list(catalog.store).attempt.flatMap {
      case Left(error) => IO.pure(ApiError.internal(error).asLeft)
      case Right(manufacturings) =>
        loadMaterials(catalog, manufacturings.flatMap(_.materials.map(_.materialId))).flatMap {
          case Left(failure) => IO.pure(failure.asLeft)
          case Right(materials) => manufacturings.traverse(hydrateWith(catalog, _, materials)).map(_.sequence)
        }
    }

  private def readLogic(catalog: Catalog)(id: UUID): IO[Either[ApiFailure, ManufacturingResponse]] =
    ManufacturingApp.get(catalog.store, ManufacturingId(id)).attempt.flatMap {
      case Left(error) => IO.pure(ApiError.internal(error).asLeft)
      case Right(Some(manufacturing)) => hydrate(catalog, manufacturing)
      case Right(None) => IO.pure(ApiError.notFound("Manufacturing", id.toString).asLeft)
    }

  /** Replaces the template; materials it already has may stay archived (see [[ManufacturingRequest.toDomain]]). */
  private def updateLogic(catalog: Catalog)(id: UUID, request: ManufacturingRequest): IO[Either[ApiFailure, ManufacturingResponse]] =
    updateExisting(id, ManufacturingApp.get(catalog.store, ManufacturingId(id))) { current =>
      validated(catalog, request, id, current.materials) { manufacturing =>
        ManufacturingApp.update(catalog.store, manufacturing).attempt.flatMap {
          case Left(error) => IO.pure(ApiError.internal(error).asLeft)
          case Right(Left(ManufacturingError.ManufacturingNotFound)) => IO.pure(ApiError.notFound("Manufacturing", id.toString).asLeft)
          case Right(Left(error)) => IO.pure(conflict(error).asLeft)
          case Right(Right(())) => hydrate(catalog, manufacturing)
        }
      }
    }

  /**
   * Runs `replace` on the stored template `id` read by `load`: a missing template is a 404 at once, before the request is validated (so an unknown or
   * archived material in it never turns the 404 into a 400), and a failing read a 500.
   */
  private[service] def updateExisting[R](id: UUID, load: IO[Option[Manufacturing]])(
      replace: Manufacturing => IO[Either[ApiFailure, R]],
  ): IO[Either[ApiFailure, R]] =
    load.attempt.flatMap {
      case Left(error) => IO.pure(ApiError.internal(error).asLeft)
      case Right(None) => IO.pure(ApiError.notFound("Manufacturing", id.toString).asLeft)
      case Right(Some(current)) => replace(current)
    }

  private def deleteLogic(store: ManufacturingApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    ManufacturingApp.delete(store, ManufacturingId(id)).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Left(ManufacturingError.ManufacturingNotFound)) => ApiError.notFound("Manufacturing", id.toString).asLeft
      case Right(Left(error)) => conflict(error).asLeft
      case Right(Right(())) => ().asRight
    }

  /** Runs `body` with the template of `request`, once its fields, its materials (against the warehouse) and its task ids are valid. */
  private def validated(catalog: Catalog, request: ManufacturingRequest, id: UUID, current: List[ManufacturingMaterial])(
      body: Manufacturing => IO[Either[ApiFailure, ManufacturingResponse]],
  ): IO[Either[ApiFailure, ManufacturingResponse]] =
    loadMaterials(catalog, request.materialIds).flatMap {
      case Left(failure) => IO.pure(failure.asLeft)
      case Right(materials) =>
        request.toDomain(id, MaterialCatalog(materials), current).toEither match
          case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
          case Right(manufacturing) =>
            validateTaskIds(catalog.tasks, manufacturing.taskIds.toList).flatMap {
              case Some(failure) => IO.pure(failure.asLeft)
              case None => body(manufacturing)
            }
    }

  /** The warehouse materials with these ids, active or archived; ids of unknown materials are left out. */
  private def loadMaterials(catalog: Catalog, ids: List[MaterialId]): IO[Either[ApiFailure, Map[MaterialId, StoredMaterial]]] =
    if ids.isEmpty then IO.pure(Map.empty.asRight)
    else MaterialApp.findMany(catalog.materials, ids.distinct).attempt.map(_.leftMap(ApiError.internal))

  private def hydrate(catalog: Catalog, manufacturing: Manufacturing): IO[Either[ApiFailure, ManufacturingResponse]] =
    loadMaterials(catalog, manufacturing.materials.map(_.materialId)).flatMap {
      case Left(failure) => IO.pure(failure.asLeft)
      case Right(materials) => hydrateWith(catalog, manufacturing, materials)
    }

  private def hydrateWith(
      catalog: Catalog,
      manufacturing: Manufacturing,
      materials: Map[MaterialId, StoredMaterial],
  ): IO[Either[ApiFailure, ManufacturingResponse]] =
    val ids = manufacturing.taskIds.toList
    TaskApp.getMany(catalog.tasks, ids).map(found => ids.map(id => id -> found.get(id))).map { pairs =>
      val missing = pairs.collect { case (id, None) => id }
      if missing.nonEmpty then ApiError.notFound("Task", missing.mkString(", ")).asLeft
      else ManufacturingResponse.fromDomain(manufacturing, pairs.collect { case (_, Some(task)) => task }, materials).asRight
    }

  private def validateTaskIds(tasks: TaskApp.Store, taskIds: List[TaskId]): IO[Option[ApiFailure]] =
    TaskApp.getMany(tasks, taskIds.distinct).map(found => taskIds.distinct.filterNot(found.contains)).map { missing =>
      if missing.nonEmpty then Some(ApiError.notFound("Task", missing.mkString(", "))) else None
    }
end ManufacturingHttpApi
