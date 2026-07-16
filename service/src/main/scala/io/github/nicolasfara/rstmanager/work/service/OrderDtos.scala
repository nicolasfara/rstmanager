package io.github.nicolasfara.rstmanager.work.service

import java.util.{ Locale, UUID }

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderDependencies.*
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.*

import cats.data.{ NonEmptyList, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import sttp.tapir.Schema

/** JSON DTOs and conversions for the order REST API (request graph adapted from the planning input DTOs). */
object OrderDtos:
  final case class TaskDependencyDto(taskId: UUID, dependsOn: List[UUID])

  object TaskDependencyDto:
    val example: TaskDependencyDto =
      TaskDependencyDto(
        UUID.fromString("00000000-0000-0000-0000-000000000302").nn,
        List(UUID.fromString("00000000-0000-0000-0000-000000000301").nn),
      )

  /** Dependency edges of one manufacturing towards the manufacturings that must be completed before it. */
  final case class ManufacturingDependencyDto(manufacturingId: UUID, dependsOn: List[UUID])

  object ManufacturingDependencyDto:
    val example: ManufacturingDependencyDto =
      ManufacturingDependencyDto(
        UUID.fromString("00000000-0000-0000-0000-000000000602").nn,
        List(UUID.fromString("00000000-0000-0000-0000-000000000601").nn),
      )

  /** Dependency edges between the manufacturings of an order creation request, referenced by their position in `manufacturings`. */
  final case class ManufacturingDependencyByIndexDto(manufacturingIndex: Int, dependsOnIndexes: List[Int])

  object ManufacturingDependencyByIndexDto:
    val example: ManufacturingDependencyByIndexDto = ManufacturingDependencyByIndexDto(1, List(0))

  final case class ScheduledTaskDto(
      id: UUID,
      taskId: UUID,
      status: String,
      expectedHours: Int,
      completedHours: Option[Int],
      completionDate: Option[String],
  ):
    def toDomain(path: String): ValidatedNec[String, ScheduledTask] =
      TaskHours.validatedNec(expectedHours).andThen { expected =>
        normalizeKind(status) match
          case "pending" => ScheduledTask.PendingTask(id, taskId, expected).validNec
          case "in_progress" | "inprogress" =>
            required(completedHours, s"$path.completedHours").andThen(TaskHours.validatedNec).map { completed =>
              ScheduledTask.InProgressTask(id, taskId, expected, completed)
            }
          case "completed" =>
            (
              required(completedHours, s"$path.completedHours").andThen(TaskHours.validatedNec),
              required(completionDate, s"$path.completionDate").andThen(parseDate(_, s"$path.completionDate")),
            ).mapN { (completed, completedOn) => ScheduledTask.CompletedTask(id, taskId, expected, completed, completedOn) }
          case other => s"$path.status '$other' is not supported. Use pending, in_progress, or completed.".invalidNec
      }
  end ScheduledTaskDto

  object ScheduledTaskDto:
    val example: ScheduledTaskDto =
      ScheduledTaskDto(
        UUID.fromString("00000000-0000-0000-0000-000000000401").nn,
        UUID.fromString("00000000-0000-0000-0000-000000000301").nn,
        "pending",
        8,
        Some(0),
        None,
      )

    def fromDomain(task: ScheduledTask): ScheduledTaskDto = task match
      case PendingTask(id, taskId, expected) => ScheduledTaskDto(id, taskId, "pending", expected.value, Some(0), None)
      case InProgressTask(id, taskId, expected, completed) => ScheduledTaskDto(id, taskId, "in_progress", expected.value, Some(completed.value), None)
      case CompletedTask(id, taskId, expected, completed, completionDate) =>
        ScheduledTaskDto(id, taskId, "completed", expected.value, Some(completed.value), Some(formatDate(completionDate)))

  final case class ManufacturingDto(
      code: String,
      completionDate: String,
      status: String,
      tasks: List[ScheduledTaskDto],
      dependencies: List[TaskDependencyDto],
      startedAt: Option[String],
      completedAt: Option[String],
      pausedAt: Option[String],
      pauseReason: Option[String],
      description: Option[String] = None,
      preferredEmployeeId: Option[UUID] = None,
  ):
    def toDomain(path: String, id: UUID): ValidatedNec[String, ScheduledManufacturing] =
      val taskList = tasks.zipWithIndex.traverse { case (task, index) => task.toDomain(s"$path.tasks[$index]") }
        .andThen(list => NonEmptyList.fromList(list).toValidNec(s"$path.tasks must contain at least one task"))

      val dependencyGraph = dependencies.foldLeft(ManufacturingDependencies()) { (current, dependency) =>
        current.addTaskDependencies(dependency.taskId, dependency.dependsOn.toSet)
      }

      val validatedDependencies = taskList.andThen { scheduledTasks =>
        val knownIds = scheduledTasks.toList.map(_.taskId).toSet
        val referencedIds = dependencyGraph.toEdgePairs.flatMap((source, target) => List(source, target)).toSet
        val unknownIds = referencedIds.filterNot(knownIds.contains)
        if unknownIds.nonEmpty then
          s"$path.dependencies references tasks not part of this manufacturing: ${unknownIds.toList.map(_.toString).sorted.mkString(", ")}".invalidNec
        else if dependencyGraph.hasCycle then s"$path.dependencies must not contain cycles.".invalidNec
        else dependencyGraph.validNec
      }

      val info = (manufacturingCode(code), parseDate(completionDate, s"$path.completionDate"), taskList, validatedDependencies).mapN {
        (code, expectedCompletionDate, scheduledTasks, taskDependencies) =>
          ScheduledManufacturingInfo(
            id,
            code,
            expectedCompletionDate,
            scheduledTasks,
            taskDependencies,
            description.map(_.trim.nn).filter(_.nonEmpty),
            preferredEmployeeId,
          )
      }

      info.andThen { manufacturingInfo =>
        normalizeKind(status) match
          case "not_started" | "notstarted" => ScheduledManufacturing.NotStartedManufacturing(manufacturingInfo).validNec
          case "in_progress" | "inprogress" =>
            required(startedAt, s"$path.startedAt").andThen(parseDate(_, s"$path.startedAt")).map { started =>
              ScheduledManufacturing.InProgressManufacturing(manufacturingInfo, started)
            }
          case "paused" =>
            (
              required(startedAt, s"$path.startedAt").andThen(parseDate(_, s"$path.startedAt")),
              required(pausedAt, s"$path.pausedAt").andThen(parseDate(_, s"$path.pausedAt")),
            ).mapN { (started, paused) => ScheduledManufacturing.PausedManufacturing(manufacturingInfo, pauseReason, started, paused) }
          case "completed" =>
            (
              required(startedAt, s"$path.startedAt").andThen(parseDate(_, s"$path.startedAt")),
              required(completedAt, s"$path.completedAt").andThen(parseDate(_, s"$path.completedAt")),
            ).mapN { (started, completed) => ScheduledManufacturing.CompletedManufacturing(manufacturingInfo, started, completed) }
          case other => s"$path.status '$other' is not supported. Use not_started, in_progress, paused, or completed.".invalidNec
      }
    end toDomain
  end ManufacturingDto

  object ManufacturingDto:
    val example: ManufacturingDto =
      ManufacturingDto(
        "MFG-2026-001",
        "2026-06-19T17:00:00.000Z",
        "not_started",
        List(
          ScheduledTaskDto.example,
          ScheduledTaskDto(
            UUID.fromString("00000000-0000-0000-0000-000000000402").nn,
            UUID.fromString("00000000-0000-0000-0000-000000000302").nn,
            "pending",
            12,
            Some(0),
            None,
          ),
        ),
        List(TaskDependencyDto.example),
        None,
        None,
        None,
        None,
        Some("Serramenti in alluminio per la commessa"),
      )

    def fromDomain(manufacturing: ScheduledManufacturing): ManufacturingResponse =
      val info = manufacturing.info
      val (status, startedAt, endedAt, reason) = manufacturing match
        case ScheduledManufacturing.NotStartedManufacturing(_) => ("not_started", None, None, None)
        case ScheduledManufacturing.InProgressManufacturing(_, started) => ("in_progress", Some(started), None, None)
        case ScheduledManufacturing.PausedManufacturing(_, pauseReason, started, paused) => ("paused", Some(started), Some(paused), pauseReason)
        case ScheduledManufacturing.CompletedManufacturing(_, started, completed) => ("completed", Some(started), Some(completed), None)
      ManufacturingResponse(
        info.id,
        info.code,
        formatDate(info.completionDate),
        status,
        startedAt.map(formatDate),
        endedAt.map(formatDate),
        reason,
        info.tasks.toList.map(ScheduledTaskDto.fromDomain),
        info.dependencies.toEdgePairs.groupMap(_._1)(_._2).toList.map((taskId, dependsOn) => TaskDependencyDto(taskId, dependsOn)),
        info.description,
        info.preferredEmployeeId,
      )
    end fromDomain
  end ManufacturingDto

  final case class OrderRequest(
      number: String,
      customerId: UUID,
      creationDate: String,
      deliveryDate: String,
      promisedDeliveryDate: String,
      priority: String,
      manufacturings: List[ManufacturingDto],
      description: Option[String] = None,
      dependencies: Option[List[ManufacturingDependencyByIndexDto]] = None,
  ):
    def toDomain(id: UUID): ValidatedNec[String, (OrderData, DateTime)] =
      toDomain(id, () => UUID.randomUUID().nn)

    def toDomain(id: UUID, nextManufacturingId: () => UUID): ValidatedNec[String, (OrderData, DateTime)] =
      val manufacturingIds = manufacturings.map(_ => nextManufacturingId())
      val manufacturingList = manufacturings
        .zip(manufacturingIds)
        .zipWithIndex
        .traverse { case ((manufacturing, manufacturingId), index) =>
          manufacturing.toDomain(s"manufacturings[$index]", manufacturingId)
        }
        .andThen(list => NonEmptyList.fromList(list).toValidNec("manufacturings must contain at least one manufacturing"))

      (
        number.refineValidatedNec[OrderNumber],
        parseDate(creationDate, "creationDate"),
        parseDate(deliveryDate, "deliveryDate"),
        parseDate(promisedDeliveryDate, "promisedDeliveryDate"),
        priorityToDomain(priority, "priority"),
        manufacturingList,
        dependencyGraph(manufacturingIds),
      ).mapN { (orderNumber, createdAt, expectedDelivery, promisedDelivery, orderPriority, manufacturingNel, manufacturingDependencies) =>
        (
          OrderData(
            id,
            orderNumber,
            customerId,
            createdAt,
            expectedDelivery,
            orderPriority,
            manufacturingNel,
            description.map(_.trim.nn).filter(_.nonEmpty),
            manufacturingDependencies,
          ),
          promisedDelivery,
        )
      }
    end toDomain

    /** Resolves the index-based dependency entries against the generated manufacturing ids and validates the resulting graph. */
    private def dependencyGraph(manufacturingIds: List[UUID]): ValidatedNec[String, OrderDependencies] =
      val count = manufacturingIds.size
      def validIndex(value: Int, path: String): ValidatedNec[String, Int] =
        if value >= 0 && value < count then value.validNec
        else s"$path must be a manufacturing index between 0 and ${count - 1}.".invalidNec

      dependencies
        .getOrElse(Nil)
        .zipWithIndex
        .traverse { case (dependency, index) =>
          val path = s"dependencies[$index]"
          (
            validIndex(dependency.manufacturingIndex, s"$path.manufacturingIndex"),
            dependency.dependsOnIndexes.zipWithIndex.traverse { case (dependsOnIndex, position) =>
              validIndex(dependsOnIndex, s"$path.dependsOnIndexes[$position]")
            },
          ).mapN { (sourceIndex, dependsOnIndexes) =>
            (manufacturingIds(sourceIndex), dependsOnIndexes.map(manufacturingIds(_)))
          }
        }
        .andThen { resolvedEdges =>
          val graph = resolvedEdges.foldLeft(OrderDependencies.empty) { case (current, (source, dependsOn)) =>
            current.addManufacturingDependencies(source, dependsOn.toSet)
          }
          if graph.hasCycle then "dependencies must not contain cycles.".invalidNec
          else graph.validNec
        }
    end dependencyGraph
  end OrderRequest

  object OrderRequest:
    val example: OrderRequest =
      OrderRequest(
        "ORD-2026-001",
        UUID.fromString("00000000-0000-0000-0000-000000000201").nn,
        "2026-06-14T09:00:00.000Z",
        "2026-06-22T17:00:00.000Z",
        "2026-06-22T17:00:00.000Z",
        "urgent",
        List(ManufacturingDto.example),
      )

  final case class OrderUpdateRequest(
      priority: Option[String],
      promisedDeliveryDate: Option[String],
      description: Option[String] = None,
  ):
    def toCommands: ValidatedNec[String, List[OrderService.Command]] =
      (
        priority.traverse(priorityToDomain(_, "priority")),
        promisedDeliveryDate.traverse(parseDate(_, "promisedDeliveryDate")),
      ).mapN { (parsedPriority, parsedDate) =>
        parsedPriority.map(OrderService.Command.ChangePriority.apply).toList ++
          parsedDate.map(OrderService.Command.UpdatePromisedDeliveryDate.apply).toList ++
          description.map(d => OrderService.Command.ChangeDescription(normalizeText(d))).toList
      }

  object OrderUpdateRequest:
    val example: OrderUpdateRequest =
      OrderUpdateRequest(Some("urgent"), Some("2026-06-24T17:00:00.000Z"), Some("Priorità rivista dopo il sopralluogo"))

  /** Recalibration of a manufacturing: its description, work deadline and/or lifecycle status. */
  final case class ManufacturingUpdateRequest(
      description: Option[String],
      completionDate: Option[String],
      status: Option[String],
      reason: Option[String],
  ):
    def toCommands(manufacturingId: UUID): ValidatedNec[String, List[OrderService.Command]] =
      (
        completionDate.traverse(parseDate(_, "completionDate")),
        status.traverse(manufacturingStatusToDomain),
      ).mapN { (parsedCompletionDate, parsedStatus) =>
        description.map(d => OrderService.Command.ChangeManufacturingDescription(manufacturingId, normalizeText(d))).toList ++
          parsedCompletionDate.map(date => OrderService.Command.ChangeManufacturingCompletionDate(manufacturingId, date)).toList ++
          parsedStatus.map(s => OrderService.Command.ChangeManufacturingStatus(manufacturingId, s, reason.flatMap(r => normalizeText(r)))).toList
      }

  object ManufacturingUpdateRequest:
    val example: ManufacturingUpdateRequest =
      ManufacturingUpdateRequest(Some("Serramenti in alluminio"), Some("2026-06-20T17:00:00.000Z"), Some("in_progress"), None)

  /** Replaces the dependency graph between the order manufacturings. */
  final case class OrderDependenciesUpdateRequest(dependencies: List[ManufacturingDependencyDto]):
    def toCommand: ValidatedNec[String, OrderService.Command] =
      val graph = dependencies.foldLeft(OrderDependencies.empty) { (current, dependency) =>
        current.addManufacturingDependencies(dependency.manufacturingId, dependency.dependsOn.toSet)
      }
      if graph.hasCycle then "dependencies must not contain cycles.".invalidNec
      else OrderService.Command.ChangeManufacturingDependencies(graph).validNec

  object OrderDependenciesUpdateRequest:
    val example: OrderDependenciesUpdateRequest = OrderDependenciesUpdateRequest(List(ManufacturingDependencyDto.example))

  /** Replaces the task dependency graph of a manufacturing. */
  final case class TaskDependenciesUpdateRequest(dependencies: List[TaskDependencyDto]):
    def toCommand(manufacturingId: UUID): ValidatedNec[String, OrderService.Command] =
      val graph = dependencies.foldLeft(ManufacturingDependencies()) { (current, dependency) =>
        current.addTaskDependencies(dependency.taskId, dependency.dependsOn.toSet)
      }
      if graph.hasCycle then "dependencies must not contain cycles.".invalidNec
      else OrderService.Command.ChangeTaskDependencies(manufacturingId, graph).validNec

  object TaskDependenciesUpdateRequest:
    val example: TaskDependenciesUpdateRequest = TaskDependenciesUpdateRequest(List(TaskDependencyDto.example))

  /** Adds a new scheduled task (referencing a catalog task) to a manufacturing. */
  final case class AddTaskRequest(taskId: UUID, expectedHours: Int, dependsOn: List[UUID]):
    def toCommand(manufacturingId: UUID, taskInstanceId: UUID): ValidatedNec[String, OrderService.Command] =
      ScheduledTask.createScheduledTask(taskInstanceId, taskId, expectedHours).map { task =>
        OrderService.Command.AddManufacturingTask(manufacturingId, task, dependsOn)
      }

  object AddTaskRequest:
    val example: AddTaskRequest =
      AddTaskRequest(UUID.fromString("00000000-0000-0000-0000-000000000301").nn, 8, Nil)

  final case class TransitionRequest(action: String, reason: Option[String]):
    def toCommand: ValidatedNec[String, OrderService.Command] =
      normalizeKind(action) match
        case "suspend" => reason.traverse(_.refineValidatedNec[SuspensionReason]).map(OrderService.Command.Suspend.apply)
        case "reactivate" => OrderService.Command.Reactivate.validNec
        case "complete" => OrderService.Command.Complete.validNec
        case "deliver" => OrderService.Command.Deliver.validNec
        case "reopen" => OrderService.Command.Reopen.validNec
        case other => s"action '$other' is not supported. Use suspend, reactivate, complete, deliver, or reopen.".invalidNec

  object TransitionRequest:
    val example: TransitionRequest = TransitionRequest("suspend", Some("Waiting for material"))

  /** Recalibration of a single scheduled task instance: its completed hours (progress) and/or its total expected hours. */
  final case class TaskProgressUpdateRequest(completedHours: Option[Int], expectedHours: Option[Int]):
    def toCommands(manufacturingId: UUID, taskId: UUID): ValidatedNec[String, List[OrderService.Command]] =
      (
        expectedHours.traverse(validateExpectedHours),
        completedHours.traverse(h => TaskHours.validatedNec(h).leftMap(_.map(m => s"completedHours $m"))),
      ).mapN { (expected, completed) =>
        // Apply the new total first so the progress is re-derived against it.
        expected.map(OrderService.Command.ChangeTaskExpectedHours(manufacturingId, taskId, _)).toList ++
          completed.map(OrderService.Command.SetTaskProgress(manufacturingId, taskId, _)).toList
      }

    private def validateExpectedHours(hours: Int): ValidatedNec[String, TaskHours] =
      if hours < 1 then "expectedHours must be at least 1.".invalidNec
      else TaskHours.validatedNec(hours).leftMap(_.map(m => s"expectedHours $m"))

  object TaskProgressUpdateRequest:
    val example: TaskProgressUpdateRequest = TaskProgressUpdateRequest(Some(6), Some(12))

  final case class ManufacturingResponse(
      id: UUID,
      code: String,
      completionDate: String,
      status: String,
      startedAt: Option[String],
      endedAt: Option[String],
      pauseReason: Option[String],
      tasks: List[ScheduledTaskDto],
      dependencies: List[TaskDependencyDto],
      description: Option[String],
      preferredEmployeeId: Option[UUID],
  )

  final case class SetPreferredEmployeeRequest(employeeId: Option[UUID]):
    def toCommand(manufacturingId: UUID): OrderService.Command =
      OrderService.Command.SetPreferredEmployee(manufacturingId, employeeId)

  object SetPreferredEmployeeRequest:
    val example: SetPreferredEmployeeRequest = SetPreferredEmployeeRequest(None)

  final case class OrderResponse(
      id: UUID,
      number: String,
      customerId: UUID,
      status: String,
      priority: String,
      creationDate: String,
      deliveryDate: String,
      promisedDeliveryDate: Option[String],
      manufacturings: List[ManufacturingResponse],
      description: Option[String],
      dependencies: List[ManufacturingDependencyDto],
  )

  object OrderResponse:
    def fromDomain(order: Order): Option[OrderResponse] =
      dataAndStatus(order).map { case (data, status, promised) =>
        OrderResponse(
          data.id,
          data.number,
          data.customerId,
          status,
          data.priority.toString.toLowerCase(Locale.ROOT).nn,
          formatDate(data.creationDate),
          formatDate(data.deliveryDate),
          promised.map(formatDate),
          data.setOfManufacturing.toList.map(ManufacturingDto.fromDomain),
          data.description,
          data.dependencies.toEdgePairs
            .groupMap(_._1)(_._2)
            .toList
            .map((manufacturingId, dependsOn) => ManufacturingDependencyDto(manufacturingId, dependsOn)),
        )
      }

    private def dataAndStatus(order: Order): Option[(OrderData, String, Option[DateTime])] = order match
      case Order.NewOrder => None
      case Order.InProgressOrder(data, promised) => Some((data, "in_progress", Some(promised)))
      case Order.SuspendedOrder(data, promised, _, _) => Some((data, "suspended", Some(promised)))
      case Order.CompletedOrder(data, _) => Some((data, "completed", None))
      case Order.DeliveredOrder(data, _, _) => Some((data, "delivered", None))
      case Order.CancelledOrder(data, _, _) => Some((data, "cancelled", None))
  end OrderResponse

  private def priorityToDomain(value: String, path: String): ValidatedNec[String, OrderPriority] =
    normalizeKind(value) match
      case "normal" => OrderPriority.Normal.validNec
      case "urgent" => OrderPriority.Urgent.validNec
      case other => s"$path '$other' is not supported. Use normal or urgent.".invalidNec

  private def manufacturingStatusToDomain(value: String): ValidatedNec[String, ManufacturingStatus] =
    normalizeKind(value) match
      case "not_started" | "notstarted" => ManufacturingStatus.NotStarted.validNec
      case "in_progress" | "inprogress" => ManufacturingStatus.InProgress.validNec
      case "paused" => ManufacturingStatus.Paused.validNec
      case "completed" => ManufacturingStatus.Completed.validNec
      case other => s"status '$other' is not supported. Use not_started, in_progress, paused, or completed.".invalidNec

  /** Trims free text and collapses blank strings to `None`, so an empty value clears the field. */
  private def normalizeText(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  private def manufacturingCode(value: String): ValidatedNec[String, String :| ManufacturingCode] =
    value.refineValidatedNec[ManufacturingCode]

  private def parseDate(value: String, path: String): ValidatedNec[String, DateTime] =
    Either.catchNonFatal(DateTime.parse(value).nn).leftMap(_ => s"$path must be an ISO-8601 date-time.").toValidatedNec

  private def required[A](value: Option[A], path: String): ValidatedNec[String, A] = value.toValidNec(s"$path is required.")

  private def normalizeKind(value: String): String = value.trim.nn.toLowerCase(Locale.ROOT).nn.replace('-', '_').nn

  private def formatDate(value: DateTime): String = value.toString

  given Codec[TaskDependencyDto] = deriveCodec
  given Codec[ManufacturingDependencyDto] = deriveCodec
  given Codec[ManufacturingDependencyByIndexDto] = deriveCodec
  given Codec[ScheduledTaskDto] = deriveCodec
  given Codec[ManufacturingDto] = deriveCodec
  given Codec[OrderRequest] = deriveCodec
  given Codec[OrderUpdateRequest] = deriveCodec
  given Codec[ManufacturingUpdateRequest] = deriveCodec
  given Codec[OrderDependenciesUpdateRequest] = deriveCodec
  given Codec[TaskDependenciesUpdateRequest] = deriveCodec
  given Codec[AddTaskRequest] = deriveCodec
  given Codec[TransitionRequest] = deriveCodec
  given Codec[TaskProgressUpdateRequest] = deriveCodec
  given Codec[ManufacturingResponse] = deriveCodec
  given Codec[SetPreferredEmployeeRequest] = deriveCodec
  given Codec[OrderResponse] = deriveCodec

  given Schema[TaskDependencyDto] = Schema.derived
  given Schema[ManufacturingDependencyDto] = Schema.derived
  given Schema[ManufacturingDependencyByIndexDto] = Schema.derived
  given Schema[ScheduledTaskDto] = Schema.derived
  given Schema[ManufacturingDto] = Schema.derived
  given Schema[OrderRequest] = Schema.derived
  given Schema[OrderUpdateRequest] = Schema.derived
  given Schema[ManufacturingUpdateRequest] = Schema.derived
  given Schema[OrderDependenciesUpdateRequest] = Schema.derived
  given Schema[TaskDependenciesUpdateRequest] = Schema.derived
  given Schema[AddTaskRequest] = Schema.derived
  given Schema[TransitionRequest] = Schema.derived
  given Schema[TaskProgressUpdateRequest] = Schema.derived
  given Schema[ManufacturingResponse] = Schema.derived
  given Schema[SetPreferredEmployeeRequest] = Schema.derived
  given Schema[OrderResponse] = Schema.derived
end OrderDtos
