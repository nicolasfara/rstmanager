package io.github.nicolasfara.rstmanager.work.service

import java.util.Locale
import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.*
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
  ):
    def toDomain(path: String, id: UUID): ValidatedNec[String, ScheduledManufacturing] =
      val taskList = tasks.zipWithIndex
        .traverse { case (task, index) => task.toDomain(s"$path.tasks[$index]") }
        .andThen(list => NonEmptyList.fromList(list).toValidNec(s"$path.tasks must contain at least one task"))

      val dependencyGraph = dependencies.foldLeft(ManufacturingDependencies()) { (current, dependency) =>
        current.addTaskDependencies(dependency.taskId, dependency.dependsOn.toSet)
      }

      val info = (manufacturingCode(code), parseDate(completionDate, s"$path.completionDate"), taskList).mapN {
        (code, expectedCompletionDate, scheduledTasks) => ScheduledManufacturingInfo(id, code, expectedCompletionDate, scheduledTasks, dependencyGraph)
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
      )

  final case class OrderRequest(
      number: String,
      customerId: UUID,
      creationDate: String,
      deliveryDate: String,
      promisedDeliveryDate: String,
      priority: String,
      manufacturings: List[ManufacturingDto],
  ):
    def toDomain(id: UUID, nextManufacturingId: () => UUID = () => UUID.randomUUID().nn): ValidatedNec[String, (OrderData, DateTime)] =
      val manufacturingList = manufacturings.zipWithIndex
        .traverse { case (manufacturing, index) => manufacturing.toDomain(s"manufacturings[$index]", nextManufacturingId()) }
        .andThen(list => NonEmptyList.fromList(list).toValidNec("manufacturings must contain at least one manufacturing"))

      (
        number.refineValidatedNec[OrderNumber],
        parseDate(creationDate, "creationDate"),
        parseDate(deliveryDate, "deliveryDate"),
        parseDate(promisedDeliveryDate, "promisedDeliveryDate"),
        priorityToDomain(priority, "priority"),
        manufacturingList,
      ).mapN { (orderNumber, createdAt, expectedDelivery, promisedDelivery, orderPriority, manufacturingNel) =>
        (OrderData(id, orderNumber, customerId, createdAt, expectedDelivery, orderPriority, manufacturingNel), promisedDelivery)
      }

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

  final case class OrderUpdateRequest(priority: Option[String], promisedDeliveryDate: Option[String]):
    def toCommands: ValidatedNec[String, List[OrderService.Command]] =
      (
        priority.traverse(priorityToDomain(_, "priority")),
        promisedDeliveryDate.traverse(parseDate(_, "promisedDeliveryDate")),
      ).mapN { (parsedPriority, parsedDate) =>
        parsedPriority.map(OrderService.Command.ChangePriority.apply).toList ++
          parsedDate.map(OrderService.Command.UpdatePromisedDeliveryDate.apply).toList
      }

  object OrderUpdateRequest:
    val example: OrderUpdateRequest =
      OrderUpdateRequest(Some("urgent"), Some("2026-06-24T17:00:00.000Z"))

  final case class TransitionRequest(action: String, reason: Option[String]):
    def toCommand: ValidatedNec[String, OrderService.Command] =
      normalizeKind(action) match
        case "suspend" => reason.traverse(_.refineValidatedNec[SuspensionReason]).map(OrderService.Command.Suspend.apply)
        case "reactivate" => OrderService.Command.Reactivate.validNec
        case "complete" => OrderService.Command.Complete.validNec
        case "deliver" => OrderService.Command.Deliver.validNec
        case other => s"action '$other' is not supported. Use suspend, reactivate, complete, or deliver.".invalidNec

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
  )

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
        )
      }

    private def dataAndStatus(order: Order): Option[(OrderData, String, Option[DateTime])] = order match
      case Order.NewOrder => None
      case Order.InProgressOrder(data, promised) => Some((data, "in_progress", Some(promised)))
      case Order.SuspendedOrder(data, promised, _, _) => Some((data, "suspended", Some(promised)))
      case Order.CompletedOrder(data, _) => Some((data, "completed", None))
      case Order.DeliveredOrder(data, _, _) => Some((data, "delivered", None))
      case Order.CancelledOrder(data, _, _) => Some((data, "cancelled", None))

  private def priorityToDomain(value: String, path: String): ValidatedNec[String, OrderPriority] =
    normalizeKind(value) match
      case "normal" => OrderPriority.Normal.validNec
      case "urgent" => OrderPriority.Urgent.validNec
      case other => s"$path '$other' is not supported. Use normal or urgent.".invalidNec

  private def manufacturingCode(value: String): ValidatedNec[String, String :| ManufacturingCode] =
    value.refineValidatedNec[ManufacturingCode]

  private def parseDate(value: String, path: String): ValidatedNec[String, DateTime] =
    Either.catchNonFatal(DateTime.parse(value).nn).leftMap(_ => s"$path must be an ISO-8601 date-time.").toValidatedNec

  private def required[A](value: Option[A], path: String): ValidatedNec[String, A] = value.toValidNec(s"$path is required.")

  private def normalizeKind(value: String): String = value.trim.nn.toLowerCase(Locale.ROOT).nn.replace('-', '_').nn

  private def formatDate(value: DateTime): String = value.toString

  given Codec[TaskDependencyDto] = deriveCodec
  given Codec[ScheduledTaskDto] = deriveCodec
  given Codec[ManufacturingDto] = deriveCodec
  given Codec[OrderRequest] = deriveCodec
  given Codec[OrderUpdateRequest] = deriveCodec
  given Codec[TransitionRequest] = deriveCodec
  given Codec[TaskProgressUpdateRequest] = deriveCodec
  given Codec[ManufacturingResponse] = deriveCodec
  given Codec[OrderResponse] = deriveCodec

  given Schema[TaskDependencyDto] = Schema.derived
  given Schema[ScheduledTaskDto] = Schema.derived
  given Schema[ManufacturingDto] = Schema.derived
  given Schema[OrderRequest] = Schema.derived
  given Schema[OrderUpdateRequest] = Schema.derived
  given Schema[TransitionRequest] = Schema.derived
  given Schema[TaskProgressUpdateRequest] = Schema.derived
  given Schema[ManufacturingResponse] = Schema.derived
  given Schema[OrderResponse] = Schema.derived
end OrderDtos
