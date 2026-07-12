package io.github.nicolasfara.rstmanager.planning.service

import java.util.Locale
import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.*
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.service.http.ApiError

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import edomata.backend.eventsourcing.AggregateState
import io.circe.{ Codec as CirceCodec }
import io.circe.generic.semiauto.deriveCodec
import sttp.tapir.Schema

/** JSON-facing DTOs and conversions for the planning REST API. */
object PlanningApiDtos:
  final case class HealthResponse(status: String, service: String, docsUrl: String)

  final case class PlanningAttemptRequest(
      id: Option[UUID],
      startOn: String,
      trigger: PlanningTriggerDto,
      requestedOn: Option[String],
      orderIds: Option[List[UUID]],
      employeeIds: Option[List[UUID]],
  ):
    def toDomain(now: DateTime, generatedRequestId: UUID): ValidatedNec[String, PlanningAttemptInput] =
      (
        id.getOrElse(generatedRequestId).validNec,
        parseDate(startOn, "startOn"),
        trigger.toDomain("trigger"),
        requestedOn.traverse(parseDate(_, "requestedOn")).map(_.getOrElse(now)),
      ).mapN { (requestId, startDate, trigger, requestedDate) =>
        PlanningAttemptInput(requestId, startDate, trigger, requestedDate, orderIds, employeeIds)
      }

  object PlanningAttemptRequest:
    val example: PlanningAttemptRequest =
      PlanningAttemptRequest(
        None,
        "2026-06-15T00:00:00.000Z",
        PlanningTriggerDto("daily_planning", None, None, None),
        Some("2026-06-14T09:30:00.000Z"),
        None,
        None,
      )

  /** A validated planning request draft together with optional entity selections the route must load before dispatching the command. */
  final case class PlanningAttemptInput(
      id: UUID,
      startOn: DateTime,
      trigger: PlanningTrigger,
      requestedOn: DateTime,
      orderIds: Option[List[UUID]],
      employeeIds: Option[List[UUID]],
  ):
    def toPlanningRequest(openOrderIds: List[UUID]): PlanningRequest =
      PlanningRequest(id, startOn, trigger, requestedOn, openOrderIds)

  final case class PlanningAttemptResponse(commandId: String, planning: PlanningStateDto)

  final case class PlanningStateDto(
      status: String,
      version: Long,
      request: Option[PlanningRequestDto],
      result: Option[PlanningResultDto],
      inProgress: Option[InProgressPlanningDto],
      errors: List[PlanningDomainErrorDto],
      completedOn: Option[String],
      rejectedOn: Option[String],
  )

  object PlanningStateDto:
    def fromAggregateState(aggregate: AggregateState[Planning, PlanningEvent, PlanningError]): Either[ApiError, PlanningStateDto] =
      aggregate match
        case AggregateState.Valid(state, version) => fromDomain(state, version).asRight
        case AggregateState.Conflicted(_, _, errors) =>
          ApiError(
            "planning-state-conflicted",
            "Planning state cannot be replayed cleanly from the event journal.",
            errors.toChain.toList.map(PlanningDomainErrorDto.fromDomain(_).message),
          ).asLeft

    private def fromDomain(state: Planning, version: Long): PlanningStateDto =
      state match
        case Planning.NewPlanning =>
          PlanningStateDto("new", version, None, None, None, Nil, None, None)
        case Planning.InProgressPlanning(request, slices, delayedOrders, delayedManufacturings, unplannedOrders, warnings) =>
          PlanningStateDto(
            "in_progress",
            version,
            Some(PlanningRequestDto.fromDomain(request)),
            None,
            Some(
              InProgressPlanningDto(
                slices.map(ScheduledTaskSliceDto.fromDomain),
                delayedOrders.map(DelayedOrderDto.fromDomain),
                delayedManufacturings.map(DelayedManufacturingDto.fromDomain),
                unplannedOrders.map(UnplannedOrderDto.fromDomain),
                warnings.map(PlanningWarningDto.fromDomain),
              ),
            ),
            Nil,
            None,
            None,
          )
        case Planning.CompletedPlanning(request, result, completedOn) =>
          PlanningStateDto(
            "completed",
            version,
            Some(PlanningRequestDto.fromDomain(request)),
            Some(PlanningResultDto.fromDomain(result)),
            None,
            Nil,
            Some(formatDate(completedOn)),
            None,
          )
        case Planning.RejectedPlanning(request, errors, rejectedOn) =>
          PlanningStateDto(
            "rejected",
            version,
            Some(PlanningRequestDto.fromDomain(request)),
            None,
            None,
            errors.toList.map(PlanningDomainErrorDto.fromDomain),
            None,
            Some(formatDate(rejectedOn)),
          )

  final case class PlanningRequestDto(id: UUID, startOn: String, trigger: PlanningTriggerDto, requestedOn: String, openOrderIds: List[UUID])

  object PlanningRequestDto:
    def fromDomain(request: PlanningRequest): PlanningRequestDto =
      PlanningRequestDto(
        request.id,
        formatDate(request.startOn),
        PlanningTriggerDto.fromDomain(request.trigger),
        formatDate(request.requestedOn),
        request.openOrderIds,
      )

  final case class PlanningTriggerDto(kind: String, orderId: Option[UUID], manufacturingId: Option[UUID], taskId: Option[UUID]):
    def toDomain(path: String): ValidatedNec[String, PlanningTrigger] =
      normalizeKind(kind) match
        case "daily_planning" | "dailyplanning" => PlanningTrigger.DailyPlanning.validNec
        case "order_changed" | "orderchanged" =>
          required(orderId, s"$path.orderId").map(PlanningTrigger.OrderChanged.apply)
        case "manufacturing_changed" | "manufacturingchanged" =>
          (
            required(orderId, s"$path.orderId"),
            required(manufacturingId, s"$path.manufacturingId"),
          ).mapN(PlanningTrigger.ManufacturingChanged.apply)
        case "task_changed" | "taskchanged" =>
          (
            required(orderId, s"$path.orderId"),
            required(manufacturingId, s"$path.manufacturingId"),
            required(taskId, s"$path.taskId"),
          ).mapN(PlanningTrigger.TaskChanged.apply)
        case "workforce_capacity_changed" | "workforcecapacitychanged" => PlanningTrigger.WorkforceCapacityChanged.validNec
        case "manual_recovery" | "manualrecovery" => PlanningTrigger.ManualRecovery.validNec
        case other =>
          s"$path.kind '$other' is not supported. Use daily_planning, order_changed, manufacturing_changed, task_changed, workforce_capacity_changed, or manual_recovery."
            .invalidNec

  object PlanningTriggerDto:
    def fromDomain(trigger: PlanningTrigger): PlanningTriggerDto =
      trigger match
        case PlanningTrigger.DailyPlanning => PlanningTriggerDto("daily_planning", None, None, None)
        case PlanningTrigger.OrderChanged(orderId) => PlanningTriggerDto("order_changed", Some(orderId), None, None)
        case PlanningTrigger.ManufacturingChanged(orderId, manufacturingId) =>
          PlanningTriggerDto("manufacturing_changed", Some(orderId), Some(manufacturingId), None)
        case PlanningTrigger.TaskChanged(orderId, manufacturingId, taskId) =>
          PlanningTriggerDto("task_changed", Some(orderId), Some(manufacturingId), Some(taskId))
        case PlanningTrigger.WorkforceCapacityChanged => PlanningTriggerDto("workforce_capacity_changed", None, None, None)
        case PlanningTrigger.ManualRecovery => PlanningTriggerDto("manual_recovery", None, None, None)

  final case class InProgressPlanningDto(
      slices: List[ScheduledTaskSliceDto],
      delayedOrders: List[DelayedOrderDto],
      delayedManufacturings: List[DelayedManufacturingDto],
      unplannedOrders: List[UnplannedOrderDto],
      warnings: List[PlanningWarningDto],
  )

  final case class PlanningResultDto(
      schedules: List[DailyScheduleDto],
      delayedOrders: List[DelayedOrderDto],
      delayedManufacturings: List[DelayedManufacturingDto],
      unplannedOrders: List[UnplannedOrderDto],
      warnings: List[PlanningWarningDto],
  )

  object PlanningResultDto:
    def fromDomain(result: PlanningResult): PlanningResultDto =
      PlanningResultDto(
        result.schedules.map(DailyScheduleDto.fromDomain),
        result.delayedOrders.map(DelayedOrderDto.fromDomain),
        result.delayedManufacturings.map(DelayedManufacturingDto.fromDomain),
        result.unplannedOrders.map(UnplannedOrderDto.fromDomain),
        result.warnings.map(PlanningWarningDto.fromDomain),
      )

  final case class DailyScheduleDto(day: String, slices: List[ScheduledTaskSliceDto])

  object DailyScheduleDto:
    def fromDomain(schedule: DailySchedule): DailyScheduleDto =
      DailyScheduleDto(formatDate(schedule.day), schedule.slices.toList.map(ScheduledTaskSliceDto.fromDomain))

  final case class ScheduledTaskSliceDto(
      orderId: UUID,
      manufacturingId: UUID,
      taskId: UUID,
      day: String,
      candidateEmployee: CandidateEmployeeDto,
      remainingHoursAfterSlice: Int,
  )

  object ScheduledTaskSliceDto:
    def fromDomain(slice: ScheduledTaskSlice): ScheduledTaskSliceDto =
      ScheduledTaskSliceDto(
        slice.orderId,
        slice.manufacturingId,
        slice.taskId,
        formatDate(slice.day),
        CandidateEmployeeDto.fromDomain(slice.candidateEmployee),
        slice.remainingHoursAfterSlice.value,
      )

  final case class CandidateEmployeeDto(employeeId: UUID, availableHours: Int, assignedHours: Int)

  object CandidateEmployeeDto:
    def fromDomain(candidate: CandidateEmployee): CandidateEmployeeDto =
      CandidateEmployeeDto(candidate.employeeId, candidate.availableHours.value, candidate.assignedHours.value)

  final case class DelayedOrderDto(orderId: UUID, expectedDeliveryDate: String, promisedDeliveryDate: String)

  object DelayedOrderDto:
    def fromDomain(delay: DelayedOrder): DelayedOrderDto =
      DelayedOrderDto(delay.orderId, formatDate(delay.expectedDeliveryDate), formatDate(delay.promisedDeliveryDate))

  final case class DelayedManufacturingDto(
      orderId: UUID,
      manufacturingId: UUID,
      expectedCompletionDate: String,
      computedCompletionDate: String,
  )

  object DelayedManufacturingDto:
    def fromDomain(delay: DelayedManufacturing): DelayedManufacturingDto =
      DelayedManufacturingDto(
        delay.orderId,
        delay.manufacturingId,
        formatDate(delay.expectedCompletionDate),
        formatDate(delay.computedCompletionDate),
      )

  final case class UnplannedOrderDto(orderId: UUID, blockedTasks: List[UnplannedTaskDto])

  object UnplannedOrderDto:
    def fromDomain(unplannedOrder: UnplannedOrder): UnplannedOrderDto =
      UnplannedOrderDto(unplannedOrder.orderId, unplannedOrder.blockedTasks.toList.map(UnplannedTaskDto.fromDomain))

  final case class UnplannedTaskDto(manufacturingId: UUID, taskId: UUID, reason: UnplannedReasonDto)

  object UnplannedTaskDto:
    def fromDomain(unplannedTask: UnplannedTask): UnplannedTaskDto =
      UnplannedTaskDto(unplannedTask.manufacturingId, unplannedTask.taskId, UnplannedReasonDto.fromDomain(unplannedTask.reason))

  final case class UnplannedReasonDto(
      code: String,
      message: String,
      requiredHours: Option[Int],
      cycle: List[UUID],
      dependency: Option[UUID],
  )

  object UnplannedReasonDto:
    def fromDomain(reason: UnplannedReason): UnplannedReasonDto =
      reason match
        case UnplannedReason.NoFutureCapacity(requiredHours) =>
          UnplannedReasonDto("no_future_capacity", s"No future production day can provide ${requiredHours.value} hours.", Some(requiredHours.value), Nil, None)
        case UnplannedReason.DependencyCycle(cycle) =>
          UnplannedReasonDto("dependency_cycle", "The manufacturing dependency graph contains a cycle.", None, cycle.toList.sortBy(_.toString), None)
        case UnplannedReason.MissingDependency(dependency) =>
          UnplannedReasonDto("missing_dependency", s"Task dependency $dependency is not present in the manufacturing.", None, Nil, Some(dependency))
        case UnplannedReason.BlockedByDependency(dependency) =>
          UnplannedReasonDto("blocked_by_dependency", s"The task is blocked by unplanned dependency $dependency.", None, Nil, Some(dependency))

  final case class PlanningWarningDto(message: String)

  object PlanningWarningDto:
    def fromDomain(warning: PlanningWarning): PlanningWarningDto = PlanningWarningDto(warning.message)

  final case class PlanningDomainErrorDto(code: String, message: String)

  object PlanningDomainErrorDto:
    def fromDomain(error: PlanningError): PlanningDomainErrorDto =
      error match
        case PlanningError.InvalidEmployeeAssignment(availableHours, assignedHours) =>
          PlanningDomainErrorDto("invalid_employee_assignment", s"Assigned hours ${assignedHours.value} exceed available hours ${availableHours.value}.")
        case PlanningError.InvalidOrderDelay(orderId, expectedDeliveryDate, promisedDeliveryDate) =>
          PlanningDomainErrorDto(
            "invalid_order_delay",
            s"Order $orderId promised date ${formatDate(promisedDeliveryDate)} is not after expected date ${formatDate(expectedDeliveryDate)}.",
          )
        case PlanningError.InvalidManufacturingDelay(orderId, manufacturingId, expectedCompletionDate, computedCompletionDate) =>
          PlanningDomainErrorDto(
            "invalid_manufacturing_delay",
            s"Manufacturing $manufacturingId for order $orderId computed date ${formatDate(computedCompletionDate)} is not after expected date ${formatDate(expectedCompletionDate)}.",
          )
        case PlanningError.EmptyDailySchedule(day) =>
          PlanningDomainErrorDto("empty_daily_schedule", s"Daily schedule for ${formatDate(day)} has no task slices.")
        case PlanningError.TaskSliceOutsideScheduleDay(scheduleDay, sliceDay) =>
          PlanningDomainErrorDto(
            "task_slice_outside_schedule_day",
            s"Task slice day ${formatDate(sliceDay)} is outside schedule day ${formatDate(scheduleDay)}.",
          )
        case PlanningError.ManufacturingDeadlineExceeded(orderId, manufacturingId, expectedDate, computedDate) =>
          PlanningDomainErrorDto(
            "manufacturing_deadline_exceeded",
            s"Manufacturing $manufacturingId for order $orderId completes on ${formatDate(computedDate)} after expected date ${formatDate(expectedDate)}.",
          )
        case PlanningError.TaskCannotBeScheduled(orderId, manufacturingId, taskId, day, blockingConstraint) =>
          PlanningDomainErrorDto(
            "task_cannot_be_scheduled",
            s"Task $taskId for manufacturing $manufacturingId and order $orderId cannot be scheduled on ${formatDate(day)}: $blockingConstraint.",
          )
        case PlanningError.PlanningAlreadyInProgress(requestId) =>
          PlanningDomainErrorDto("planning_already_in_progress", s"Planning request $requestId is already in progress.")
        case PlanningError.PlanningMustBeInProgress =>
          PlanningDomainErrorDto("planning_must_be_in_progress", "Planning must be in progress for this operation.")

  private def parseDate(value: String, path: String): ValidatedNec[String, DateTime] =
    Either
      .catchNonFatal(DateTime.parse(value).nn)
      .leftMap(_ => s"$path must be an ISO-8601 date-time.")
      .toValidatedNec

  private def required[A](value: Option[A], path: String): ValidatedNec[String, A] =
    value.toValidNec(s"$path is required.")

  private def normalizeKind(value: String): String =
    value.trim.nn.toLowerCase(Locale.ROOT).nn.replace('-', '_').nn

  private def formatDate(value: DateTime): String = value.toString

  given CirceCodec[HealthResponse] = deriveCodec
  given CirceCodec[PlanningAttemptRequest] = deriveCodec
  given CirceCodec[PlanningAttemptResponse] = deriveCodec
  given CirceCodec[PlanningStateDto] = deriveCodec
  given CirceCodec[PlanningRequestDto] = deriveCodec
  given CirceCodec[PlanningTriggerDto] = deriveCodec
  given CirceCodec[InProgressPlanningDto] = deriveCodec
  given CirceCodec[PlanningResultDto] = deriveCodec
  given CirceCodec[DailyScheduleDto] = deriveCodec
  given CirceCodec[ScheduledTaskSliceDto] = deriveCodec
  given CirceCodec[CandidateEmployeeDto] = deriveCodec
  given CirceCodec[DelayedOrderDto] = deriveCodec
  given CirceCodec[DelayedManufacturingDto] = deriveCodec
  given CirceCodec[UnplannedOrderDto] = deriveCodec
  given CirceCodec[UnplannedTaskDto] = deriveCodec
  given CirceCodec[UnplannedReasonDto] = deriveCodec
  given CirceCodec[PlanningWarningDto] = deriveCodec
  given CirceCodec[PlanningDomainErrorDto] = deriveCodec

  given Schema[HealthResponse] = Schema.derived
  given Schema[PlanningTriggerDto] = Schema.derived
  given Schema[PlanningRequestDto] = Schema.derived
  given Schema[CandidateEmployeeDto] = Schema.derived
  given Schema[ScheduledTaskSliceDto] = Schema.derived
  given Schema[DelayedOrderDto] = Schema.derived
  given Schema[DelayedManufacturingDto] = Schema.derived
  given Schema[UnplannedReasonDto] = Schema.derived
  given Schema[UnplannedTaskDto] = Schema.derived
  given Schema[UnplannedOrderDto] = Schema.derived
  given Schema[PlanningWarningDto] = Schema.derived
  given Schema[PlanningResultDto] = Schema.derived
  given Schema[DailyScheduleDto] = Schema.derived
  given Schema[PlanningDomainErrorDto] = Schema.derived
  given Schema[InProgressPlanningDto] = Schema.derived
  given Schema[PlanningStateDto] = Schema.derived
  given Schema[PlanningAttemptRequest] = Schema.derived
  given Schema[PlanningAttemptResponse] = Schema.derived
end PlanningApiDtos
