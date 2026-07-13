package io.gitbub.nicolasfara.rstmanager.api

import java.util.UUID

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** DTOs mirroring the RST Manager REST API JSON payloads.
  *
  * These duplicate the shapes defined in the `service` module on purpose: the frontend is a Scala.js
  * module and cannot link against the JVM `domain`/`service` classes, so the JSON contract is restated
  * here with circe codecs. Optional JSON fields are modelled as [[scala.Option]].
  */
object Dtos:

  // ---- Shared / system ---------------------------------------------------------------------------

  final case class ApiError(code: String, message: String, details: List[String])
  final case class HealthResponse(status: String, service: String, docsUrl: String)

  // ---- Employees ---------------------------------------------------------------------------------

  final case class EmployeeContractDto(kind: String, startDate: String, endDate: Option[String], weeklyHours: Option[Int])

  final case class HoursOverrideDto(
      kind: String,
      hours: Option[Int],
      reason: Option[String],
      day: Option[String],
      startDate: Option[String],
      endDate: Option[String],
  )

  final case class EmployeeRequest(
      name: String,
      surname: String,
      contract: EmployeeContractDto,
      budgetWeeklyHours: Int,
      overrides: List[HoursOverrideDto],
  )

  final case class EmployeeResponse(
      id: UUID,
      name: String,
      surname: String,
      contract: EmployeeContractDto,
      budgetWeeklyHours: Int,
      overrides: List[HoursOverrideDto],
  )

  // ---- Customers ---------------------------------------------------------------------------------

  final case class CustomerRequest(
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      postalCode: String,
      country: String,
      fiscalCode: String,
      customerType: String,
  )

  final case class CustomerResponse(
      id: UUID,
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      postalCode: String,
      country: String,
      fiscalCode: String,
      customerType: String,
  )

  // ---- Tasks (catalog) ---------------------------------------------------------------------------

  final case class TaskRequest(name: String, description: Option[String], requiredHours: Int)
  final case class TaskResponse(id: UUID, name: String, description: Option[String], requiredHours: Int)

  // ---- Orders ------------------------------------------------------------------------------------

  final case class TaskDependencyDto(taskId: UUID, dependsOn: List[UUID])

  final case class ScheduledTaskDto(
      id: UUID,
      taskId: UUID,
      status: String,
      expectedHours: Int,
      completedHours: Option[Int],
      completionDate: Option[String],
  )

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
  )

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
  )

  final case class OrderRequest(
      number: String,
      customerId: UUID,
      creationDate: String,
      deliveryDate: String,
      promisedDeliveryDate: String,
      priority: String,
      manufacturings: List[ManufacturingDto],
      description: Option[String] = None,
  )

  final case class OrderUpdateRequest(priority: Option[String], promisedDeliveryDate: Option[String], description: Option[String] = None)
  final case class TransitionRequest(action: String, reason: Option[String])
  final case class TaskProgressUpdateRequest(completedHours: Option[Int], expectedHours: Option[Int])

  /** Update a manufacturing's free-text description and/or its lifecycle status (`reason` used when pausing). */
  final case class ManufacturingUpdateRequest(description: Option[String], status: Option[String], reason: Option[String])

  /** Add a new scheduled task (referencing a catalog task) to a manufacturing. */
  final case class AddTaskRequest(taskId: UUID, expectedHours: Int, dependsOn: List[UUID])

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
  )

  // ---- Planning ----------------------------------------------------------------------------------

  final case class PlanningTriggerDto(kind: String, orderId: Option[UUID], manufacturingId: Option[UUID], taskId: Option[UUID])

  final case class PlanningAttemptRequest(
      id: Option[UUID],
      startOn: String,
      trigger: PlanningTriggerDto,
      requestedOn: Option[String],
      orderIds: Option[List[UUID]],
      employeeIds: Option[List[UUID]],
  )

  final case class CandidateEmployeeDto(employeeId: UUID, availableHours: Int, assignedHours: Int)

  final case class ScheduledTaskSliceDto(
      orderId: UUID,
      manufacturingId: UUID,
      taskId: UUID,
      day: String,
      candidateEmployee: CandidateEmployeeDto,
      remainingHoursAfterSlice: Int,
  )

  final case class DailyScheduleDto(day: String, slices: List[ScheduledTaskSliceDto])

  final case class DelayedOrderDto(orderId: UUID, expectedDeliveryDate: String, promisedDeliveryDate: String)

  final case class DelayedManufacturingDto(
      orderId: UUID,
      manufacturingId: UUID,
      expectedCompletionDate: String,
      computedCompletionDate: String,
  )

  final case class UnplannedReasonDto(
      code: String,
      message: String,
      requiredHours: Option[Int],
      cycle: List[UUID],
      dependency: Option[UUID],
  )

  final case class UnplannedTaskDto(manufacturingId: UUID, taskId: UUID, reason: UnplannedReasonDto)
  final case class UnplannedOrderDto(orderId: UUID, blockedTasks: List[UnplannedTaskDto])
  final case class PlanningWarningDto(message: String)
  final case class PlanningDomainErrorDto(code: String, message: String)

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

  final case class PlanningRequestDto(
      id: UUID,
      startOn: String,
      trigger: PlanningTriggerDto,
      requestedOn: String,
      openOrderIds: List[UUID],
  )

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

  final case class PlanningAttemptResponse(commandId: String, planning: PlanningStateDto)

  // ---- Codecs (lazy givens, derivation order-independent) ----------------------------------------

  given Codec[ApiError] = deriveCodec
  given Codec[HealthResponse] = deriveCodec

  given Codec[EmployeeContractDto] = deriveCodec
  given Codec[HoursOverrideDto] = deriveCodec
  given Codec[EmployeeRequest] = deriveCodec
  given Codec[EmployeeResponse] = deriveCodec

  given Codec[CustomerRequest] = deriveCodec
  given Codec[CustomerResponse] = deriveCodec

  given Codec[TaskRequest] = deriveCodec
  given Codec[TaskResponse] = deriveCodec

  given Codec[TaskDependencyDto] = deriveCodec
  given Codec[ScheduledTaskDto] = deriveCodec
  given Codec[ManufacturingDto] = deriveCodec
  given Codec[ManufacturingResponse] = deriveCodec
  given Codec[OrderRequest] = deriveCodec
  given Codec[OrderUpdateRequest] = deriveCodec
  given Codec[ManufacturingUpdateRequest] = deriveCodec
  given Codec[AddTaskRequest] = deriveCodec
  given Codec[TransitionRequest] = deriveCodec
  given Codec[TaskProgressUpdateRequest] = deriveCodec
  given Codec[OrderResponse] = deriveCodec

  given Codec[PlanningTriggerDto] = deriveCodec
  given Codec[PlanningAttemptRequest] = deriveCodec
  given Codec[CandidateEmployeeDto] = deriveCodec
  given Codec[ScheduledTaskSliceDto] = deriveCodec
  given Codec[DailyScheduleDto] = deriveCodec
  given Codec[DelayedOrderDto] = deriveCodec
  given Codec[DelayedManufacturingDto] = deriveCodec
  given Codec[UnplannedReasonDto] = deriveCodec
  given Codec[UnplannedTaskDto] = deriveCodec
  given Codec[UnplannedOrderDto] = deriveCodec
  given Codec[PlanningWarningDto] = deriveCodec
  given Codec[PlanningDomainErrorDto] = deriveCodec
  given Codec[InProgressPlanningDto] = deriveCodec
  given Codec[PlanningResultDto] = deriveCodec
  given Codec[PlanningRequestDto] = deriveCodec
  given Codec[PlanningStateDto] = deriveCodec
  given Codec[PlanningAttemptResponse] = deriveCodec
end Dtos
