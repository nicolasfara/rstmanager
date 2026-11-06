package io.gitbub.nicolasfara.rstmanager.api

import java.util.UUID

import io.circe.Codec
import io.circe.derivation.{ Configuration, ConfiguredCodec }
import io.circe.generic.semiauto.deriveCodec

/**
 * DTOs mirroring the RST Manager REST API JSON payloads.
 *
 * These duplicate the shapes defined in the `service` module on purpose: the frontend is a Scala.js module and cannot link against the JVM
 * `domain`/`service` classes, so the JSON contract is restated here with circe codecs. Optional JSON fields are modelled as [[scala.Option]].
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
      businessName: Option[String],
      pec: Option[String],
      notes: Option[String],
      boatModel: Option[String],
      boatName: Option[String],
      boatBerth: Option[String],
      port: Option[String],
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
      businessName: Option[String],
      pec: Option[String],
      notes: Option[String],
      boatModel: Option[String],
      boatName: Option[String],
      boatBerth: Option[String],
      port: Option[String],
  )

  // ---- Tasks (catalog) ---------------------------------------------------------------------------

  final case class TaskRequest(name: String, description: Option[String], requiredMinutes: Int, defaultEmployeeId: Option[UUID] = None)
  final case class TaskResponse(id: UUID, name: String, description: Option[String], requiredMinutes: Int, defaultEmployeeId: Option[UUID])

  // ---- Manufacturings (catalog) ------------------------------------------------------------------

  final case class ManufacturingCatalogDependencyDto(taskId: UUID, dependsOn: List[UUID])

  /** Default employee proposed for one task of the template when the manufacturing is scheduled inside an order. */
  final case class TaskDefaultEmployeeDto(taskId: UUID, employeeId: UUID)

  /** Per-task duration override (minutes) of the template; a task without an entry falls back to the task-catalog duration. */
  final case class TaskDurationOverrideDto(taskId: UUID, minutes: Int)

  final case class ManufacturingCatalogRequest(
      code: String,
      name: String,
      description: Option[String],
      taskIds: List[UUID],
      dependencies: List[ManufacturingCatalogDependencyDto],
      defaultEmployees: Option[List[TaskDefaultEmployeeDto]] = None,
      taskDurations: Option[List[TaskDurationOverrideDto]] = None,
  )

  final case class ManufacturingCatalogResponse(
      id: UUID,
      code: String,
      name: String,
      description: Option[String],
      taskIds: List[UUID],
      tasks: List[TaskResponse],
      dependencies: List[ManufacturingCatalogDependencyDto],
      totalRequiredMinutes: Int,
      defaultEmployees: List[TaskDefaultEmployeeDto] = Nil,
      taskDurations: List[TaskDurationOverrideDto] = Nil,
  )

  // ---- Orders ------------------------------------------------------------------------------------

  final case class TaskDependencyDto(taskId: UUID, dependsOn: List[UUID])

  /** Dependency edges of one manufacturing towards the manufacturings that must be completed before it. */
  final case class ManufacturingDependencyDto(manufacturingId: UUID, dependsOn: List[UUID])

  /** Dependency edges between the manufacturings of an order creation request, referenced by their position in `manufacturings`. */
  final case class ManufacturingDependencyByIndexDto(manufacturingIndex: Int, dependsOnIndexes: List[Int])

  final case class ScheduledTaskDto(
      id: UUID,
      taskId: UUID,
      status: String,
      expectedMinutes: Int,
      completedMinutes: Option[Int],
      completionDate: Option[String],
      preferredEmployeeId: Option[UUID] = None,
  )

  /**
   * A material line of a request: a warehouse product and the quantity used (canonical decimal string, at most 3 decimals). An absent `unitPrice` (4
   * decimals) keeps the price already frozen for the product in the manufacturing, or else lets the server freeze the product's current customer
   * price.
   */
  final case class MaterialDto(productId: UUID, quantity: String, unitPrice: Option[String])

  /** `PUT /orders/{id}/manufacturings/{manufacturingId}/materials`: replaces the material lines of a manufacturing. */
  final case class MaterialsUpdateRequest(materials: List[MaterialDto])

  /**
   * A material line of a manufacturing as read back: the frozen `unitPrice` (4 decimals), the `lineCost` (2 decimals), and the product master data as
   * the warehouse knows it now (absent for a product it no longer knows; `unit` is the wire name).
   */
  final case class MaterialResponse(
      productId: UUID,
      internalCode: Option[String],
      description: Option[String],
      unit: Option[String],
      archived: Boolean,
      quantity: String,
      unitPrice: String,
      lineCost: String,
  )

  /** A manufacturing of an order creation / add-manufacturing request; `materials` may be omitted (no materials). */
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
      materials: Option[List[MaterialDto]] = None,
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
      preferredEmployeeId: Option[UUID] = None,
      materials: List[MaterialResponse] = Nil,
      materialsCost: String = "0.00",
  )

  final case class SetPreferredEmployeeRequest(employeeId: Option[UUID])

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
  )

  final case class OrderUpdateRequest(priority: Option[String], promisedDeliveryDate: Option[String], description: Option[String] = None)
  final case class TransitionRequest(action: String, reason: Option[String])
  final case class TaskProgressUpdateRequest(completedMinutes: Option[Int], expectedMinutes: Option[Int])

  /** Replaces the dependency graph between the order manufacturings. */
  final case class OrderDependenciesUpdateRequest(dependencies: List[ManufacturingDependencyDto])

  /** Replaces the task dependency graph of a manufacturing. */
  final case class TaskDependenciesUpdateRequest(dependencies: List[TaskDependencyDto])

  /** Update a manufacturing's free-text description, work deadline and/or lifecycle status (`reason` used when pausing). */
  final case class ManufacturingUpdateRequest(
      description: Option[String],
      completionDate: Option[String],
      status: Option[String],
      reason: Option[String],
  )

  /** Add a new scheduled task (referencing a catalog task) to a manufacturing, optionally with a preferred employee. */
  final case class AddTaskRequest(taskId: UUID, expectedMinutes: Int, dependsOn: List[UUID], preferredEmployeeId: Option[UUID] = None)

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
      materialsCost: String = "0.00",
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

  final case class CandidateEmployeeDto(employeeId: UUID, availableHours: Int, assignedMinutes: Int)

  final case class ScheduledTaskSliceDto(
      orderId: UUID,
      manufacturingId: UUID,
      taskId: UUID,
      day: String,
      candidateEmployee: CandidateEmployeeDto,
      remainingMinutesAfterSlice: Int,
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
      requiredMinutes: Option[Int],
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

  /** Order-simulation request: exactly one of `totalMinutes` or `manufacturingIds` must be provided. */
  final case class OrderSimulationRequest(totalMinutes: Option[Int], manufacturingIds: Option[List[UUID]])

  final case class OrderSimulationResponse(
      feasible: Boolean,
      totalMinutes: Int,
      startDate: Option[String],
      estimatedCompletionDate: Option[String],
      reasons: List[UnplannedReasonDto],
  )

  // ---- Warehouse ---------------------------------------------------------------------------------

  /** Supplier registry entry (create/replace payload). Blank optional values are sent as absent; the server trims and normalizes them. */
  final case class SupplierRequest(
      name: String,
      vatNumber: Option[String],
      email: Option[String],
      phone: Option[String],
      address: Option[String],
      notes: Option[String],
  )

  final case class SupplierResponse(
      id: UUID,
      name: String,
      vatNumber: Option[String],
      email: Option[String],
      phone: Option[String],
      address: Option[String],
      notes: Option[String],
  )

  /** A manufacturer part number of a product, bought from `supplierId`: the same shape in requests and responses. */
  final case class MpnDto(code: String, supplierId: UUID, description: Option[String])

  /**
   * Product registry entry (create/replace payload). `unit` is the wire name (`pieces`, `linear_meters`, `square_meters`, `kilograms`);
   * `customerPrice` a canonical decimal string with `.` and up to 4 decimals (see `WarehouseFormats.parseDecimal`).
   */
  final case class ProductRequest(
      internalCode: String,
      description: String,
      productType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: String,
      mpns: List[MpnDto],
  )

  /** A supplier of the product, derived from its MPNs; `active` is false for a supplier deleted after the MPN was added. */
  final case class ProductSupplierDto(id: UUID, name: String, active: Boolean)

  /**
   * Stock of a product on a day: signed quantity with 3 decimals (negative only as an anomaly), the highest unit cost recorded so far (4 decimals),
   * the value `quantity × maxUnitCost` (2 decimals; absent while no cost is known), and the total spent on purchases up to that day.
   */
  final case class StockDto(quantity: String, maxUnitCost: Option[String], value: Option[String], purchasedTotal: String, negative: Boolean)

  /** A product as read back: prices with exactly 4 decimals (`"18.5000"`), days as `YYYY-MM-DD`, `stock` as of today. */
  final case class ProductResponse(
      id: UUID,
      internalCode: String,
      description: String,
      productType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: String,
      mpns: List[MpnDto],
      suppliers: List[ProductSupplierDto],
      createdOn: String,
      archived: Boolean,
      archivedOn: Option[String],
      stock: StockDto,
  )

  // ---- Warehouse: stock movements ----------------------------------------------------------------
  // Quantities are decimal strings with 3 decimals, unit costs with 4, amounts with 2; days are `YYYY-MM-DD`. An omitted `on` is "today" server-side.

  /** A purchase under one of the product's MPNs (Operator). */
  final case class PurchaseRequest(mpnCode: String, supplierId: UUID, quantity: String, unitCost: String, on: Option[String], note: Option[String])

  /** Stock taken out by hand (Operator); refused with 409 `insufficient-stock` when the balance would go below zero on `on` or later. */
  final case class UnloadRequest(quantity: String, on: Option[String], reason: String)

  /** A signed correction of the stock (Operator); `unitCost` only for a positive `delta` (it then counts as a price point). */
  final case class AdjustmentRequest(delta: String, unitCost: Option[String], on: Option[String], reason: String)

  /** The movement just recorded and the product with its updated stock. */
  final case class MovementRecordedResponse(movementId: UUID, product: ProductResponse)

  /**
   * A movement in the list: `kind` is `purchase`, `unload`, `adjustment`, or `consumption` (automatic consumption by an order manufacturing: an
   * outflow, or its reversal when `delta` is positive); `quantity` the magnitude, `delta` the signed change, `value` the signed value (absent without
   * a cost), `balance` the running balance after it (a voided movement leaves it unchanged). A consumption names its `orderId`/`manufacturingId`,
   * with the order number and manufacturing code when the server can still resolve them; the four are absent for the other kinds.
   */
  final case class MovementResponse(
      id: UUID,
      sequence: Int,
      kind: String,
      on: String,
      quantity: String,
      delta: String,
      unitCost: Option[String],
      value: Option[String],
      supplier: Option[ProductSupplierDto],
      mpnCode: Option[String],
      reason: Option[String],
      note: Option[String],
      voided: Boolean,
      voidedOn: Option[String],
      voidReason: Option[String],
      balance: String,
      orderId: Option[UUID] = None,
      manufacturingId: Option[UUID] = None,
      orderNumber: Option[String] = None,
      manufacturingCode: Option[String] = None,
  )

  /** The movements of a product from `from` to `to` (inclusive, both optional) with the balance before the first day and after the last. */
  final case class MovementsResponse(
      productId: UUID,
      unit: String,
      from: Option[String],
      to: Option[String],
      openingBalance: String,
      closingBalance: String,
      movements: List[MovementResponse],
  )

  // ---- Warehouse: reports ------------------------------------------------------------------------

  /** A product in the state of a day, with the customer price in effect on that day and its stock at the end of it. */
  final case class StateRowResponse(
      productId: UUID,
      internalCode: String,
      description: String,
      productType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: Option[String],
      stock: StockDto,
      archived: Boolean,
      suppliers: List[ProductSupplierDto],
      mpns: List[MpnDto],
  )

  final case class StateTotalsResponse(products: Int, value: String, purchasedTotal: String, negativeProducts: Int, unvaluedProducts: Int)

  /** `GET /warehouse/state?at=`: the warehouse at the end of day `at`. */
  final case class WarehouseStateResponse(at: String, rows: List[StateRowResponse], totals: StateTotalsResponse)

  /** Quantities in and out (non-negative) and their difference, in one unit of measure. */
  final case class UnitFlowResponse(unit: String, quantityIn: String, quantityOut: String, net: String)

  /** Totals of a period: `net = inValue - outValue`; `purchaseValue` is the part of `inValue` bought from suppliers. */
  final case class FlowTotalsResponse(inValue: String, outValue: String, net: String, purchaseValue: String, quantities: List[UnitFlowResponse])

  /** One ISO week (from Monday; `label` like `2026-W43`) or month (`2026-10`); `from`/`to` are the days counted, clipped to the period. */
  final case class FlowBucketResponse(start: String, label: String, from: String, to: String, totals: FlowTotalsResponse)

  /** `GET /warehouse/flows?from&to&granularity`. */
  final case class FlowReportResponse(from: String, to: String, granularity: String, buckets: List[FlowBucketResponse], totals: FlowTotalsResponse)

  /** What was bought under one MPN of one product. */
  final case class MpnSpendResponse(
      productId: UUID,
      internalCode: String,
      description: String,
      mpnCode: String,
      unit: String,
      quantity: String,
      amount: String,
      purchases: Int,
  )

  /** What was bought from one supplier (`active` is false once the supplier is deleted). */
  final case class SupplierSpendResponse(
      supplierId: UUID,
      name: String,
      active: Boolean,
      amount: String,
      purchases: Int,
      mpns: List[MpnSpendResponse],
  )

  /** `GET /warehouse/supplier-spend?from&to`. */
  final case class SupplierSpendReportResponse(from: String, to: String, total: String, suppliers: List[SupplierSpendResponse])

  // ---- Codecs (lazy givens, derivation order-independent) ----------------------------------------

  /**
   * Derivation for responses that gained fields after their first release: an absent field decodes to its case-class default (plain `deriveCodec`
   * ignores defaults and fails on a missing non-`Option` field). Encoding is unchanged.
   */
  private val withDefaults: Configuration = Configuration.default.withDefaults

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
  given Codec[ManufacturingCatalogDependencyDto] = deriveCodec
  given Codec[TaskDefaultEmployeeDto] = deriveCodec
  given Codec[TaskDurationOverrideDto] = deriveCodec
  given Codec[ManufacturingCatalogRequest] = deriveCodec
  given Codec[ManufacturingCatalogResponse] = ConfiguredCodec.derived(using withDefaults)

  given Codec[TaskDependencyDto] = deriveCodec
  given Codec[ManufacturingDependencyDto] = deriveCodec
  given Codec[ManufacturingDependencyByIndexDto] = deriveCodec
  given Codec[ScheduledTaskDto] = deriveCodec
  given Codec[MaterialDto] = deriveCodec
  given Codec[MaterialsUpdateRequest] = deriveCodec
  given Codec[MaterialResponse] = deriveCodec
  given Codec[ManufacturingDto] = deriveCodec
  given Codec[ManufacturingResponse] = ConfiguredCodec.derived(using withDefaults)
  given Codec[SetPreferredEmployeeRequest] = deriveCodec
  given Codec[OrderRequest] = deriveCodec
  given Codec[OrderUpdateRequest] = deriveCodec
  given Codec[ManufacturingUpdateRequest] = deriveCodec
  given Codec[OrderDependenciesUpdateRequest] = deriveCodec
  given Codec[TaskDependenciesUpdateRequest] = deriveCodec
  given Codec[AddTaskRequest] = deriveCodec
  given Codec[TransitionRequest] = deriveCodec
  given Codec[TaskProgressUpdateRequest] = deriveCodec
  given Codec[OrderResponse] = ConfiguredCodec.derived(using withDefaults)

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
  given Codec[OrderSimulationRequest] = deriveCodec
  given Codec[OrderSimulationResponse] = deriveCodec

  given Codec[SupplierRequest] = deriveCodec
  given Codec[SupplierResponse] = deriveCodec
  given Codec[MpnDto] = deriveCodec
  given Codec[ProductRequest] = deriveCodec
  given Codec[ProductSupplierDto] = deriveCodec
  given Codec[StockDto] = deriveCodec
  given Codec[ProductResponse] = deriveCodec
  given Codec[PurchaseRequest] = deriveCodec
  given Codec[UnloadRequest] = deriveCodec
  given Codec[AdjustmentRequest] = deriveCodec
  given Codec[MovementRecordedResponse] = deriveCodec
  given Codec[MovementResponse] = ConfiguredCodec.derived(using withDefaults)
  given Codec[MovementsResponse] = deriveCodec
  given Codec[StateRowResponse] = deriveCodec
  given Codec[StateTotalsResponse] = deriveCodec
  given Codec[WarehouseStateResponse] = deriveCodec
  given Codec[UnitFlowResponse] = deriveCodec
  given Codec[FlowTotalsResponse] = deriveCodec
  given Codec[FlowBucketResponse] = deriveCodec
  given Codec[FlowReportResponse] = deriveCodec
  given Codec[MpnSpendResponse] = deriveCodec
  given Codec[SupplierSpendResponse] = deriveCodec
  given Codec[SupplierSpendReportResponse] = deriveCodec
end Dtos
