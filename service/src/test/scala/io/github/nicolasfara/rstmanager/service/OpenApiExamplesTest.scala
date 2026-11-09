package io.github.nicolasfara.rstmanager.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.service.CustomerHttpApi
import io.github.nicolasfara.rstmanager.customer.service.CustomerHttpApi.CustomerRequest
import io.github.nicolasfara.rstmanager.hr.service.{ CompanyCalendarHttpApi, EmployeeHttpApi }
import io.github.nicolasfara.rstmanager.hr.service.CompanyCalendarHttpApi.ClosureRequest
import io.github.nicolasfara.rstmanager.hr.service.EmployeeHttpApi.EmployeeRequest
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.PlanningAttemptRequest
import io.github.nicolasfara.rstmanager.planning.service.PlanningEndpoints
import io.github.nicolasfara.rstmanager.warehouse.domain.{ UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialCode, MaterialDescription, MaterialId, MaterialInfo, MaterialRecord }
import io.github.nicolasfara.rstmanager.warehouse.service.{
  DeliveryNoteHttpApi,
  MaterialHttpApi,
  StockHttpApi,
  SupplierHttpApi,
  WarehouseReportsHttpApi,
}
import io.github.nicolasfara.rstmanager.warehouse.service.DeliveryNoteHttpApi.{ CancelDeliveryNoteRequest, DeliveryNoteRequest }
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp.StoredMaterial
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialHttpApi.MaterialRequest
import io.github.nicolasfara.rstmanager.warehouse.service.StockHttpApi.{ AdjustmentRequest, PurchaseRequest, UnloadRequest }
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierHttpApi.SupplierRequest
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.service.{ ManufacturingHttpApi, OrderHttpApi, TaskHttpApi }
import io.github.nicolasfara.rstmanager.work.service.ManufacturingHttpApi.ManufacturingRequest
import io.github.nicolasfara.rstmanager.work.service.OrderDtos.{
  ManufacturingUpdateRequest,
  MaterialCatalog,
  MaterialsUpdateRequest,
  OrderRequest,
  OrderUpdateRequest,
  TaskProgressUpdateRequest,
  TransitionRequest,
}
import io.github.nicolasfara.rstmanager.work.service.TaskHttpApi.TaskRequest

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import sttp.apispec.openapi.circe.yaml.*
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter

class OpenApiExamplesTest extends AnyFlatSpecLike:
  private val id: UUID = UUID.fromString("00000000-0000-0000-0000-000000000999").nn
  private val generatedManufacturingId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000998").nn
  private val now: DateTime = DateTime.parse("2026-06-14T09:30:00.000Z")

  /** The warehouse materials named by the material examples, active and priced at 18.5000 €. */
  private val exampleCatalog: MaterialCatalog =
    MaterialCatalog(
      List("00000000-0000-0000-0000-000000000c01", "00000000-0000-0000-0000-000000000c02").map { raw =>
        val materialId = MaterialId(UUID.fromString(raw).nn)
        val info = MaterialInfo(
          materialId,
          "PRD-1".refineUnsafe[MaterialCode],
          "Profilo in alluminio".refineUnsafe[MaterialDescription],
          None,
          None,
          UnitOfMeasure.LinearMeters,
          UnitPrice(185000L),
          Nil,
        )
        materialId -> StoredMaterial(MaterialRecord.created(info, LocalDate.parse("2026-06-01")), None)
      }.toMap,
    )

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  "OpenAPI request examples" should "be accepted by edge DTO validation" in:
    validOrFail(PlanningAttemptRequest.example.toDomain(now, id))
    val (orderData, _) = validOrFail(OrderRequest.example.toDomain(id, () => generatedManufacturingId, exampleCatalog))
    orderData.setOfManufacturing.head.info.id shouldBe ScheduledManufacturingId(generatedManufacturingId)
    orderData.setOfManufacturing.head.info.materials.map(_.unitPrice) shouldBe List(UnitPrice(185000L))
    validOrFail(MaterialsUpdateRequest.example.toCommand(generatedManufacturingId, exampleCatalog, Nil))
    validOrFail(OrderUpdateRequest.example.toCommands)
    validOrFail(ManufacturingUpdateRequest.example.toCommands(generatedManufacturingId))
    validOrFail(TransitionRequest.example.toCommand)
    validOrFail(TaskProgressUpdateRequest.example.toCommands(generatedManufacturingId, id))
    validOrFail(EmployeeRequest.example.toDomain(id))
    validOrFail(ClosureRequest.example.toDomain(id))
    validOrFail(CustomerRequest.example.toDomain(id))
    validOrFail(TaskRequest.example.toDomain(id))
    validOrFail(ManufacturingRequest.example.toDomain(id))
    validOrFail(SupplierRequest.example.toDomain(id))
    validOrFail(MaterialRequest.example.toDomain(id))
    validOrFail(PurchaseRequest.example.toDomain)
    validOrFail(UnloadRequest.example.toDomain)
    validOrFail(AdjustmentRequest.example.toDomain)
    validOrFail(DeliveryNoteRequest.example.parse)
    validOrFail(CancelDeliveryNoteRequest.example.parse)

  they should "render valid ISO date-time examples in the generated OpenAPI document" in:
    val endpoints =
      PlanningEndpoints.all ++
        CustomerHttpApi.endpoints ++
        TaskHttpApi.endpoints ++
        ManufacturingHttpApi.endpoints ++
        OrderHttpApi.endpoints ++
        EmployeeHttpApi.endpoints ++
        CompanyCalendarHttpApi.endpoints ++
        SupplierHttpApi.endpoints ++
        MaterialHttpApi.endpoints ++
        StockHttpApi.endpoints ++
        WarehouseReportsHttpApi.endpoints ++
        DeliveryNoteHttpApi.endpoints
    val yaml = OpenAPIDocsInterpreter().toOpenAPI(endpoints, "RST Manager API", "0.1.0").toYaml

    yaml should include("bearerAuth")
    yaml should include("2026-06-15T00:00:00.000Z")
    yaml should include("2026-06-14T09:00:00.000Z")
    yaml should include("2026-06-19T17:00:00.000Z")
    yaml should include("2026-01-01T00:00:00.000Z")
    yaml should include("MFG-2026-001")
    yaml should include("/api/v1/warehouse/suppliers/{id}")
    yaml should include("/api/v1/warehouse/materials/{id}")
    yaml should include("includeArchived")
    yaml should include("18.5000")
    yaml should include("/api/v1/warehouse/materials/{id}/purchases")
    yaml should include("/api/v1/warehouse/materials/{id}/movements/{movementId}")
    yaml should include("/api/v1/warehouse/state")
    yaml should include("/api/v1/warehouse/flows")
    yaml should include("/api/v1/warehouse/supplier-spend")
    yaml should include("/api/v1/warehouse/export.xlsx")
    yaml should include("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    yaml should include("Content-Disposition")
    yaml should include("granularity")
    yaml should include("25.500")
    yaml should include("/api/v1/orders/{id}/manufacturings/{manufacturingId}/materials")
    yaml should include("12.5000")
    yaml should include("/api/v1/warehouse/delivery-notes/{id}/cancel")
    yaml should include("1234/A")
    yaml should include("/api/v1/company-calendar/closures/{id}")
    yaml should include("2026-08-10T00:00:00.000Z")
    yaml should not include "00000000-0000-0000-0000-000000000501"
    yaml should not include "startOn: string"
    yaml should not include "creationDate: string"
    yaml should not include "startDate: string"
end OpenApiExamplesTest
