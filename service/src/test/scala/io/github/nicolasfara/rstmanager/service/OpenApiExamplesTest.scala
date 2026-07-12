package io.github.nicolasfara.rstmanager.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.service.CustomerHttpApi
import io.github.nicolasfara.rstmanager.customer.service.CustomerHttpApi.CustomerRequest
import io.github.nicolasfara.rstmanager.hr.service.EmployeeHttpApi
import io.github.nicolasfara.rstmanager.hr.service.EmployeeHttpApi.EmployeeRequest
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.PlanningAttemptRequest
import io.github.nicolasfara.rstmanager.planning.service.PlanningEndpoints
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId.given
import io.github.nicolasfara.rstmanager.work.service.OrderDtos.{ OrderRequest, OrderUpdateRequest, TransitionRequest }
import io.github.nicolasfara.rstmanager.work.service.{ OrderHttpApi, TaskHttpApi }
import io.github.nicolasfara.rstmanager.work.service.TaskHttpApi.TaskRequest

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.DateTime
import sttp.apispec.openapi.circe.yaml.*
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class OpenApiExamplesTest extends AnyFlatSpecLike:
  private val id: UUID = UUID.fromString("00000000-0000-0000-0000-000000000999").nn
  private val generatedManufacturingId: ScheduledManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000998").nn
  private val now: DateTime = DateTime.parse("2026-06-14T09:30:00.000Z").nn

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  "OpenAPI request examples" should "be accepted by edge DTO validation" in:
    validOrFail(PlanningAttemptRequest.example.toDomain(now, id))
    val (orderData, _) = validOrFail(OrderRequest.example.toDomain(id, () => generatedManufacturingId))
    orderData.setOfManufacturing.head.info.id shouldBe generatedManufacturingId
    validOrFail(OrderUpdateRequest.example.toCommands)
    validOrFail(TransitionRequest.example.toCommand)
    validOrFail(EmployeeRequest.example.toDomain(id))
    validOrFail(CustomerRequest.example.toDomain(id))
    validOrFail(TaskRequest.example.toDomain(id))

  they should "render valid ISO date-time examples in the generated OpenAPI document" in:
    val endpoints =
      PlanningEndpoints.all ++
        CustomerHttpApi.endpoints ++
        TaskHttpApi.endpoints ++
        OrderHttpApi.endpoints ++
        EmployeeHttpApi.endpoints
    val yaml = OpenAPIDocsInterpreter().toOpenAPI(endpoints, "RST Manager API", "0.1.0").toYaml

    yaml should include("2026-06-15T00:00:00.000Z")
    yaml should include("2026-06-14T09:00:00.000Z")
    yaml should include("2026-06-19T17:00:00.000Z")
    yaml should include("2026-01-01T00:00:00.000Z")
    yaml should include("MFG-2026-001")
    yaml should not include "00000000-0000-0000-0000-000000000501"
    yaml should not include "startOn: string"
    yaml should not include "creationDate: string"
    yaml should not include "startDate: string"
end OpenApiExamplesTest
