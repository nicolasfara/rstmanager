package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialId, MaterialRecord }
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/**
 * JSON DTOs, tapir endpoint, and http4s server logic for the material requirements of the open orders (`/warehouse/material-requirements`): what each
 * material has in stock today, what the manufacturings not completed of the orders in progress or suspended still need, what is left, and which
 * manufacturings the stock cannot cover.
 *
 * The figures are computed by the pure [[MaterialRequirements]] over the orders and materials read through the [[WarehouseConsumptionGateway]]. It is
 * a pure read: nothing is persisted, and it is computed on request only. Quantities are decimal strings with 3 decimals, in each material's unit.
 */
object MaterialRequirementsHttpApi:
  import ApiError.ApiFailure
  import WarehouseHttpSupport.*

  /** Stock, commitment and availability of one material (`available` is negative when the open orders need more than the stock). */
  final case class MaterialRequirementResponse(
      materialId: UUID,
      internalCode: String,
      description: String,
      unit: String,
      stock: String,
      committed: String,
      available: String,
  )

  object MaterialRequirementResponse:
    def fromDomain(requirement: MaterialRequirement, materials: Map[MaterialId, MaterialRecord]): Option[MaterialRequirementResponse] =
      materials.get(requirement.materialId).map { material =>
        MaterialRequirementResponse(
          requirement.materialId.value,
          material.info.internalCode,
          material.info.description,
          material.info.unit.wireName,
          requirement.stock.toDecimalString,
          requirement.committed.toDecimalString,
          requirement.available.toDecimalString,
        )
      }

  /** A manufacturing the stock of a material cannot cover: `missing` (at most `required`) has to be ordered. */
  final case class MaterialShortageResponse(
      orderId: UUID,
      orderNumber: String,
      manufacturingId: UUID,
      manufacturingCode: String,
      materialId: UUID,
      required: String,
      missing: String,
  )

  object MaterialShortageResponse:
    def fromDomain(shortage: MaterialShortage): MaterialShortageResponse =
      MaterialShortageResponse(
        shortage.orderId.value,
        shortage.orderNumber,
        shortage.manufacturingId.value,
        shortage.manufacturingCode,
        shortage.materialId.value,
        shortage.required.toDecimalString,
        shortage.missing.toDecimalString,
      )

  /** A material line of an open order naming a material the warehouse does not know. */
  final case class UnknownMaterialDemandResponse(
      orderId: UUID,
      orderNumber: String,
      manufacturingId: UUID,
      manufacturingCode: String,
      materialId: UUID,
      required: String,
  )

  object UnknownMaterialDemandResponse:
    def fromDomain(demand: UnknownMaterialDemand): UnknownMaterialDemandResponse =
      UnknownMaterialDemandResponse(
        demand.orderId.value,
        demand.orderNumber,
        demand.manufacturingId.value,
        demand.manufacturingCode,
        demand.materialId.value,
        demand.required.toDecimalString,
      )

  /**
   * The requirements on day `at` (YYYY-MM-DD): every material of the warehouse (archived ones included) sorted by internal code, the shortages in
   * allocation order per material, and the demand on unknown materials.
   */
  final case class MaterialRequirementsResponse(
      at: String,
      materials: List[MaterialRequirementResponse],
      shortages: List[MaterialShortageResponse],
      unknownMaterials: List[UnknownMaterialDemandResponse],
  )

  object MaterialRequirementsResponse:
    def fromDomain(at: LocalDate, requirements: Requirements, materials: List[MaterialRecord]): MaterialRequirementsResponse =
      val byId = materials.map(material => material.info.id -> material).toMap
      MaterialRequirementsResponse(
        formatDay(at),
        requirements.materials.flatMap(MaterialRequirementResponse.fromDomain(_, byId)),
        requirements.shortages.map(MaterialShortageResponse.fromDomain),
        requirements.unknownMaterials.map(UnknownMaterialDemandResponse.fromDomain),
      )

  given Codec[MaterialRequirementResponse] = deriveCodec
  given Codec[MaterialShortageResponse] = deriveCodec
  given Codec[UnknownMaterialDemandResponse] = deriveCodec
  given Codec[MaterialRequirementsResponse] = deriveCodec
  given Schema[MaterialRequirementResponse] = Schema.derived
  given Schema[MaterialShortageResponse] = Schema.derived
  given Schema[UnknownMaterialDemandResponse] = Schema.derived
  given Schema[MaterialRequirementsResponse] = Schema.derived

  val requirements: Secured.SecuredEndpoint[Option[UUID], MaterialRequirementsResponse] =
    Secured.base.get
      .in("warehouse" / "material-requirements")
      .in(
        query[Option[UUID]]("excludeOrder")
          .description("Order left out of the demand: the form editing it counts its own material lines, which would otherwise be committed twice."),
      )
      .tag("Warehouse")
      .summary("Stock committed by the open orders, availability per material, and the manufacturings short of material")
      .out(jsonBody[MaterialRequirementsResponse])

  def endpoints: List[AnyEndpoint] = List(requirements)

  def routes(orders: OrderApp.Store, materials: MaterialApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] =
    List(security.secure(requirements, Role.Viewer)(requirementsLogic(WarehouseConsumptionGateway.fromStores(orders, materials), today)))

  /**
   * The requirements of every order of the registry against every material, with the stock at the end of `today`.
   *
   * Only the orders in progress or suspended need materials, but no read model tells them apart without loading them: the order registry lists every
   * order ever created (cancelled ones included), like the one the planning lists through `OrderApp.list`. They are therefore loaded in one batched
   * query ([[WarehouseConsumptionGateway.loadOrders]], over the cached `StreamReader`, which only fetches the events appended since the last read)
   * and [[MaterialRequirements.compute]] drops the closed ones. An index of the open orders would need a new journal namespace.
   */
  def requirementsLogic(gateway: WarehouseConsumptionGateway, today: IO[LocalDate])(
      excludeOrder: Option[UUID],
  ): IO[Either[ApiFailure, MaterialRequirementsResponse]] =
    val computed =
      for
        day <- today
        ids <- gateway.orderIds
        orders <- gateway.loadOrders(ids)
        materials <- gateway.listMaterials
      yield MaterialRequirementsResponse.fromDomain(
        day,
        MaterialRequirements.compute(orders, materials, day, excludeOrder.map(OrderId(_))),
        materials,
      )
    computed.attempt.map(_.leftMap(internal))
end MaterialRequirementsHttpApi
