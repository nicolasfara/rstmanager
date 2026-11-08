package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.{ Quantity, SignedQuantity, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialHttpApi.{ MaterialResponse, MaterialSupplierDto }
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier
import io.github.nicolasfara.rstmanager.warehouse.service.WarehouseHttpSupport.OrderLabel
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/**
 * JSON DTOs, tapir endpoints, and http4s server logic for the stock movements of a material, under `/warehouse/materials/{id}`: purchases, unloads,
 * adjustments (Operator), voids (Admin), and the movement list with the running balance (Viewer). Automatic consumptions by order manufacturings are
 * listed too, but only the reconciliation with the orders records them, and they cannot be voided.
 *
 * Quantities travel as decimal strings with 3 decimals, unit costs with 4, amounts with 2, days as `YYYY-MM-DD`. A movement day may be omitted: it
 * then is the business day the request is handled on. Error messages are in Italian.
 */
object StockHttpApi:
  import ApiError.ApiFailure
  import WarehouseHttpSupport.*

  /** Builds a movement command once the service has chosen the movement id. */
  type MovementCommand = MovementId => MaterialService.Command

  final case class PurchaseRequest(
      mpnCode: String,
      supplierId: UUID,
      quantity: String,
      unitCost: String,
      on: Option[String],
      note: Option[String],
  ):
    def toDomain: ValidatedNec[String, MovementCommand] =
      (
        field("MPN", MaterialInfo.normalizedCode(mpnCode).refineValidatedNec[MpnCode]),
        field("Quantità", Quantity.fromDecimalString(quantity).toValidatedNec),
        field("Costo unitario", UnitPrice.fromDecimalString(unitCost).toValidatedNec),
        parseOptionalDay(on, "Data"),
      ).mapN((code, parsedQuantity, cost, day) =>
        MaterialService.Command.RecordPurchase(_, code, SupplierId(supplierId), parsedQuantity, cost, normalized(note), day),
      )

  object PurchaseRequest:
    val example: PurchaseRequest =
      PurchaseRequest(
        "AC-152-NAVY",
        UUID.fromString("00000000-0000-0000-0000-000000000301").nn,
        "25.500",
        "3.5000",
        Some("2026-10-20"),
        Some("DDT 1234 del 20/10/2026"),
      )

  final case class UnloadRequest(quantity: String, on: Option[String], reason: String):
    def toDomain: ValidatedNec[String, MovementCommand] =
      (
        field("Quantità", Quantity.fromDecimalString(quantity).toValidatedNec),
        parseOptionalDay(on, "Data"),
        parseReason(reason, "Motivo"),
      ).mapN((parsedQuantity, day, parsedReason) => MaterialService.Command.Unload(_, parsedQuantity, parsedReason, day))

  object UnloadRequest:
    val example: UnloadRequest = UnloadRequest("2.000", Some("2026-10-21"), "Campionatura per cliente")

  final case class AdjustmentRequest(delta: String, unitCost: Option[String], on: Option[String], reason: String):
    def toDomain: ValidatedNec[String, MovementCommand] =
      (
        field("Variazione", SignedQuantity.fromDecimalString(delta).toValidatedNec),
        unitCost.map(_.trim.nn).filter(_.nonEmpty).traverse(cost => field("Costo unitario", UnitPrice.fromDecimalString(cost).toValidatedNec)),
        parseOptionalDay(on, "Data"),
        parseReason(reason, "Motivo"),
      ).mapN((parsedDelta, cost, day, parsedReason) => MaterialService.Command.Adjust(_, parsedDelta, cost, parsedReason, day))

  object AdjustmentRequest:
    val example: AdjustmentRequest = AdjustmentRequest("40.000", Some("3.2000"), Some("2026-10-01"), "Giacenza iniziale all'avvio del magazzino")

  /** The movement just recorded and the material with its updated stock. */
  final case class MovementRecordedResponse(movementId: UUID, material: MaterialResponse)

  /**
   * A movement in the list.
   *
   * @param kind
   *   `purchase`, `unload`, `adjustment`, or `consumption` (automatic consumption by an order manufacturing: an outflow, or its reversal when `delta`
   *   is positive).
   * @param quantity
   *   Magnitude of the change (non-negative).
   * @param delta
   *   Signed change of the stock.
   * @param unitCost
   *   Unit cost the movement is valued at: the purchase cost, the cost of an incoming adjustment, or for an outgoing movement or a consumption
   *   reversal the highest cost of its day; absent when there is none.
   * @param value
   *   Signed value of the movement (negative when stock goes out); absent without a cost.
   * @param balance
   *   Balance after the movement (a voided movement leaves it unchanged).
   * @param orderId
   *   Order of a consumption.
   * @param manufacturingId
   *   Scheduled manufacturing (inside the order) of a consumption.
   * @param orderNumber
   *   Number of the order of a consumption, when the order can be loaded.
   * @param manufacturingCode
   *   Template code of the manufacturing of a consumption, when it is still in the order.
   * @param deliveryNoteId
   *   Delivery note (DDT) of a purchase it recorded: such a purchase changes only with the note, it cannot be voided by hand.
   * @param deliveryNoteNumber
   *   Number of the delivery note of a purchase, as it was when the purchase was recorded.
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
      supplier: Option[MaterialSupplierDto],
      mpnCode: Option[String],
      reason: Option[String],
      note: Option[String],
      voided: Boolean,
      voidedOn: Option[String],
      voidReason: Option[String],
      balance: String,
      orderId: Option[UUID],
      manufacturingId: Option[UUID],
      orderNumber: Option[String],
      manufacturingCode: Option[String],
      deliveryNoteId: Option[UUID],
      deliveryNoteNumber: Option[String],
  )

  object MovementResponse:
    def fromDomain(
        line: LedgerLine,
        ledger: StockLedger,
        suppliers: Map[SupplierId, StoredSupplier],
        orders: Map[OrderId, OrderLabel],
    ): MovementResponse =
      val movement = line.movement
      val unitCost = ledger.unitCostOf(movement)
      val base = MovementResponse(
        movement.id.value,
        movement.sequence,
        "adjustment",
        formatDay(movement.on),
        movement.delta.abs.toDecimalString,
        movement.delta.toDecimalString,
        unitCost.map(_.toDecimalString),
        line.value.map(_.toDecimalString),
        None,
        None,
        None,
        None,
        movement.voided,
        movement.voiding.map(voiding => formatDay(voiding.voidedOn)),
        movement.voiding.map(voiding => voiding.reason: String),
        line.balance.toDecimalString,
        None,
        None,
        None,
        None,
        None,
        None,
      )
      movement.kind match
        case MovementKind.Purchase(code, supplierId, _, _, note, deliveryNote) =>
          base.copy(
            kind = "purchase",
            supplier = Some(MaterialSupplierDto.of(supplierId, suppliers)),
            mpnCode = Some(code),
            note = note,
            deliveryNoteId = deliveryNote.map(_.id.value),
            deliveryNoteNumber = deliveryNote.map(ref => ref.number: String),
          )
        case MovementKind.Unload(reason) => base.copy(kind = "unload", reason = Some(reason))
        case MovementKind.Adjustment(_, reason) => base.copy(reason = Some(reason))
        case MovementKind.Consumption(ref) =>
          val label = orders.get(ref.orderId)
          base.copy(
            kind = "consumption",
            orderId = Some(ref.orderId.value),
            manufacturingId = Some(ref.manufacturingId.value),
            orderNumber = label.map(_.number),
            manufacturingCode = label.flatMap(_.manufacturingCodes.get(ref.manufacturingId)),
          )
      end match
    end fromDomain
  end MovementResponse

  /**
   * The movements of a material from `from` to `to` (both optional, inclusive) in chronological order (day, then recording order), with the balance
   * before the first day and after the last.
   */
  final case class MovementsResponse(
      materialId: UUID,
      unit: String,
      from: Option[String],
      to: Option[String],
      openingBalance: String,
      closingBalance: String,
      movements: List[MovementResponse],
  )

  given Codec[PurchaseRequest] = deriveCodec
  given Codec[UnloadRequest] = deriveCodec
  given Codec[AdjustmentRequest] = deriveCodec
  given Codec[MovementRecordedResponse] = deriveCodec
  given Codec[MovementResponse] = deriveCodec
  given Codec[MovementsResponse] = deriveCodec
  given Schema[PurchaseRequest] = Schema.derived
  given Schema[UnloadRequest] = Schema.derived
  given Schema[AdjustmentRequest] = Schema.derived
  given Schema[MovementRecordedResponse] = Schema.derived
  given Schema[MovementResponse] = Schema.derived
  given Schema[MovementsResponse] = Schema.derived

  private val material = "warehouse" / "materials" / path[UUID]("id")

  val recordPurchase: Secured.SecuredEndpoint[(UUID, PurchaseRequest), MovementRecordedResponse] =
    Secured.base.post
      .in(material / "purchases")
      .tag("Warehouse")
      .summary("Record a purchase under one of the material's MPNs")
      .in(jsonBody[PurchaseRequest].example(PurchaseRequest.example))
      .out(jsonBody[MovementRecordedResponse])

  val recordUnload: Secured.SecuredEndpoint[(UUID, UnloadRequest), MovementRecordedResponse] =
    Secured.base.post
      .in(material / "unloads")
      .tag("Warehouse")
      .summary("Take stock out by hand (rejected if the stock would go below zero)")
      .in(jsonBody[UnloadRequest].example(UnloadRequest.example))
      .out(jsonBody[MovementRecordedResponse])

  val recordAdjustment: Secured.SecuredEndpoint[(UUID, AdjustmentRequest), MovementRecordedResponse] =
    Secured.base.post
      .in(material / "adjustments")
      .tag("Warehouse")
      .summary("Correct the stock by a signed quantity")
      .in(jsonBody[AdjustmentRequest].example(AdjustmentRequest.example))
      .out(jsonBody[MovementRecordedResponse])

  val voidMovement: Secured.SecuredEndpoint[(UUID, UUID, Option[String]), Unit] =
    Secured.base.delete
      .in(material / "movements" / path[UUID]("movementId"))
      .in(query[Option[String]]("reason").description("Why the movement is voided (required)."))
      .tag("Warehouse")
      .summary(
        "Void a stock movement (automatic consumptions by order manufacturings cannot be voided: 409 consumption-not-voidable; " +
          "purchases of a delivery note neither: 409 delivery-note-movement-not-voidable)",
      )
      .out(statusCode(StatusCode.NoContent))

  val listMovements: Secured.SecuredEndpoint[(UUID, Option[String], Option[String]), MovementsResponse] =
    Secured.base.get
      .in(material / "movements")
      .in(query[Option[String]]("from").description("First day, YYYY-MM-DD (inclusive)."))
      .in(query[Option[String]]("to").description("Last day, YYYY-MM-DD (inclusive)."))
      .tag("Warehouse")
      .summary("List the stock movements of a material with the running balance")
      .out(jsonBody[MovementsResponse])

  def endpoints: List[AnyEndpoint] = List(recordPurchase, recordUnload, recordAdjustment, voidMovement, listMovements)

  def routes(store: MaterialApp.Store, suppliers: SupplierApp.Store, orders: OrderApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] =
    List(
      security.secure(recordPurchase, Role.Operator)((id, request) => recordLogic(store, suppliers)(id, request.toDomain)),
      security.secure(recordUnload, Role.Operator)((id, request) => recordLogic(store, suppliers)(id, request.toDomain)),
      security.secure(recordAdjustment, Role.Operator)((id, request) => recordLogic(store, suppliers)(id, request.toDomain)),
      security.secure(voidMovement, Role.Admin)(voidLogic(store)),
      security.secure(listMovements, Role.Viewer)(listLogic(store, suppliers, orders)),
    )

  private def recordLogic(store: MaterialApp.Store, suppliers: SupplierApp.Store)(
      id: UUID,
      parsed: ValidatedNec[String, MovementCommand],
  ): IO[Either[ApiFailure, MovementRecordedResponse]] =
    parsed.toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right(command) =>
        withMaterial(store, id) { stored =>
          MaterialApp.recordMovement(store, MaterialId(id), command).flatMap {
            case Left(error) => IO.pure(materialFailure(error, id, Some(stored.info)).asLeft)
            case Right(movementId) => MaterialHttpApi.readLogic(store, suppliers)(id).map(_.map(MovementRecordedResponse(movementId.value, _)))
          }
        }

  private def voidLogic(store: MaterialApp.Store)(id: UUID, movementId: UUID, reason: Option[String]): IO[Either[ApiFailure, Unit]] =
    parseReason(reason.getOrElse(""), "Motivo dell'annullo").toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right(parsedReason) =>
        withMaterial(store, id)(stored =>
          MaterialApp
            .voidMovement(store, MaterialId(id), MovementId(movementId), parsedReason)
            .map(_.leftMap(materialFailure(_, id, Some(stored.info)))),
        )

  private def listLogic(store: MaterialApp.Store, suppliers: SupplierApp.Store, orders: OrderApp.Store)(
      id: UUID,
      from: Option[String],
      to: Option[String],
  ): IO[Either[ApiFailure, MovementsResponse]] =
    (parseOptionalDay(from, "Dal"), parseOptionalDay(to, "Al")).tupled.andThen {
      case (Some(start), Some(end)) if start.isAfter(end) => periodReversed(start, end).invalidNec
      case period => period.validNec
    }.toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right((start, end)) =>
        withMaterial(store, id) { stored =>
          val ledger = stored.material.ledger
          val purchaseSuppliers = ledger.movements.toList.collect {
            case StockMovement(_, _, _, _, MovementKind.Purchase(_, supplierId, _, _, _, _), _) => supplierId
          }
          (MaterialHttpApi.suppliersById(suppliers, purchaseSuppliers), orderLabels(orders, ledger.movements.toList)).mapN((found, labels) =>
            movements(stored, start, end, found, labels).asRight,
          )
        }

  /** The movements of `stored` from `start` to `end`, each bound optional. */
  private def movements(
      stored: MaterialApp.StoredMaterial,
      start: Option[LocalDate],
      end: Option[LocalDate],
      suppliers: Map[SupplierId, StoredSupplier],
      orders: Map[OrderId, OrderLabel],
  ): MovementsResponse =
    val ledger = stored.material.ledger
    val listed = ledger.lines.filter(line => start.forall(!line.movement.on.isBefore(_)) && end.forall(!line.movement.on.isAfter(_)))
    val opening = start.fold(SignedQuantity.zero)(day => ledger.balanceAt(day.minusDays(1)))
    MovementsResponse(
      stored.info.id.value,
      stored.info.unit.wireName,
      start.map(formatDay),
      end.map(formatDay),
      opening.toDecimalString,
      listed.lastOption.fold(opening)(_.balance).toDecimalString,
      listed.map(MovementResponse.fromDomain(_, ledger, suppliers, orders)),
    )

  /** Runs `logic` on the material `id` (active or archived), or answers 404; unexpected failures become a 500. */
  private def withMaterial[R](store: MaterialApp.Store, id: UUID)(
      logic: MaterialApp.StoredMaterial => IO[Either[ApiFailure, R]],
  ): IO[Either[ApiFailure, R]] =
    MaterialApp
      .find(store, MaterialId(id))
      .flatMap {
        case Some(stored) => logic(stored)
        case None => IO.pure(materialNotFound(id).asLeft)
      }
      .handleError(error => internal(error).asLeft)

  private def parseReason(raw: String, name: String): ValidatedNec[String, String :| MovementReason] =
    field(name, raw.trim.nn.refineValidatedNec[MovementReason])
end StockHttpApi
