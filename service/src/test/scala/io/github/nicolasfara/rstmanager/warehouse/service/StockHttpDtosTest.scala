package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierId }
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialHttpApi.MaterialSupplierDto
import io.github.nicolasfara.rstmanager.warehouse.service.StockHttpApi.*
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier
import io.github.nicolasfara.rstmanager.warehouse.service.WarehouseHttpSupport.OrderLabel
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class StockHttpDtosTest extends AnyFlatSpecLike:
  private val supplierId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000301").nn
  private val movementId: MovementId = MovementId(UUID.fromString("00000000-0000-0000-0000-000000000701").nn)
  private val reason: String :| MovementReason = "Sfrido"

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)
  private def errors[A](value: ValidatedNec[String, A]): List[String] = value.fold(_.toChain.toList, result => fail(s"Unexpectedly valid: $result"))

  "PurchaseRequest.toDomain" should "parse the decimals, normalize the MPN code, and keep an omitted day for the server" in:
    val command = valid(PurchaseRequest(" ac-152 ", supplierId, "2.5", "3.7", None, Some("  ")).toDomain)(movementId)
    command shouldEqual MaterialService.Command.RecordPurchase(
      movementId,
      "AC-152",
      SupplierId(supplierId),
      Quantity.applyUnsafe(2500L),
      UnitPrice.applyUnsafe(37000L),
      None,
      None,
    )
    valid(PurchaseRequest.example.toDomain)(movementId) match
      case purchase: MaterialService.Command.RecordPurchase => purchase.on shouldEqual Some(LocalDate.parse("2026-10-20"))
      case other => fail(s"Unexpected command: $other")

  it should "report every malformed field at once, in Italian" in:
    val reported = errors(PurchaseRequest(" ", supplierId, "-1", "1,5", Some("20/10/2026"), None).toDomain)
    reported.map(_.takeWhile(_ != ':')) shouldEqual List("MPN", "Quantità", "Costo unitario", "Data")
    reported should contain("Data: '20/10/2026' non è una data valida (usa il formato AAAA-MM-GG)")
    reported should contain("MPN: Il codice MPN non può essere vuoto")

  "UnloadRequest and AdjustmentRequest" should "need a reason and accept a signed delta with an optional cost" in:
    errors(UnloadRequest("1", None, "  ").toDomain) shouldEqual List("Motivo: Il motivo non può essere vuoto")
    errors(AdjustmentRequest("x", Some("-1"), None, "Sfrido").toDomain).map(_.takeWhile(_ != ':')) shouldEqual List("Variazione", "Costo unitario")
    valid(AdjustmentRequest("-1.5", Some(" "), Some("2026-10-01"), " Sfrido ").toDomain)(movementId) shouldEqual
      MaterialService.Command.Adjust(movementId, SignedQuantity.applyUnsafe(-1500L), None, reason, Some(LocalDate.parse("2026-10-01")))
    valid(AdjustmentRequest.example.toDomain)
    valid(UnloadRequest.example.toDomain)

  "MovementResponse" should "show the kind, the signed delta, the cost used, the supplier, and the running balance" in:
    val ledger = StockLedger.empty
      .appended(movementId, LocalDate.parse("2026-10-01"), SignedQuantity.applyUnsafe(2000L), purchaseOf(2000L, 30000L))
      .appended(MovementId(new UUID(0L, 2L)), LocalDate.parse("2026-10-02"), SignedQuantity.applyUnsafe(-500L), MovementKind.Unload(reason))
      .appended(MovementId(new UUID(0L, 3L)), LocalDate.parse("2026-10-02"), SignedQuantity.applyUnsafe(100L), MovementKind.Adjustment(None, reason))
      .voided(MovementId(new UUID(0L, 3L)), MovementVoid(reason, LocalDate.parse("2026-10-03")))
    val supplier = Supplier.createSupplier(SupplierId(supplierId), "Tessuti Nautici", None, None, None, None, None).toOption.toList
    val suppliers = supplier.map(found => found.id -> StoredSupplier(found, deleted = false)).toMap
    val responses = ledger.lines.map(MovementResponse.fromDomain(_, ledger, suppliers, Map.empty))
    responses.map(r => (r.kind, r.quantity, r.delta, r.unitCost, r.value, r.balance, r.voided)) shouldEqual List(
      ("purchase", "2.000", "2.000", Some("3.0000"), Some("6.00"), "2.000", false),
      ("unload", "0.500", "-0.500", Some("3.0000"), Some("-1.50"), "1.500", false),
      ("adjustment", "0.100", "0.100", None, None, "1.500", true),
    )
    responses.headOption.flatMap(_.supplier) shouldEqual Some(MaterialSupplierDto(supplierId, "Tessuti Nautici", active = true))
    responses.lastOption.flatMap(_.voidedOn) shouldEqual Some("2026-10-03")
    responses.headOption.map(_.asJson.hcursor.downField("delta").as[String]) shouldEqual Some(Right("2.000"))

  it should "list automatic consumptions with their order, valuing a reversal at the highest cost of its day" in:
    val ref = ConsumptionRef(OrderId(new UUID(9L, 1L)), ScheduledManufacturingId(new UUID(9L, 2L)))
    val ledger = StockLedger.empty
      .appended(movementId, LocalDate.parse("2026-10-01"), SignedQuantity.applyUnsafe(2000L), purchaseOf(2000L, 30000L))
      .consumptionSet(ref, Quantity.applyUnsafe(2500L), LocalDate.parse("2026-10-02"))
      .consumptionSet(ref, Quantity.applyUnsafe(1000L), LocalDate.parse("2026-10-03"))
    val labels = Map(ref.orderId -> OrderLabel("2026-014", Map(ref.manufacturingId -> "TND-01")))
    val responses = ledger.lines.drop(1).map(MovementResponse.fromDomain(_, ledger, Map.empty, labels))
    responses.map(r => (r.kind, r.delta, r.unitCost, r.value, r.balance)) shouldEqual List(
      ("consumption", "-2.500", Some("3.0000"), Some("-7.50"), "-0.500"),
      ("consumption", "1.500", Some("3.0000"), Some("4.50"), "1.000"),
    )
    responses.map(r => (r.orderId, r.manufacturingId, r.orderNumber, r.manufacturingCode)).distinct shouldEqual
      List((Some(ref.orderId.value), Some(ref.manufacturingId.value), Some("2026-014"), Some("TND-01")))
    ledger.lines.lift(1).map(MovementResponse.fromDomain(_, ledger, Map.empty, Map.empty).orderNumber) shouldEqual Some(None)
    val failure = WarehouseHttpSupport.materialFailure(MaterialError.ConsumptionNotVoidable(movementId), supplierId, None)
    failure._1.code shouldEqual 409
    failure._2.code shouldEqual "consumption-not-voidable"
    failure._2.message should include("scarico automatico")

  "WarehouseHttpSupport" should "parse report periods strictly and cap their length" in:
    WarehouseHttpSupport.parsePeriod(Some("2026-01-01"), Some("2026-12-31")).toOption shouldEqual
      Some((LocalDate.parse("2026-01-01"), LocalDate.parse("2026-12-31")))
    errors(WarehouseHttpSupport.parsePeriod(None, Some("2026-1-5"))) shouldEqual List(
      "Dal: la data di inizio è obbligatoria",
      "Al: '2026-1-5' non è una data valida (usa il formato AAAA-MM-GG)",
    )
    errors(WarehouseHttpSupport.parsePeriod(Some("2026-02-01"), Some("2026-01-31"))) shouldEqual
      List("Dal: la data di inizio (01/02/2026) è successiva a quella di fine (31/01/2026)")
    errors(WarehouseHttpSupport.parsePeriod(Some("2020-01-01"), Some("2025-01-01"))) shouldEqual List("Al: il periodo non può superare 5 anni")
    WarehouseHttpSupport.parsePeriod(Some("2020-01-01"), Some("2024-12-31")).isValid shouldEqual true

  it should "explain stock rejections in Italian, naming the material" in:
    val info = MaterialInfo
      .createMaterialInfo(MaterialId(supplierId), "tes-1", "Tessuto", None, None, UnitOfMeasure.LinearMeters, UnitPrice.zero, Nil)
      .toOption
    val failure = WarehouseHttpSupport.materialFailure(
      MaterialError.InsufficientStock(LocalDate.parse("2026-10-20"), SignedQuantity.applyUnsafe(2000L)),
      supplierId,
      info,
    )
    val body = failure._2
    failure._1.code shouldEqual 409
    body.code shouldEqual "insufficient-stock"
    body.message should include("TES-1")
    body.details shouldEqual List("Disponibile dal 20/10/2026: 2 m")
    WarehouseHttpSupport
      .materialFailure(MaterialError.InsufficientStock(LocalDate.parse("2026-10-20"), SignedQuantity.applyUnsafe(1234500L)), supplierId, info)
      ._2
      .details shouldEqual List("Disponibile dal 20/10/2026: 1.234,5 m")
    WarehouseHttpSupport
      .materialFailure(MaterialError.InsufficientStock(LocalDate.parse("2026-10-20"), SignedQuantity.applyUnsafe(2500L)), supplierId, None)
      ._2
      .details shouldEqual List("Disponibile dal 20/10/2026: 2,5")
    WarehouseHttpSupport.materialFailure(MaterialError.StockLimitExceeded, supplierId, info)._2.message should include("(1.000.000.000 m)")
    WarehouseHttpSupport.materialFailure(MaterialError.NonPositiveQuantity, supplierId, info)._2.details shouldEqual
      List("Quantità: la quantità deve essere diversa da zero")
    WarehouseHttpSupport.materialFailure(MaterialError.MaterialIsArchived, supplierId, info)._2.message shouldEqual
      "Il materiale TES-1 è archiviato e non può più essere modificato."

  "WarehouseHttpSupport formatting" should "write quantities, units, and euro the Italian way" in:
    import WarehouseHttpSupport.*
    displayQuantity(SignedQuantity.applyUnsafe(2500L), UnitOfMeasure.LinearMeters) shouldEqual "2,5 m"
    displayQuantity(SignedQuantity.applyUnsafe(3000L), UnitOfMeasure.Pieces) shouldEqual "3 pz"
    displayQuantity(SignedQuantity.applyUnsafe(-1234567L), UnitOfMeasure.SquareMeters) shouldEqual "-1.234,567 m²"
    displayQuantity(SignedQuantity.applyUnsafe(1L), UnitOfMeasure.Kilograms) shouldEqual "0,001 kg"
    displayQuantity(SignedQuantity.zero) shouldEqual "0"
    displayEuro(Amount.applyUnsafe(123456789L)) shouldEqual "1.234.567,89 €"
    displayEuro(Amount.applyUnsafe(-5L)) shouldEqual "-0,05 €"
    displayUnitPrice(UnitPrice.applyUnsafe(37000L)) shouldEqual "3,70 €"
    displayUnitPrice(UnitPrice.applyUnsafe(37512L)) shouldEqual "3,7512 €"

  private def purchaseOf(quantity: Long, cost: Long): MovementKind =
    MovementKind.Purchase(
      "AC-152",
      SupplierId(supplierId),
      UnitPrice.applyUnsafe(cost),
      Valuation.amount(Quantity.applyUnsafe(quantity), UnitPrice.applyUnsafe(cost)),
      None,
    )
end StockHttpDtosTest
