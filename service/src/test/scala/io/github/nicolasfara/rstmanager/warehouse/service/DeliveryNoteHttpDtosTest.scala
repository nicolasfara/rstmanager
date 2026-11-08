package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierId }
import io.github.nicolasfara.rstmanager.warehouse.service.DeliveryNoteHttpApi.*
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp.StoredMaterial
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class DeliveryNoteHttpDtosTest extends AnyFlatSpecLike:
  private val today: LocalDate = LocalDate.parse("2026-11-05")
  private val supplierUuid: UUID = UUID.fromString("00000000-0000-0000-0000-000000000301").nn
  private val supplierId: SupplierId = SupplierId(supplierUuid)
  private val noteId: DeliveryNoteId = DeliveryNoteId(UUID.fromString("00000000-0000-0000-0000-000000000b01").nn)
  private val fabricId: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000401").nn)
  private val screwsId: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000402").nn)
  private val otherId: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000403").nn)

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)
  private def errors[A](value: ValidatedNec[String, A]): List[String] = value.fold(_.toChain.toList, result => fail(s"Unexpectedly valid: $result"))

  private val supplier: Supplier = valid(Supplier.createSupplier(supplierId, "Tessuti Nautici", None, None, None, None, None))

  private def material(id: MaterialId, code: String, unit: UnitOfMeasure, mpns: List[String]): StoredMaterial =
    val info = valid(
      MaterialInfo.createMaterialInfo(
        id,
        code,
        s"Materiale $code",
        None,
        None,
        unit,
        UnitPrice.applyUnsafe(10000L),
        mpns.map(mpn => valid(Mpn.createMpn(mpn, supplierId, None, Some(UnitPrice.applyUnsafe(35000L))))),
      ),
    )
    StoredMaterial(MaterialRecord.created(info, LocalDate.parse("2026-01-01")), None)

  private val materials: Map[MaterialId, StoredMaterial] = List(
    material(fabricId, "TES-001", UnitOfMeasure.LinearMeters, List("AC-152-NAVY")),
    material(screwsId, "VIT-M6", UnitOfMeasure.Pieces, Nil),
    material(otherId, "VIT-M8", UnitOfMeasure.Pieces, List("VIT-M8-20")),
  ).map(stored => stored.info.id -> stored).toMap

  private def context(notes: List[DeliveryNoteAggregate], existing: Option[DeliveryNoteRecord]): CheckContext =
    CheckContext(today, Some(StoredSupplier(supplier, deleted = false)), materials, notes, existing)

  private def lineRequest(materialId: MaterialId, code: String, newMpn: Boolean, quantity: String): DeliveryNoteLineRequest =
    DeliveryNoteLineRequest(materialId.value, code, newMpn, quantity, "3.5")

  private val request: DeliveryNoteRequest =
    DeliveryNoteRequest(
      " 1234/A ",
      "2026-11-02",
      supplierUuid,
      Some(" "),
      List(lineRequest(fabricId, " ac-152-navy ", newMpn = false, "25.5"), lineRequest(screwsId, "vit-m6-20", newMpn = true, "100")),
    )

  private def info(notes: List[DeliveryNoteAggregate]): DeliveryNoteInfo = valid(check(noteId, valid(request.parse), context(notes, None)))

  "DeliveryNoteRequest.parse" should "parse the decimals and the day, normalize the codes, and number the lines" in:
    val parsed = valid(request.parse)
    (parsed.number: String) shouldEqual "1234/A"
    parsed.date shouldEqual LocalDate.parse("2026-11-02")
    parsed.note shouldEqual None
    parsed.lines.map(line => (line.n, line.mpnCode: String, line.quantity.value, line.unitCost.value)) shouldEqual
      List((1, "AC-152-NAVY", 25500L, 35000L), (2, "VIT-M6-20", 100000L, 35000L))

  it should "report every malformed field at once, line by line, in Italian" in:
    val bad = request.copy(number = " ", date = "02/11/2026", lines = List(DeliveryNoteLineRequest(fabricId.value, " ", false, "0", "x")))
    errors(bad.parse).map(_.takeWhile(_ != ':')) shouldEqual List("Numero", "Data", "Riga 1", "Riga 1", "Riga 1")
    errors(bad.parse) should contain("Riga 1: Quantità: la quantità deve essere maggiore di zero")
    errors(request.copy(lines = Nil).parse) shouldEqual List("Righe: il DDT deve avere almeno una riga")

  it should "decode the wire shape, the note being optional" in:
    val json = s"""{"number":"7","date":"2026-11-02","supplierId":"$supplierUuid","lines":[{"materialId":"${fabricId.value}",""" +
      """"mpnCode":"AC-152-NAVY","newMpn":false,"quantity":"1.000","unitCost":"3.5000"}]}"""
    decode[DeliveryNoteRequest](json).map(_.note) shouldEqual Right(None)

  "DeliveryNoteHttpApi.check" should "build the note with the unit of each material" in:
    val checked = info(Nil)
    checked.lines.map(line => (line.materialId, line.unit)) shouldEqual List(fabricId -> UnitOfMeasure.LinearMeters, screwsId -> UnitOfMeasure.Pieces)
    checked.total shouldEqual Amount.applyUnsafe(43925L) // 25.5 m × 3.50 € + 100 pz × 3.50 €

  it should "reject an unknown MPN, an MPN taken by another material, a missing material, and fractional pieces" in:
    val parsed = valid(
      request
        .copy(lines =
          List(
            lineRequest(fabricId, "NOPE", newMpn = false, "1"),
            lineRequest(screwsId, "VIT-M8-20", newMpn = true, "1"),
            lineRequest(MaterialId(new UUID(1L, 1L)), "X", newMpn = true, "1"),
            lineRequest(screwsId, "VIT-M6-20", newMpn = true, "1.5"),
          ),
        )
        .parse,
    )
    errors(check(noteId, parsed, context(Nil, None))) shouldEqual List(
      "Riga 1: MPN: l'MPN NOPE del fornitore non è fra gli MPN del materiale TES-001",
      "Riga 2: MPN: l'MPN VIT-M8-20 del fornitore è già usato dal materiale VIT-M8",
      "Riga 3: il materiale indicato non esiste",
      "Riga 4: Quantità: il materiale VIT-M6 è gestito a pezzi, la quantità deve essere un numero intero",
    )

  it should "reject a deleted supplier, a future day, and a number already used by the supplier that year" in:
    val other = DeliveryNoteInfo(DeliveryNoteId(new UUID(2L, 2L)), "1234/a", LocalDate.parse("2026-03-01"), supplierId, None, Nil)
    val notes = List(DeliveryNoteAggregate.Registered(DeliveryNoteRecord(other, LocalDate.parse("2026-03-01"), None)))
    val parsed = valid(request.copy(date = "2026-11-06").parse)
    errors(check(noteId, parsed, context(notes, None).copy(supplier = Some(StoredSupplier(supplier, deleted = true))))) shouldEqual List(
      "Fornitore: il fornitore Tessuti Nautici è stato eliminato",
      "Data: la data 06/11/2026 è successiva a oggi (05/11/2026)",
      "Numero: esiste già il DDT n. 1234/a del 01/03/2026 del fornitore Tessuti Nautici",
    )
    val cancelled = List(DeliveryNoteAggregate.Cancelled(DeliveryNoteRecord(other, LocalDate.parse("2026-03-01"), None), today, "Doppio"))
    check(noteId, valid(request.parse), context(cancelled, None)).isValid shouldEqual true
    check(other.id, valid(request.parse), context(notes, None)).isValid shouldEqual true
    check(noteId, valid(request.copy(date = "2025-12-31").parse), context(notes, None)).isValid shouldEqual true

  "DeliveryNoteHttpApi.dryRun" should "report a change that leaves the stock below zero on the line of the material" in:
    val registered = info(Nil)
    val desired = DesiredDeliveryNote(registered.ref, DeliveryNoteReconciliation.purchaseLines(registered))
    dryRun(noteId, desired, Map(fabricId -> 1, screwsId -> 2), materials, today).isValid shouldEqual true
    // Apply the note, then take 20 m of fabric out: the note can no longer drop to 1 m nor be cancelled.
    val stocked = materials.map { (id, stored) =>
      val lines = desired.lines.getOrElse(id, Nil)
      val applied = stored.material.withLedger(_.deliveryNoteSet(registered.ref, lines.map(_.priced), today))
      val unloaded =
        if id == fabricId then
          applied.withLedger(
            _.appended(MovementId(new UUID(3L, 3L)), today, SignedQuantity.applyUnsafe(-20000L), MovementKind.Unload("Campionatura")),
          )
        else applied
      val withNewMpn = if id == screwsId then unloaded.withMpn(valid(Mpn.createMpn("VIT-M6-20", supplierId, None, None))) else unloaded
      id -> stored.copy(material = withNewMpn)
    }
    val smaller =
      registered.copy(lines = registered.lines.map(line => if line.materialId == fabricId then line.copy(quantity = Quantity(1000L)) else line))
    val shrunk = DesiredDeliveryNote(smaller.ref, DeliveryNoteReconciliation.purchaseLines(smaller))
    errors(dryRun(noteId, shrunk, Map(fabricId -> 1, screwsId -> 2), stocked, today)) shouldEqual List(
      "Riga 1: Giacenza insufficiente per il materiale TES-001: dal 02/11/2026 in poi la giacenza non può scendere sotto zero. " +
        "(Disponibile dal 02/11/2026: 5,5 m)",
    )
    errors(dryRun(noteId, DesiredDeliveryNote(registered.ref, Map.empty), Map.empty, stocked, today)).map(_.takeWhile(_ != ':')) shouldEqual
      List("Materiale TES-001")

  "DeliveryNoteResponse" should "show the note with its supplier, its lines, their totals, and whether they are applied" in:
    val registered = info(Nil)
    val note = DeliveryNoteAggregate.Registered(DeliveryNoteRecord(registered, today, Some(today)))
    val fabricApplied = materials.updatedWith(fabricId)(_.map { stored =>
      val lines = DeliveryNoteReconciliation.purchaseLines(registered).getOrElse(fabricId, Nil)
      stored.copy(material = stored.material.withLedger(_.deliveryNoteSet(registered.ref, lines.map(_.priced), today)))
    })
    val response = DeliveryNoteResponse
      .fromDomain(note, Map(supplierId -> StoredSupplier(supplier, deleted = false)), fabricApplied)
      .getOrElse(fail("No response"))
    (response.number, response.date, response.supplierName, response.supplierActive, response.status) shouldEqual
      ("1234/A", "2026-11-02", "Tessuti Nautici", true, "registered")
    (response.registeredOn, response.changedOn, response.cancelledOn, response.total) shouldEqual ("2026-11-05", Some("2026-11-05"), None, "439.25")
    response.lines.map(line => (line.internalCode, line.unit, line.mpnCode, line.quantity, line.unitCost, line.total, line.applied)) shouldEqual List(
      ("TES-001", "linear_meters", "AC-152-NAVY", "25.500", "3.5000", "89.25", true),
      ("VIT-M6", "pieces", "VIT-M6-20", "100.000", "3.5000", "350.00", false),
    )
    response.asJson.hcursor.downField("status").focus shouldEqual Some(Json.fromString("registered"))
    val cancelled = DeliveryNoteAggregate.Cancelled(DeliveryNoteRecord(registered, today, None), today, "Doppio")
    DeliveryNoteResponse
      .fromDomain(cancelled, Map.empty, materials)
      .map(r => (r.status, r.cancelReason, r.supplierActive, r.lines.map(_.applied))) shouldEqual
      Some(("cancelled", Some("Doppio"), false, List(true, true)))
    DeliveryNoteResponse.fromDomain(DeliveryNoteAggregate.Empty, Map.empty, materials) shouldEqual None
end DeliveryNoteHttpDtosTest
