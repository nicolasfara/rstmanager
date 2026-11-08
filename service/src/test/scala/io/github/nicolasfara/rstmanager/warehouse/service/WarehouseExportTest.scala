package io.github.nicolasfara.rstmanager.warehouse.service

import java.io.ByteArrayInputStream
import java.time.Instant
import java.util.UUID

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierId }
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier
import io.github.nicolasfara.rstmanager.warehouse.service.WarehouseExport.{ Cell, NumberFormat }
import io.github.nicolasfara.rstmanager.warehouse.service.WarehouseHttpSupport.OrderLabel
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

import cats.data.ValidatedNec
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import org.dhatim.fastexcel.reader.{ CellType, ReadableWorkbook, ReadingOptions, Row }
import org.http4s.{ AuthScheme, Credentials, Headers, Method, Request, Uri }
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.typelevel.ci.CIString
import sttp.tapir.server.http4s.Http4sServerInterpreter

class WarehouseExportTest extends AnyFlatSpecLike:
  private val supplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-000000000301").nn)
  private val goneSupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-000000000302").nn)
  private val unknownSupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-000000000303").nn)
  private val reason: String :| MovementReason = "Campionatura"
  private val at = LocalDate.parse("2026-10-10")
  private val generatedAt = Instant.parse("2026-11-01T09:30:00Z").nn

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)
  private def day(text: String): LocalDate = LocalDate.parse(text)
  private def movementId(n: Long): MovementId = MovementId(new UUID(0L, n))
  private def qty(thousandths: Long): SignedQuantity = SignedQuantity.applyUnsafe(thousandths)

  private def purchase(code: String, supplier: SupplierId, quantity: Long, cost: Long, note: Option[String]): MovementKind =
    val unitCost = UnitPrice.applyUnsafe(cost)
    MovementKind.Purchase(code.refineUnsafe, supplier, unitCost, Valuation.amount(Quantity.applyUnsafe(quantity), unitCost), note)

  private def material(code: String, unit: UnitOfMeasure, price: Long, mpns: List[Mpn], created: String)(ledger: StockLedger => StockLedger) =
    val info = valid(
      MaterialInfo.createMaterialInfo(
        MaterialId(UUID.nameUUIDFromBytes(code.getBytes).nn),
        code,
        s"Materiale $code",
        None,
        None,
        unit,
        UnitPrice.applyUnsafe(price),
        mpns,
      ),
    )
    ReportedMaterial(MaterialRecord.created(info, day(created)).withLedger(ledger), None)

  private val tessuto = material(
    "TES-1",
    UnitOfMeasure.LinearMeters,
    185000L,
    List(valid(Mpn.createMpn("AC-152", supplierId, None)), valid(Mpn.createMpn("OLD-1", goneSupplierId, None))),
    "2026-10-01",
  )(
    _.appended(movementId(1), day("2026-10-01"), qty(10000L), purchase("AC-152", supplierId, 10000L, 30000L, Some("DDT 1")))
      .appended(movementId(2), day("2026-10-05"), qty(5500L), purchase("OLD-1", goneSupplierId, 5500L, 35000L, None))
      .appended(movementId(3), day("2026-10-06"), qty(-2500L), MovementKind.Unload(reason))
      .appended(movementId(4), day("2026-10-07"), qty(1000L), MovementKind.Adjustment(None, reason))
      .voided(movementId(4), MovementVoid(reason, day("2026-10-08")))
      .appended(movementId(5), day("2026-10-20"), qty(1000L), purchase("AC-152", supplierId, 1000L, 40000L, None)),
  )

  private val viti = material("VIT-1", UnitOfMeasure.Pieces, 5000L, Nil, "2026-10-02")(
    _.appended(movementId(6), day("2026-10-02"), qty(4000L), MovementKind.Adjustment(None, reason)),
  )

  private val colla = material("NEG-1", UnitOfMeasure.Kilograms, 0L, List(valid(Mpn.createMpn("GLUE", unknownSupplierId, None))), "2026-10-02")(
    _.appended(movementId(7), day("2026-10-03"), qty(1000L), purchase("GLUE", unknownSupplierId, 1000L, 20000L, None))
      .appended(movementId(8), day("2026-10-04"), qty(-3000L), MovementKind.Unload(reason)),
  )

  private val materials = List(tessuto, viti, colla)

  private val suppliers: Map[SupplierId, StoredSupplier] = Map(
    supplierId -> StoredSupplier(valid(Supplier.createSupplier(supplierId, "Tessuti Nautici", None, None, None, None, None)), deleted = false),
    goneSupplierId -> StoredSupplier(
      valid(Supplier.createSupplier(goneSupplierId, "Vecchio Fornitore", None, None, None, None, None)),
      deleted = true,
    ),
  )

  private val state = WarehouseReports.stateAt(materials, at)
  private val movements = WarehouseExport.movementsUntil(materials, at)

  private def number(units: Long, scale: Int, format: NumberFormat): Cell = Cell.Number(BigDecimal(BigInt(units), scale), format)

  "WarehouseExport.rows" should "describe every material of the state with typed cells, one per column" in:
    val rows = WarehouseExport.rows(state, suppliers)
    rows.map(_.headOption) shouldEqual List(Some(Cell.Text("NEG-1")), Some(Cell.Text("TES-1")), Some(Cell.Text("VIT-1")))
    rows.foreach(_ should have size WarehouseExport.stockColumns.size.toLong)
    rows.lift(1) shouldEqual Some(
      List(
        Cell.Text("TES-1"),
        Cell.Text("Materiale TES-1"),
        Cell.Blank,
        Cell.Text("m"),
        number(13000L, 3, NumberFormat.Quantity),
        number(35000L, 4, NumberFormat.UnitCost),
        number(4550L, 2, NumberFormat.Euro),
        number(4925L, 2, NumberFormat.Euro),
        number(185000L, 4, NumberFormat.UnitCost),
        Cell.Text("Tessuti Nautici; Vecchio Fornitore (eliminato)"),
        Cell.Text("AC-152 (Tessuti Nautici); OLD-1 (Vecchio Fornitore (eliminato))"),
        Cell.Blank,
        Cell.Blank,
        Cell.Text("Attivo"),
      ),
    )

  it should "flag negative and unvalued stock, count pieces as whole numbers, and name unknown suppliers by id" in:
    val rows = WarehouseExport.rows(state, suppliers)
    val anomaly = WarehouseExport.stockColumns.indexWhere(_.header == "Anomalia")
    rows.map(_.lift(anomaly)) shouldEqual List(Some(Cell.Text("Giacenza negativa")), Some(Cell.Blank), Some(Cell.Text("Non valorizzato")))
    rows.lift(2).flatMap(_.lift(4)) shouldEqual Some(number(4000L, 3, NumberFormat.Pieces))
    rows.lift(2).flatMap(_.lift(3)) shouldEqual Some(Cell.Text("pz"))
    rows.lift(0).flatMap(_.lift(6)) shouldEqual Some(number(-400L, 2, NumberFormat.Euro))
    rows.lift(0).flatMap(_.lift(9)) shouldEqual Some(Cell.Text(unknownSupplierId.value.toString))

  it should "close the sheet with the totals of the state" in:
    val sheet = WarehouseExport.stockSheet(state, suppliers)
    sheet.totals shouldEqual Some(
      List(
        Cell.Text("Totale"),
        Cell.Text("3 materiali"),
        Cell.Blank,
        Cell.Blank,
        Cell.Blank,
        Cell.Blank,
        number(4150L, 2, NumberFormat.Euro),
        number(5125L, 2, NumberFormat.Euro),
        Cell.Blank,
        Cell.Blank,
        Cell.Blank,
        Cell.Blank,
        Cell.Text("1 con giacenza negativa; 1 non valorizzati"),
        Cell.Blank,
      ),
    )

  "WarehouseExport.movementsUntil" should "list the movements not voided up to the day, chronologically, valued like the movement list" in:
    movements.map(exported => (exported.movement.on.toString, exported.material.record.info.internalCode: String)) shouldEqual List(
      ("2026-10-01", "TES-1"),
      ("2026-10-02", "VIT-1"),
      ("2026-10-03", "NEG-1"),
      ("2026-10-04", "NEG-1"),
      ("2026-10-05", "TES-1"),
      ("2026-10-06", "TES-1"),
    )
    val unload = movements.lastOption.getOrElse(fail("No movement"))
    WarehouseExport.movementRow(unload, suppliers, Map.empty) shouldEqual List(
      Cell.Day(day("2026-10-06")),
      Cell.Text("TES-1"),
      Cell.Text("Materiale TES-1"),
      Cell.Text("m"),
      Cell.Text("Scarico"),
      Cell.Blank,
      Cell.Blank,
      number(-2500L, 3, NumberFormat.Quantity),
      number(35000L, 4, NumberFormat.UnitCost),
      number(-875L, 2, NumberFormat.Euro),
      Cell.Text("Campionatura"),
    )
    movements.headOption.map(WarehouseExport.movementRow(_, suppliers, Map.empty)).map(_.slice(4, 7)) shouldEqual
      Some(List(Cell.Text("Carico"), Cell.Text("Tessuti Nautici"), Cell.Text("AC-152")))
    movements.headOption.flatMap(WarehouseExport.movementRow(_, suppliers, Map.empty).lastOption) shouldEqual Some(Cell.Text("DDT 1"))
    movements.lift(1).map(WarehouseExport.movementRow(_, suppliers, Map.empty)).flatMap(_.lift(8)) shouldEqual Some(Cell.Blank)

  it should "name automatic consumptions and their reversals with the order they belong to" in:
    val ref = ConsumptionRef(OrderId(new UUID(9L, 1L)), ScheduledManufacturingId(new UUID(9L, 2L)))
    val unknown = ConsumptionRef(OrderId(new UUID(9L, 3L)), ScheduledManufacturingId(new UUID(9L, 4L)))
    val consumed = material("PRF-1", UnitOfMeasure.LinearMeters, 10000L, Nil, "2026-10-01")(
      _.appended(movementId(20), day("2026-10-01"), qty(5000L), MovementKind.Adjustment(Some(UnitPrice.applyUnsafe(20000L)), reason))
        .consumptionSet(ref, Quantity.applyUnsafe(3000L), day("2026-10-02"))
        .consumptionSet(ref, Quantity.applyUnsafe(1000L), day("2026-10-03"))
        .consumptionSet(unknown, Quantity.applyUnsafe(500L), day("2026-10-04")),
    )
    val labels = Map(ref.orderId -> OrderLabel("2026-014", Map(ref.manufacturingId -> "TND-01")))
    val rows = WarehouseExport.movementsUntil(List(consumed), at).map(WarehouseExport.movementRow(_, suppliers, labels))
    rows.map(row => (row.lift(4), row.lift(7), row.lift(9), row.lastOption)) shouldEqual List(
      (
        Some(Cell.Text("Rettifica")),
        Some(number(5000L, 3, NumberFormat.Quantity)),
        Some(number(1000L, 2, NumberFormat.Euro)),
        Some(Cell.Text("Campionatura")),
      ),
      (
        Some(Cell.Text("Scarico lavorazione")),
        Some(number(-3000L, 3, NumberFormat.Quantity)),
        Some(number(-600L, 2, NumberFormat.Euro)),
        Some(Cell.Text("Ordine 2026-014 – lavorazione TND-01")),
      ),
      (
        Some(Cell.Text("Storno lavorazione")),
        Some(number(2000L, 3, NumberFormat.Quantity)),
        Some(number(400L, 2, NumberFormat.Euro)),
        Some(Cell.Text("Ordine 2026-014 – lavorazione TND-01")),
      ),
      (
        Some(Cell.Text("Scarico lavorazione")),
        Some(number(-500L, 3, NumberFormat.Quantity)),
        Some(number(-100L, 2, NumberFormat.Euro)),
        Some(Cell.Text(s"Ordine ${unknown.orderId.value}")),
      ),
    )

  "WarehouseExport.fileName" should "carry the day" in:
    WarehouseExport.fileName(at) shouldEqual "magazzino-2026-10-10.xlsx"
    WarehouseReportsHttpApi.contentDisposition(WarehouseExport.fileName(at)) shouldEqual "attachment; filename=\"magazzino-2026-10-10.xlsx\""

  "WarehouseReportsHttpApi.parseStateDay" should "default to today and refuse a future day in Italian" in:
    val today = day("2026-11-01")
    WarehouseReportsHttpApi.parseStateDay(None, today).toOption shouldEqual Some(today)
    WarehouseReportsHttpApi.parseStateDay(Some("2026-10-10"), today).toOption shouldEqual Some(at)
    WarehouseReportsHttpApi.parseStateDay(Some("2026-11-02"), today).fold(_.toChain.toList, _ => Nil) shouldEqual
      List("Giorno: la data 02/11/2026 è successiva a oggi (01/11/2026)")

  "WarehouseExport.write" should "produce a workbook that reads back with the same headers, numbers, dates, and totals" in:
    val bytes = WarehouseExport.write(WarehouseExport.workbook(state, movements, suppliers, Map.empty, generatedAt)).unsafeRunSync()
    ReadableWorkbook.isOOXMLZipHeader(bytes.take(8)) shouldEqual true
    val workbook = new ReadableWorkbook(new ByteArrayInputStream(bytes), new ReadingOptions(true, false))
    try
      workbook.getSheets.nn.iterator().nn.asScala.map(_.getName).toList shouldEqual List("Magazzino", "Movimenti", "Info")
      def rowsOf(name: String): List[Row] =
        workbook.findSheet(name).nn.toScala.getOrElse(fail(s"No sheet $name")).read().nn.asScala.toList

      val stock = rowsOf("Magazzino")
      stock should have size 5
      val header = stock.headOption.getOrElse(fail("No header"))
      (0 until header.getCellCount).map(header.getCellText).toList shouldEqual WarehouseExport.stockColumns.map(_.header)
      val tes = stock.lift(2).getOrElse(fail("No TES-1 row"))
      tes.getCellText(0) shouldEqual "TES-1"
      tes.getCell(4).getType shouldEqual CellType.NUMBER
      BigDecimal(tes.getCell(4).asNumber) shouldEqual BigDecimal("13")
      BigDecimal(tes.getCell(6).asNumber) shouldEqual BigDecimal("45.5")
      tes.getCell(6).getDataFormatString should include("€")
      tes.getCell(4).getDataFormatString shouldEqual "0.000"
      tes.getCell(8).getDataFormatString shouldEqual "#,##0.0000 \"€\""
      val totals = stock.lastOption.getOrElse(fail("No totals"))
      totals.getCellText(0) shouldEqual "Totale"
      BigDecimal(totals.getCell(6).asNumber) shouldEqual BigDecimal("41.5")
      BigDecimal(totals.getCell(7).asNumber) shouldEqual BigDecimal("51.25")

      val moves = rowsOf("Movimenti")
      moves should have size 7
      moves.lift(1).map(_.getCell(0).asDate.nn.toLocalDate.toString) shouldEqual Some("2026-10-01")
      moves.lift(1).map(_.getCell(0).getDataFormatString) shouldEqual Some(WarehouseExport.dayFormat)
      moves.lastOption.map(row => BigDecimal(row.getCell(7).asNumber)) shouldEqual Some(BigDecimal("-2.5"))

      val info = rowsOf("Info")
      info.lift(1).map(_.getCellText(0)) shouldEqual Some("Stato magazzino al")
      info.lift(1).map(_.getCell(1).asDate.nn.toLocalDate.toString) shouldEqual Some("2026-10-10")
      info.lift(2).map(_.getCellText(1)) shouldEqual Some("01/11/2026 10:30 (ora italiana)")
    finally workbook.close()
    end try

  "WarehouseReportsHttpApi.exportXlsx" should "serve the workbook as an xlsx attachment" in:
    val served = WarehouseReportsHttpApi.exportXlsx
      .serverSecurityLogicSuccess[Unit, IO](_ => IO.unit)
      .serverLogic(_ => _ => IO.pure(Right((Array[Byte](1, 2, 3), WarehouseReportsHttpApi.contentDisposition("magazzino-2026-10-10.xlsx")))))
    val routes = Http4sServerInterpreter[IO]().toRoutes(served)
    val request = Request[IO](Method.GET, Uri.unsafeFromString("/api/v1/warehouse/export.xlsx?at=2026-10-10"))
      .withHeaders(Headers(Authorization(Credentials.Token(AuthScheme.Bearer, "token"))))
    val response = routes.orNotFound.run(request).unsafeRunSync()
    response.status.code shouldEqual 200
    response.headers.get(CIString("Content-Type")).map(_.head.value) shouldEqual Some(WarehouseExport.mediaType)
    response.headers.get(CIString("Content-Disposition")).map(_.head.value) shouldEqual Some("attachment; filename=\"magazzino-2026-10-10.xlsx\"")
    response.body.compile.toList.unsafeRunSync() shouldEqual List[Byte](1, 2, 3)
end WarehouseExportTest
