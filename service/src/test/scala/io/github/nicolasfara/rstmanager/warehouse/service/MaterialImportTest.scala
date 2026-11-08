package io.github.nicolasfara.rstmanager.warehouse.service

import java.io.ByteArrayOutputStream
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialId, MaterialInfo, Mpn }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierId }
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialHttpApi.MaterialImportResponse
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialImport.*

import cats.data.ValidatedNec
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.parser.decode
import io.circe.syntax.*
import org.dhatim.fastexcel.Workbook
import org.http4s.{ AuthScheme, Credentials, Headers, MediaType, Method, Request, Uri }
import org.http4s.headers.{ `Content-Type`, Authorization }
import org.http4s.implicits.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

class MaterialImportTest extends AnyFlatSpecLike:
  private val today: LocalDate = LocalDate.parse("2026-11-08")

  /** A cell written in a test workbook. */
  private enum X derives CanEqual:
    case S(text: String)
    case N(value: Double)
    case F(formula: String)
    case Blank

  private val headers: List[String] =
    List(
      "CODICE",
      "UNITA' DI MISURA",
      "DESCRIZIONE",
      "TIPOLOGIA",
      "PREZZO CLIENTE",
      "NOTE",
      "FORNITORE",
      "CODICE MPN",
      "DESCRIZIONE FORNITORE",
      "PREZZO FORNITORE",
    )

  /** An `.xlsx` with `header` on row 1 and `rows` below it, as fastexcel writes it (formulas without a computed value). */
  private def workbook(header: List[String], rows: List[List[X]]): Array[Byte] =
    val out = new ByteArrayOutputStream()
    val book = new Workbook(out, "rstmanager-test", "1.0")
    val sheet = book.newWorksheet("materiali")
    header.zipWithIndex.foreach((text, column) => sheet.value(0, column, text))
    rows.zipWithIndex.foreach { (cells, index) =>
      cells.zipWithIndex.foreach {
        case (X.S(text), column) => sheet.value(index + 1, column, text)
        case (X.N(value), column) => sheet.value(index + 1, column, java.lang.Double.valueOf(value))
        case (X.F(formula), column) => sheet.formula(index + 1, column, formula)
        case (X.Blank, _) => ()
      }
    }
    book.finish()
    out.toByteArray.nn

  /** A row in the usual column order; blank strings are blank cells, and an MPN costs 1.50 €. */
  private def row(
      code: String,
      unit: String,
      description: String,
      kind: String,
      price: X,
      supplier: String,
      mpn: String,
      supplierDescription: String,
  ): List[X] =
    def s(text: String): X = if text.isEmpty then X.Blank else X.S(text)
    List(
      s(code),
      s(unit),
      s(description),
      s(kind),
      price,
      X.Blank,
      s(supplier),
      s(mpn),
      s(supplierDescription),
      if mpn.isEmpty then X.Blank else X.N(1.5),
    )
  end row

  private def supplier(n: Int, name: String): Supplier =
    valid(Supplier.createSupplier(SupplierId(new UUID(0L, 300L + n)), name, None, None, None, None, None))

  private val nautea: Supplier = supplier(1, "Nautea")
  private val rigging: Supplier = supplier(2, "Boat  Rigging")

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private val existing: MaterialInfo =
    valid(
      MaterialInfo.createMaterialInfo(
        MaterialId(new UUID(0L, 400L)),
        "ESISTENTE",
        "Materiale già a magazzino",
        Some("Materiale di consumo"),
        None,
        UnitOfMeasure.Pieces,
        UnitPrice.applyUnsafe(10000L),
        List(valid(Mpn.createMpn("TAKEN-1", nautea.id, None, None))),
      ),
    )

  private def rowsOf(bytes: Array[Byte]): List[ImportRow] = readSheet(bytes).fold(problems => fail(problems.toString), identity)

  private def context(rows: List[ImportRow], materials: List[MaterialInfo], suppliers: List[Supplier]): ImportContext =
    val draft = MaterialImport.draft(rows)
    ImportContext(
      today,
      materials,
      suppliers,
      ImportIds(
        draft.codes.zipWithIndex.map((code, n) => code -> MaterialId(new UUID(1L, n.toLong))).toMap,
        draft.supplierKeys.zipWithIndex.map((key, n) => key -> SupplierId(new UUID(2L, n.toLong))).toMap,
      ),
    )

  private def planOf(rows: List[List[X]], materials: List[MaterialInfo], suppliers: List[Supplier]): ValidatedNec[Problem, ImportPlan] =
    val read = rowsOf(workbook(headers, rows))
    MaterialImport.plan(read, context(read, materials, suppliers))

  private def planned(rows: List[List[X]], materials: List[MaterialInfo], suppliers: List[Supplier]): ImportPlan =
    planOf(rows, materials, suppliers).fold(problems => fail(problems.toChain.toList.map(_.message).mkString("\n")), identity)

  private def problems(rows: List[List[X]], materials: List[MaterialInfo], suppliers: List[Supplier]): List[String] =
    planOf(rows, materials, suppliers).fold(_.toChain.toList.map(_.message), plan => fail(s"Unexpectedly valid: $plan"))

  private def material(plan: ImportPlan, code: String): MaterialInfo =
    plan.materials.find(m => (m.info.internalCode: String) == code).fold(fail(s"No material $code in $plan"))(_.info)

  "readSheet" should "find the columns by header, whatever their order, accents, and extra columns" in:
    val header = List("NOTE", "Codice MPN", "Unità di misura", "codice", "ALTRO", "Descrizione", "Tipologia", "Prezzo cliente", "Fornitore") ++
      List("Descrizione fornitore", "Prezzo fornitore")
    val rows = rowsOf(
      workbook(
        header,
        List(List(X.S("nota"), X.S("mpn-1"), X.S("pz"), X.S("vit-1"), X.S("ignorata"), X.S("Vite"), X.S("Viti"), X.N(2.5), X.S("Nautea"))),
      ),
    )
    rows.map(r => (r.line, r.code, r.unit, r.notes, r.mpn, r.customerPrice)) shouldEqual
      List(
        (
          2,
          Some(CellValue.Text("vit-1")),
          Some(CellValue.Text("pz")),
          Some(CellValue.Text("nota")),
          Some(CellValue.Text("mpn-1")),
          Some(CellValue.Number(2.5)),
        ),
      )

  it should "reject a sheet missing a column, naming every missing header" in:
    readSheet(workbook(headers.filterNot(Set("NOTE", "CODICE MPN")), List(List(X.S("A"))))).left.map(_.toChain.toList.map(_.message)) shouldEqual
      Left(List(s"Riga 1: mancano le intestazioni NOTE, CODICE MPN (servono ${headers.mkString(", ")})"))

  it should "skip blank rows, keeping the Excel row numbers" in:
    val rows = rowsOf(
      workbook(headers, List(row("A", "pz", "Uno", "", X.N(1), "", "", ""), List(X.S("  ")), Nil, row("B", "pz", "Due", "", X.N(1), "", "", ""))),
    )
    rows.map(_.line) shouldEqual List(2, 5)

  it should "reject an empty sheet, too many rows, and a file that is not an xlsx" in:
    readSheet(workbook(headers, Nil)).left.map(_.toChain.toList.map(_.message)) shouldEqual
      Left(List("File: il foglio non contiene righe di materiali sotto le intestazioni"))
    val tooMany = List.tabulate(MaterialImport.maxRows + 1)(n => List(X.S(s"C$n")))
    readSheet(workbook(headers, tooMany)).left.map(_.toChain.toList.map(_.message)) shouldEqual
      Left(List(s"File: il foglio ha più di ${MaterialImport.maxRows} righe di materiali, il massimo consentito"))
    readSheet(workbook(headers, tooMany.take(MaterialImport.maxRows))).map(_.size) shouldEqual Right(MaterialImport.maxRows)
    readSheet("CODICE;UNITA".getBytes.nn).left.map(_.toChain.toList.map(_.message)) shouldEqual
      Left(List("File: il file non è una cartella di lavoro Excel .xlsx"))
    readSheet(Array.emptyByteArray).isLeft shouldEqual true

  "plan" should "make one material of the rows of a code, with one MPN per row" in:
    val plan = planned(
      List(
        row(" anella4x30 ", "pz", "Anello inox", "Accessorio", X.N(2.695), "Boat Rigging", "04.100.052", "Anello conf. 10"),
        row("ANELLA4X30", "pezzi", "", "accessorio", X.N(2.695), "boat rigging", "04.100.039 ", ""),
      ),
      Nil,
      List(rigging),
    )
    plan.suppliers shouldEqual Nil
    plan.skipped shouldEqual Nil
    val info = material(plan, "ANELLA4X30")
    info.description shouldEqual "Anello inox"
    info.materialType shouldEqual Some("Accessorio")
    info.unit shouldEqual UnitOfMeasure.Pieces
    info.customerPrice shouldEqual UnitPrice.applyUnsafe(26950L)
    info.mpns.map(mpn => (mpn.code: String, mpn.supplierId, mpn.description, mpn.expectedUnitCost)) shouldEqual
      List(
        ("04.100.052", rigging.id, Some("Anello conf. 10"), Some(UnitPrice.applyUnsafe(15000L))),
        ("04.100.039", rigging.id, None, Some(UnitPrice.applyUnsafe(15000L))),
      )
    plan.materials.map(_.lines) shouldEqual List(List(2, 3))
    MaterialImportResponse.of(plan, dryRun = true) shouldEqual MaterialImportResponse(true, List("ANELLA4X30"), Nil, Nil, 2)

  it should "report the rows of a code that disagree on unit, customer price, or type" in:
    problems(
      List(
        row("FILO", "kg", "Filo", "Cima", X.N(0.668), "", "", ""),
        row("FILO", "pz", "Filo", "cima", X.N(12.2395), "", "", ""),
        row("NASTRO", "mt", "Nastro", "Fettuccia", X.N(1), "", "", ""),
        row("NASTRO", "ml", "Nastro", "Cima", X.N(1), "", "", ""),
      ),
      Nil,
      Nil,
    ) shouldEqual List(
      "Righe 2, 3 (FILO): UNITA' DI MISURA diverse: kg, pz",
      "Righe 2, 3 (FILO): PREZZO CLIENTE diversi: 0,668 €, 12,2395 €",
      "Righe 4, 5 (NASTRO): TIPOLOGIA diverse: Fettuccia, Cima",
    )

  it should "take the description from the supplier when the material has none, and require one of them" in:
    val plan = planned(
      List(row("A", "pz", "", "", X.N(1), "", "", ""), row("A", "pz", "", "", X.N(1), "Nautea", "N-1", "Descrizione fornitore")),
      Nil,
      List(nautea),
    )
    material(plan, "A").description shouldEqual "Descrizione fornitore"
    problems(List(row("B", "pz", "", "", X.N(1), "Nautea", "N-1", "")), Nil, List(nautea)) shouldEqual
      List("Riga 2 (B): DESCRIZIONE: manca, e nessuna riga del codice ha una DESCRIZIONE FORNITORE da usare al suo posto")

  it should "read the unit aliases, packs included, and reject an unknown unit" in:
    val units = List("pz", "PEZZI", "mt", "m", "ml", "mq", "m²", "kg", "cf", "Confezioni", "packs")
    val plan = planned(units.zipWithIndex.map((unit, n) => row(s"U$n", unit, "Materiale", "", X.N(1), "", "", "")), Nil, Nil)
    plan.materials.map(_.info.unit) shouldEqual List(
      UnitOfMeasure.Pieces,
      UnitOfMeasure.Pieces,
      UnitOfMeasure.LinearMeters,
      UnitOfMeasure.LinearMeters,
      UnitOfMeasure.LinearMeters,
      UnitOfMeasure.SquareMeters,
      UnitOfMeasure.SquareMeters,
      UnitOfMeasure.Kilograms,
      UnitOfMeasure.Packs,
      UnitOfMeasure.Packs,
      UnitOfMeasure.Packs,
    )
    problems(List(row("L", "litri", "Materiale", "", X.N(1), "", "", "")), Nil, Nil) shouldEqual
      List("Riga 2, UNITA' DI MISURA: 'litri' non è un'unità di misura riconosciuta (usa pz, mt, mq, kg o cf)")

  it should "round prices HALF_UP to 4 decimals and read Italian text prices" in:
    val plan = planned(
      List(
        row("A", "pz", "Uno", "", X.N(0.66803333333), "", "", ""),
        row("B", "pz", "Due", "", X.S("1.234,56"), "", "", ""),
        row("C", "pz", "Tre", "", X.S("3,7 €"), "", "", ""),
        row("D", "pz", "Quattro", "", X.N(0.00005), "", "", ""),
      ),
      Nil,
      Nil,
    )
    plan.materials.map(_.info.customerPrice.value) shouldEqual List(6680L, 12345600L, 37000L, 1L)
    problems(List(row("E", "pz", "Cinque", "", X.S("dodici"), "", "", ""), row("F", "pz", "Sei", "", X.N(-1), "", "", "")), Nil, Nil) shouldEqual
      List(
        "Riga 2, PREZZO CLIENTE: 'dodici' non è un prezzo valido (scrivi ad esempio 12,50)",
        "Riga 3, PREZZO CLIENTE: Il prezzo unitario deve essere compreso fra 0 € e 1.000.000 €",
      )

  it should "require the customer price and the code, and keep supplier and MPN together" in:
    problems(
      List(
        row("A", "pz", "Uno", "", X.Blank, "", "", ""),
        row("", "pz", "Due", "", X.N(1), "", "", ""),
        row("C", "pz", "Tre", "", X.N(1), "Nautea", "", ""),
        row("D", "pz", "Quattro", "", X.N(1), "", "N-9", ""),
        row("E", "pz", "Cinque", "", X.N(1), "", "", "Solo descrizione"),
      ),
      Nil,
      List(nautea),
    ) shouldEqual List(
      "Riga 2, PREZZO CLIENTE: il valore è obbligatorio",
      "Riga 3, CODICE: il valore è obbligatorio",
      "Riga 4, CODICE MPN: indica il codice MPN del fornitore, oppure lascia vuoto anche FORNITORE",
      "Riga 5, FORNITORE: indica il fornitore dell'MPN, oppure lascia vuoto anche CODICE MPN",
      "Riga 6, CODICE MPN: DESCRIZIONE FORNITORE e PREZZO FORNITORE si indicano solo con FORNITORE e CODICE MPN",
    )

  it should "report a formula without a computed value" in:
    problems(List(row("A", "pz", "Uno", "", X.F("J2*0.35"), "", "", "")), Nil, Nil) shouldEqual
      List("Riga 2, PREZZO CLIENTE: formula senza valore calcolato, apri e salva il file in Excel")

  it should "reject the same MPN on two materials, and the same MPN twice on one" in:
    problems(
      List(
        row("LAMPO8200BIANCA", "pz", "Bianca", "", X.N(1), "Nautea", "LDVS082AB200", ""),
        row("LAMPO8200BLU", "pz", "Blu", "", X.N(1), "nautea", "ldvs082ab200", ""),
        row("DOPPIO", "pz", "Doppio", "", X.N(1), "Nautea", "D-1", ""),
        row("DOPPIO", "pz", "Doppio", "", X.N(1), "Nautea", "d-1", ""),
      ),
      Nil,
      List(nautea),
    ) shouldEqual List(
      "Righe 2, 3: MPN LDVS082AB200 di Nautea indicato per più materiali (LAMPO8200BIANCA, LAMPO8200BLU)",
      "Righe 4, 5 (DOPPIO): MPN: il codice MPN D-1 compare più volte per lo stesso fornitore",
    )

  it should "reject an MPN an active material already uses" in:
    problems(List(row("NUOVO", "pz", "Nuovo", "", X.N(1), "Nautea", "taken-1", "")), List(existing), List(nautea)) shouldEqual
      List("Riga 2, CODICE MPN: l'MPN TAKEN-1 di Nautea è già usato dal materiale ESISTENTE")

  it should "skip a code an active material already has, without checking its rows" in:
    val plan = planned(
      List(
        row("esistente", "litri", "", "", X.S("non un prezzo"), "", "", ""),
        row("NUOVO", "pz", "Nuovo", "materiale DI CONSUMO", X.N(1), "", "", ""),
      ),
      List(existing),
      Nil,
    )
    plan.skipped shouldEqual List(SkippedMaterial("ESISTENTE", List(2)))
    plan.materials.map(_.info.internalCode: String) shouldEqual List("NUOVO")
    material(plan, "NUOVO").materialType shouldEqual Some("Materiale di consumo")
    MaterialImportResponse.of(plan, dryRun = false) shouldEqual MaterialImportResponse(false, List("NUOVO"), List("ESISTENTE"), Nil, 0)

  it should "create an unknown supplier once, matching names without case and blanks" in:
    val plan = planned(
      List(
        row("A", "pz", "Uno", "", X.N(1), "Sail  Service", "S-1", ""),
        row("B", "pz", "Due", "", X.N(1), " sail service ", "S-2", ""),
        row("C", "pz", "Tre", "", X.N(1), "BOAT RIGGING", "R-1", ""),
      ),
      Nil,
      List(nautea, rigging),
    )
    plan.suppliers.map(supplier => supplier.name: String) shouldEqual List("Sail Service")
    val created = plan.suppliers.map(_.id)
    material(plan, "A").suppliers shouldEqual created
    material(plan, "B").suppliers shouldEqual created
    material(plan, "C").suppliers shouldEqual List(rigging.id)
    MaterialImportResponse.of(plan, dryRun = true).newSuppliers shouldEqual List("Sail Service")

  it should "reject a supplier name two active suppliers have" in:
    problems(List(row("A", "pz", "Uno", "", X.N(1), "Nautea", "N-1", "")), Nil, List(nautea, supplier(3, "NAUTEA"))) shouldEqual
      List("Riga 2, FORNITORE: ci sono 2 fornitori attivi di nome Nautea; rinominali in modo che il nome sia univoco")

  it should "list every problem of the file sorted by row" in:
    val listed = problems(
      List(
        row("B", "pz", "Due", "", X.S("x"), "", "", ""),
        row("A", "kg", "Uno", "", X.N(1), "", "", ""),
        row("A", "pz", "Uno", "", X.N(1), "", "", ""),
        row("", "pz", "Tre", "", X.N(1), "", "", ""),
      ),
      Nil,
      Nil,
    )
    listed shouldEqual List(
      "Riga 2, PREZZO CLIENTE: 'x' non è un prezzo valido (scrivi ad esempio 12,50)",
      "Righe 3, 4 (A): UNITA' DI MISURA diverse: kg, pz",
      "Riga 5, CODICE: il valore è obbligatorio",
    )

  "MaterialImportResponse" should "travel as JSON with the field names the frontend reads" in:
    val response = MaterialImportResponse(true, List("A", "B"), List("C"), List("Sail Service"), 3)
    response.asJson.noSpaces shouldEqual """{"dryRun":true,"created":["A","B"],"skipped":["C"],"newSuppliers":["Sail Service"],"mpns":3}"""
    decode[MaterialImportResponse](response.asJson.noSpaces) shouldEqual Right(response)

  "MaterialHttpApi.importXlsx" should "take the dryRun flag and the raw xlsx bytes" in:
    val served = MaterialHttpApi.importXlsx
      .serverSecurityLogicSuccess[Unit, IO](_ => IO.unit)
      .serverLogic(_ => (dryRun, body) => IO.pure(Right(MaterialImportResponse(dryRun.getOrElse(false), List(body.length.toString), Nil, Nil, 0))))
    val routes = Http4sServerInterpreter[IO]().toRoutes(served)
    def post(query: String): String =
      val request = Request[IO](Method.POST, Uri.unsafeFromString(s"/api/v1/warehouse/materials/import$query"))
        .withHeaders(
          Headers(Authorization(Credentials.Token(AuthScheme.Bearer, "token")), `Content-Type`(MediaType.unsafeParse(WarehouseExport.mediaType))),
        )
        .withBodyStream(fs2.Stream.emits(List[Byte](1, 2, 3)))
      routes.orNotFound.run(request).flatMap(_.as[String]).unsafeRunSync()
    decode[MaterialImportResponse](post("?dryRun=true")) shouldEqual Right(MaterialImportResponse(true, List("3"), Nil, Nil, 0))
    decode[MaterialImportResponse](post("")) shouldEqual Right(MaterialImportResponse(false, List("3"), Nil, Nil, 0))
end MaterialImportTest
