package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.circe.parser.decode
import io.circe.syntax.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import MaterialDrafts.*

/** Unit tests for the pure material lines of an order manufacturing: validation, request building, price freezing, and costs. */
final class MaterialDraftsSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares these DTOs structurally, so it needs CanEqual instances.
  private given CanEqual[MaterialDto, MaterialDto] = CanEqual.derived
  private given CanEqual[MaterialResponse, MaterialResponse] = CanEqual.derived

  private def uuid(s: String): UUID = UUID.fromString(s).nn

  private val fabricId = uuid("00000000-0000-0000-0000-0000000000f1")
  private val screwId = uuid("00000000-0000-0000-0000-0000000000f2")
  private val archivedId = uuid("00000000-0000-0000-0000-0000000000f3")
  private val goneId = uuid("00000000-0000-0000-0000-0000000000f4")

  // Base fixture; tests derive their variant via `.copy` (project style forbids method default args).
  private val fabric: ProductResponse =
    ProductResponse(
      fabricId,
      "TES-001",
      "Tessuto blu",
      None,
      None,
      "linear_meters",
      "18.5000",
      Nil,
      Nil,
      "2026-10-01",
      false,
      None,
      StockDto("10.000", None, None, "0.00", false),
    )

  private val screws: ProductResponse =
    fabric.copy(id = screwId, internalCode = "VIT-6", description = "Viti inox", unit = "pieces", customerPrice = "0.1235")

  /** A current line of the manufacturing, frozen at `unitPrice`. */
  private def line(productId: UUID, code: Option[String], unit: Option[String], archived: Boolean, quantity: String, unitPrice: String) =
    MaterialResponse(productId, code, code.map(c => s"Descrizione $c"), unit, archived, quantity, unitPrice, "0.00")

  private val context: Context = Context.of(List(screws, fabric), Nil)

  private def keys(): () => Int =
    val counter = Iterator.from(1)
    () => counter.next()

  // ---- context -----------------------------------------------------------------------------------

  test("the context offers the active products by code, then the current lines' products not among them"):
    val current = List(
      line(fabricId, Some("TES-001"), Some("linear_meters"), false, "1.000", "17.0000"),
      line(archivedId, Some("OLD-1"), Some("kilograms"), true, "2.000", "4.0000"),
      line(goneId, None, None, false, "1.000", "1.0000"),
    )
    val archivedProduct = fabric.copy(id = archivedId, internalCode = "AAA-0", archived = true)
    val ctx = Context.of(List(screws, archivedProduct, fabric), current)
    ctx.options shouldBe List(
      fabricId.toString -> "TES-001 — Tessuto blu",
      screwId.toString -> "VIT-6 — Viti inox",
      archivedId.toString -> "OLD-1 — Descrizione OLD-1 (archiviato)",
      goneId.toString -> "00000000 — Prodotto non più in magazzino",
    )
    ctx.frozen shouldBe Map(fabricId -> "17.0000", archivedId -> "4.0000", goneId -> "1.0000")

  test("rowsOf turns the current lines into rows with Italian quantities and no price override"):
    val rows = rowsOf(List(line(fabricId, Some("TES-001"), Some("linear_meters"), false, "2.500", "17.0000")), keys())
    rows shouldBe List(Row(1, fabricId.toString, "2,5", ""))

  // ---- validation --------------------------------------------------------------------------------

  test("valid rows become request lines in order, with canonical quantities and typed prices"):
    val rows = List(Row(1, fabricId.toString, "2,5", ""), Row(2, screwId.toString, "12", "0,2"))
    request(rows, context) shouldBe Right(List(MaterialDto(fabricId, "2.500", None), MaterialDto(screwId, "12.000", Some("0.2000"))))
    errors(rows, context) shouldBe Nil

  test("blank rows are ignored"):
    request(List(Row.empty(1), Row(2, fabricId.toString, "1", ""), Row(3, " ", "  ", "")), context) shouldBe
      Right(List(MaterialDto(fabricId, "1.000", None)))
    request(Nil, context) shouldBe Right(Nil)

  test("a missing or unavailable product is reported with its row number"):
    errors(List(Row(1, "", "2", "")), context) shouldBe List("Riga 1: seleziona un prodotto")
    errors(List(Row(1, archivedId.toString, "2", "")), context) shouldBe List("Riga 1: il prodotto selezionato non è più disponibile")
    errors(List(Row(1, "garbage", "2", "")), context) shouldBe List("Riga 1: il prodotto selezionato non è più disponibile")

  test("quantities must be present, readable, positive, and whole for pieces"):
    errors(List(Row(1, fabricId.toString, "", "1")), context) shouldBe List("Riga 1: indica la quantità")
    errors(List(Row(1, fabricId.toString, "1,2345", "")), context) shouldBe List("Riga 1: quantità non valida (es. 2,5, al massimo 3 decimali)")
    errors(List(Row(1, fabricId.toString, "abc", "")), context) shouldBe List("Riga 1: quantità non valida (es. 2,5, al massimo 3 decimali)")
    errors(List(Row(1, fabricId.toString, "-1", "")), context) shouldBe List("Riga 1: quantità non valida (es. 2,5, al massimo 3 decimali)")
    errors(List(Row(1, fabricId.toString, "0,000", "")), context) shouldBe List("Riga 1: la quantità deve essere maggiore di zero")
    errors(List(Row(1, screwId.toString, "2,5", "")), context) shouldBe
      List("Riga 1: VIT-6 è gestito a pezzi: la quantità deve essere un numero intero")
    errors(List(Row(1, screwId.toString, "3,000", "")), context) shouldBe Nil

  test("a typed price must be readable with at most 4 decimals"):
    errors(List(Row(1, fabricId.toString, "1", "3,12345")), context) shouldBe
      List("Riga 1: prezzo unitario non valido (es. 3,50, al massimo 4 decimali)")
    errors(List(Row(1, fabricId.toString, "1", "-3")), context) shouldBe
      List("Riga 1: prezzo unitario non valido (es. 3,50, al massimo 4 decimali)")

  test("every message of a row is reported, and a repeated product is flagged after its first row"):
    val rows = List(Row(1, fabricId.toString, "1", ""), Row(2, screwId.toString, "1", ""), Row(3, fabricId.toString, "x", "y"))
    errors(rows, context) shouldBe List(
      "Riga 3: quantità non valida (es. 2,5, al massimo 3 decimali)",
      "Riga 3: prezzo unitario non valido (es. 3,50, al massimo 4 decimali)",
      "Riga 3: TES-001 è già in un'altra riga: indica la quantità totale in una sola riga",
    )
    request(rows, context).isLeft shouldBe true

  test("row numbers count the blank rows too, so they match the screen"):
    errors(List(Row.empty(1), Row(2, "", "1", "")), context) shouldBe List("Riga 2: seleziona un prodotto")

  test("lines already in the manufacturing keep an archived or unknown product"):
    val current =
      List(line(archivedId, Some("OLD-1"), Some("kilograms"), true, "2.000", "4.0000"), line(goneId, None, None, false, "1.000", "1.0000"))
    val ctx = Context.of(List(fabric), current)
    val rows = rowsOf(current, keys())
    request(rows, ctx) shouldBe Right(List(MaterialDto(archivedId, "2.000", None), MaterialDto(goneId, "1.000", None)))

  // ---- price freezing ----------------------------------------------------------------------------

  test("a new product without a typed price previews the customer price and is sent without one"):
    val row = Row(1, fabricId.toString, "2", "")
    pricePreview(row, context) shouldBe Some(PricePreview("18.5000", PriceSource.Catalog))
    request(List(row), context) shouldBe Right(List(MaterialDto(fabricId, "2.000", None)))
    priceNote(PricePreview("18.5000", PriceSource.Catalog)) shouldBe "Prezzo di listino: 18,50 € (congelato al salvataggio)"

  test("a line already in the manufacturing previews its frozen price, not the current customer price, and keeps it by sending none"):
    val ctx = Context.of(List(fabric), List(line(fabricId, Some("TES-001"), Some("linear_meters"), false, "1.000", "17.0000")))
    val row = Row(1, fabricId.toString, "1", "")
    pricePreview(row, ctx) shouldBe Some(PricePreview("17.0000", PriceSource.Frozen))
    request(List(row), ctx) shouldBe Right(List(MaterialDto(fabricId, "1.000", None)))
    priceNote(PricePreview("17.0000", PriceSource.Frozen)) shouldBe "Prezzo congelato: 17,00 €"

  test("a typed price wins over the frozen and the customer price and is sent"):
    val ctx = Context.of(List(fabric), List(line(fabricId, Some("TES-001"), Some("linear_meters"), false, "1.000", "17.0000")))
    val row = Row(1, fabricId.toString, "1", "20")
    pricePreview(row, ctx) shouldBe Some(PricePreview("20.0000", PriceSource.Manual))
    request(List(row), ctx) shouldBe Right(List(MaterialDto(fabricId, "1.000", Some("20.0000"))))

  test("no preview without a product or with an unreadable price"):
    pricePreview(Row(1, "", "1", ""), context) shouldBe None
    pricePreview(Row(1, fabricId.toString, "1", "x"), context) shouldBe None

  // ---- costs -------------------------------------------------------------------------------------

  test("line costs are rounded HALF_UP to the cent and the total sums the rounded lines"):
    // 3 × 0.1235 = 0.3705 → 0.37; 0.125 m × 18.50 = 2.3125 → 2.31; 1 × 0.005 = 0.005 → 0.01 (HALF_UP)
    val rows = List(Row(1, screwId.toString, "3", ""), Row(2, fabricId.toString, "0,125", ""), Row(3, screwId.toString, "1", "0,005"))
    rows.map(lineCost(_, context)) shouldBe List(Some("0.37"), Some("2.31"), Some("0.01"))
    totalCost(rows, context) shouldBe "2.69"

  test("unreadable lines have no cost and do not count in the total"):
    val rows = List(Row(1, fabricId.toString, "x", ""), Row(2, "", "2", ""), Row(3, fabricId.toString, "2", ""))
    rows.map(lineCost(_, context)) shouldBe List(None, None, Some("37.00"))
    totalCost(rows, context) shouldBe "37.00"
    totalCost(Nil, context) shouldBe "0.00"

  // ---- wire format -------------------------------------------------------------------------------

  test("request lines encode an absent price as null, which the server reads as 'freeze or keep'"):
    MaterialsUpdateRequest(List(MaterialDto(fabricId, "2.500", None))).asJson.noSpaces shouldBe
      s"""{"materials":[{"productId":"$fabricId","quantity":"2.500","unitPrice":null}]}"""

  test("order responses without the material fields still decode (no materials, zero cost)"):
    val json =
      s"""{"id":"$fabricId","number":"ORD-1","customerId":"$screwId","status":"in_progress","priority":"normal","creationDate":"2026-10-01",
         |"deliveryDate":"2026-11-01","promisedDeliveryDate":null,"description":null,"dependencies":[],
         |"manufacturings":[{"id":"$archivedId","code":"COP-01","completionDate":"2026-10-20","status":"not_started","startedAt":null,
         |"endedAt":null,"pauseReason":null,"tasks":[],"dependencies":[],"description":null}]}""".stripMargin
    val order = decode[OrderResponse](json)
    order.map(_.materialsCost) shouldBe Right("0.00")
    order.map(_.manufacturings.map(m => (m.materials, m.materialsCost))) shouldBe Right(List((Nil, "0.00")))

  test("order responses with materials decode the frozen lines and costs"):
    val json =
      s"""{"id":"$fabricId","number":"ORD-1","customerId":"$screwId","status":"in_progress","priority":"normal","creationDate":"2026-10-01",
         |"deliveryDate":"2026-11-01","promisedDeliveryDate":null,"description":null,"dependencies":[],"materialsCost":"46.25",
         |"manufacturings":[{"id":"$archivedId","code":"COP-01","completionDate":"2026-10-20","status":"not_started","startedAt":null,
         |"endedAt":null,"pauseReason":null,"tasks":[],"dependencies":[],"description":null,"preferredEmployeeId":null,"materialsCost":"46.25",
         |"materials":[{"productId":"$fabricId","internalCode":"TES-001","description":"Tessuto blu","unit":"linear_meters","archived":false,
         |"quantity":"2.500","unitPrice":"18.5000","lineCost":"46.25"}]}]}""".stripMargin
    val order = decode[OrderResponse](json)
    order.map(_.materialsCost) shouldBe Right("46.25")
    order.map(_.manufacturings.flatMap(_.materials)) shouldBe
      Right(List(MaterialResponse(fabricId, Some("TES-001"), Some("Tessuto blu"), Some("linear_meters"), false, "2.500", "18.5000", "46.25")))

  test("movement responses without the consumption fields still decode"):
    val json =
      """{"id":"00000000-0000-0000-0000-0000000000d1","sequence":1,"kind":"unload","on":"2026-10-20","quantity":"1.000","delta":"-1.000",
        |"unitCost":null,"value":null,"supplier":null,"mpnCode":null,"reason":"x","note":null,"voided":false,"voidedOn":null,"voidReason":null,
        |"balance":"9.000"}""".stripMargin
    decode[MovementResponse](json).map(m => (m.orderId, m.orderNumber)) shouldBe Right((None, None))
end MaterialDraftsSpec
