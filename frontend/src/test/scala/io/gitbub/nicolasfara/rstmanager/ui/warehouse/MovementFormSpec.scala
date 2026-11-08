package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import MovementForm.*

/** Unit tests for the pure stock movement forms: validation messages, request building, and the purchase total preview. */
final class MovementFormSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares these DTOs structurally, so it needs CanEqual instances.
  private given CanEqual[PurchaseRequest, PurchaseRequest] = CanEqual.derived
  private given CanEqual[UnloadRequest, UnloadRequest] = CanEqual.derived
  private given CanEqual[AdjustmentRequest, AdjustmentRequest] = CanEqual.derived

  private def uuid(s: String): UUID = UUID.fromString(s).nn

  private val supplierA = uuid("00000000-0000-0000-0000-0000000000a1")
  private val supplierB = uuid("00000000-0000-0000-0000-0000000000b2")
  private val today = "2026-11-01"

  // Base fixture; tests derive their variant via `.copy` (project style forbids method default args).
  private val material: MaterialResponse =
    MaterialResponse(
      uuid("00000000-0000-0000-0000-0000000000f1"),
      "TES-001",
      "Tessuto blu",
      None,
      None,
      "linear_meters",
      "18.5000",
      List(MpnDto("AC-152", supplierA, None), MpnDto("X9", supplierB, None)),
      List(MaterialSupplierDto(supplierA, "Tessiture Rossi", true), MaterialSupplierDto(supplierB, "Vecchio", false)),
      "2026-10-01",
      false,
      None,
      StockDto("10.000", Some("3.5000"), Some("35.00"), "35.00", false),
    )

  private val pieces = material.copy(unit = "pieces")

  // ---- shared field rules ------------------------------------------------------------------------

  test("parseQuantity converts Italian decimals and rejects blank, zero, and too many decimals"):
    parseQuantity("2,5", "linear_meters") shouldBe Right("2.500")
    parseQuantity("1.234,5", "kilograms") shouldBe Right("1234.500")
    parseQuantity(" ", "linear_meters") shouldBe Left("Quantità obbligatoria")
    parseQuantity("0", "linear_meters") shouldBe Left("La quantità deve essere maggiore di zero")
    parseQuantity("1,2345", "linear_meters").isLeft shouldBe true
    parseQuantity("-1", "linear_meters").isLeft shouldBe true

  test("parseQuantity requires whole numbers for pieces"):
    parseQuantity("3", "pieces") shouldBe Right("3.000")
    parseQuantity("3,000", "pieces") shouldBe Right("3.000")
    parseQuantity("2,5", "pieces") shouldBe Left("Il materiale è gestito a pezzi: la quantità deve essere un numero intero")

  test("parseDelta accepts a sign and rejects zero and fractional pieces"):
    parseDelta("-2,5", "linear_meters") shouldBe Right("-2.500")
    parseDelta("10", "linear_meters") shouldBe Right("10.000")
    parseDelta("-0", "linear_meters") shouldBe Left("La variazione non può essere zero")
    parseDelta("-", "linear_meters").isLeft shouldBe true
    parseDelta("-1,5", "pieces").isLeft shouldBe true

  test("parseDay rejects blank, malformed, and future days"):
    parseDay("2026-10-20", today) shouldBe Right("2026-10-20")
    parseDay(today, today) shouldBe Right(today)
    parseDay("", today) shouldBe Left("Data obbligatoria")
    parseDay("2026-02-30", today) shouldBe Left("Data non valida")
    parseDay("2026-11-02", today) shouldBe Left("La data non può essere successiva a oggi")

  test("total multiplies exactly and rounds HALF_UP to the cent"):
    total("2.500", "3.5000") shouldBe "8.75"
    total("2.500", "3.3333") shouldBe "8.33" // 8.33325
    total("0.333", "0.1000") shouldBe "0.03" // 0.0333
    total("1.000", "0.0050") shouldBe "0.01" // 0.005 rounds up
    total("123456.789", "9999.9999") shouldBe "1234567877.65" // 1234567877.6543211

  // ---- purchase ----------------------------------------------------------------------------------

  test("mpnOptions label each MPN with its supplier, marking deleted ones"):
    mpnOptions(material).map(_.label) shouldBe List("AC-152 — Tessiture Rossi", "X9 — Vecchio (eliminato)")

  test("purchase starts dated today and preselects the only MPN"):
    val fresh = purchase(material, today)
    fresh.on shouldBe today
    fresh.mpnKey shouldBe ""
    purchase(material.copy(mpns = material.mpns.take(1)), today).selectedMpn.map(_.code) shouldBe Some("AC-152")

  test("purchase request carries the selected MPN, canonical decimals, the day, and a trimmed note"):
    val base = purchase(material, today)
    val state = base.copy(mpnKey = base.mpns(1).key, quantity = "25,5", unitCost = "3,5", on = "2026-10-20", note = " DDT 12 ")
    state.errors shouldBe Nil
    state.request shouldBe Some(PurchaseRequest("X9", supplierB, "25.500", "3.5000", Some("2026-10-20"), Some("DDT 12")))
    state.totalPreview shouldBe Some("89.25")
    state.copy(note = "  ").request.flatMap(_.note) shouldBe None

  test("purchase reports every problem and builds no request"):
    val state = purchase(pieces, today).copy(quantity = "1,5", unitCost = "abc", on = "2026-12-01")
    state.request shouldBe None
    state.totalPreview shouldBe None
    state.errors shouldBe List(
      "Seleziona l'MPN acquistato",
      "Il materiale è gestito a pezzi: la quantità deve essere un numero intero",
      "Costo unitario non valido (es. 3,50, al massimo 4 decimali)",
      "La data non può essere successiva a oggi",
    )

  test("purchase of a material without MPNs explains why it cannot be recorded"):
    val state = purchase(material.copy(mpns = Nil), today).copy(quantity = "1", unitCost = "1")
    state.errors shouldBe List("Il materiale non ha MPN: aggiungine uno in anagrafica prima di registrare un carico")

  test("purchase is pristine until a quantity or a cost is typed"):
    purchase(material, today).pristine shouldBe true
    purchase(material, today).copy(unitCost = "1").pristine shouldBe false

  // ---- unload ------------------------------------------------------------------------------------

  test("unload requires a positive quantity, a day, and a reason"):
    val state = unload(material, today)
    state.errors shouldBe List("Quantità obbligatoria", "Motivo obbligatorio")
    state.copy(quantity = "2", reason = " campionatura ").request shouldBe Some(UnloadRequest("2.000", Some(today), "campionatura"))

  test("unload of pieces rejects fractions"):
    unload(pieces, today).copy(quantity = "0,5", reason = "x").request shouldBe None

  // ---- adjustment --------------------------------------------------------------------------------

  test("adjustment allows a unit cost only for a positive delta"):
    val base = adjustment(material, today).copy(reason = "inventario", unitCost = "3,2")
    base.copy(delta = "40").costEnabled shouldBe true
    base.copy(delta = "40").request shouldBe Some(AdjustmentRequest("40.000", Some("3.2000"), Some(today), "inventario"))
    base.copy(delta = "-2").costEnabled shouldBe false
    base.copy(delta = "-2").request shouldBe Some(AdjustmentRequest("-2.000", None, Some(today), "inventario"))

  test("adjustment cost is optional, but must be valid when given"):
    val base = adjustment(material, today).copy(delta = "5", reason = "inventario")
    base.request.map(_.unitCost) shouldBe Some(None)
    base.copy(unitCost = "x").errors shouldBe List("Costo unitario non valido (es. 3,50, al massimo 4 decimali)")
    base.copy(delta = "-5", unitCost = "x").errors shouldBe Nil

  test("adjustment rejects zero, fractional pieces, and a missing reason"):
    adjustment(material, today).copy(delta = "0").errors shouldBe List("La variazione non può essere zero", "Motivo obbligatorio")
    adjustment(pieces, today).copy(delta = "-1,5", reason = "x").request shouldBe None
end MovementFormSpec
