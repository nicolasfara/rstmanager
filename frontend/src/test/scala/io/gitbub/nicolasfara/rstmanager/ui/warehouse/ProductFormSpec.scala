package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import ProductForm.*

/** Unit tests for the pure product form: request building, validation messages, and the MPN rows editor. */
final class ProductFormSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares these DTOs structurally, so it needs CanEqual instances.
  private given CanEqual[MpnDto, MpnDto] = CanEqual.derived
  private given CanEqual[ProductRequest, ProductRequest] = CanEqual.derived

  private def uuid(s: String): UUID = UUID.fromString(s).nn

  private val supplierA = uuid("00000000-0000-0000-0000-0000000000a1")
  private val supplierB = uuid("00000000-0000-0000-0000-0000000000b2")
  private val productId = uuid("00000000-0000-0000-0000-0000000000f1")

  // Base fixture; tests derive their variant via `.copy` (project style forbids method default args).
  private val valid: State =
    ProductForm.empty.copy(internalCode = " tes-001 ", description = " Tessuto blu ", unit = "linear_meters", customerPrice = "18,5")

  private def row(key: Int, supplier: String, code: String): MpnRow = MpnRow(key, supplier, code, "")

  // ---- request building --------------------------------------------------------------------------

  test("request trims the text, converts the Italian price, and drops blank optional values"):
    val request = valid.copy(productType = "  ", notes = " per capottine ").request
    request shouldBe Some(ProductRequest("tes-001", "Tessuto blu", None, Some("per capottine"), "linear_meters", "18.5000", Nil))

  test("request maps filled MPN rows and ignores rows left completely blank"):
    val state =
      valid.copy(mpns = List(MpnRow(0, supplierA.toString, " ac-152 ", " Acrylic "), MpnRow(1, "", " ", ""), MpnRow(2, supplierB.toString, "X9", "")))
    state.errors shouldBe Nil
    state.request.map(_.mpns) shouldBe Some(List(MpnDto("ac-152", supplierA, Some("Acrylic")), MpnDto("X9", supplierB, None)))

  test("request is None while the form has errors"):
    valid.copy(internalCode = "").request shouldBe None
    valid.copy(customerPrice = "abc").request shouldBe None

  // ---- validation --------------------------------------------------------------------------------

  test("the empty form reports every required field"):
    ProductForm.empty.errors shouldBe List("Codice obbligatorio", "Descrizione obbligatoria", "Prezzo cliente obbligatorio")

  test("a valid form has no errors"):
    valid.errors shouldBe Nil

  test("an unparseable or over-precise price is reported as invalid"):
    for price <- List("abc", "1,23456", "-3", "1,2,3") do
      withClue(price)(valid.copy(customerPrice = price).errors shouldBe List("Prezzo non valido (es. 18,50, al massimo 4 decimali)"))
    valid.copy(customerPrice = "1.234,5678").errors shouldBe Nil

  test("an unknown unit is reported"):
    valid.copy(unit = "barrels").errors shouldBe List("Unità di misura non valida")

  test("MPN rows report a missing supplier or code with their 1-based position"):
    val state = valid.copy(mpns = List(row(0, supplierA.toString, "A1"), row(1, "", "B1"), row(2, supplierB.toString, " "), row(3, "garbage", "C")))
    // Row 3 has a supplier but only blanks as code: reported as a missing code, not dropped as blank; "garbage" is not a supplier id.
    state.errors shouldBe List("Fornitore mancante nella riga MPN 2", "Codice mancante nella riga MPN 3", "Fornitore mancante nella riga MPN 4")

  test("the same code of the same supplier twice is reported, comparing codes as the server stores them"):
    val state = valid.copy(mpns =
      List(
        row(0, supplierA.toString, "ac-1"),
        row(1, supplierB.toString, "AC-1"),
        row(2, supplierA.toString, " AC-1 "),
        row(3, supplierA.toString, "Z"),
      ),
    )
    state.errors shouldBe List("MPN ripetuto per lo stesso fornitore nelle righe 1, 3")
    state.request shouldBe None

  test("the same code of different suppliers is allowed"):
    valid.copy(mpns = List(row(0, supplierA.toString, "AC-1"), row(1, supplierB.toString, "AC-1"))).errors shouldBe Nil

  test("pristine is true only for a fresh, untouched create form"):
    ProductForm.empty.pristine shouldBe true
    ProductForm.empty.copy(unit = "kilograms", productType = "Tessuti").pristine shouldBe true
    ProductForm.empty.copy(description = "x").pristine shouldBe false
    ProductForm.empty.addRow.pristine shouldBe false
    ProductForm.empty.copy(editingId = Some(productId)).pristine shouldBe false

  // ---- rows editor -------------------------------------------------------------------------------

  test("addRow mints stable, increasing keys; removeRow and updateRow target a key"):
    val three = ProductForm.empty.addRow.addRow.addRow
    three.mpns.map(_.key) shouldBe List(0, 1, 2)
    val edited = three.removeRow(1).updateRow(2)(_.copy(code = "N"))
    edited.mpns.map(r => r.key -> r.code) shouldBe List(0 -> "", 2 -> "N")
    edited.addRow.mpns.map(_.key) shouldBe List(0, 2, 3)

  // ---- fromResponse ------------------------------------------------------------------------------

  private val response: ProductResponse =
    ProductResponse(
      productId,
      "TES-001",
      "Tessuto blu",
      Some("Tessuti"),
      None,
      "linear_meters",
      "1234.5000",
      List(MpnDto("AC-152", supplierA, Some("Acrylic")), MpnDto("X9", supplierB, None)),
      List(ProductSupplierDto(supplierA, "A", true), ProductSupplierDto(supplierB, "B", false)),
      "2026-10-01",
      false,
      None,
      StockDto("0.000", None, None, "0.00", false),
    )

  test("fromResponse prefills the form with an Italian price and keyed MPN rows"):
    val state = ProductForm.fromResponse(response)
    state.editingId shouldBe Some(productId)
    state.customerPrice shouldBe "1234,50"
    state.productType shouldBe "Tessuti"
    state.notes shouldBe ""
    state.mpns shouldBe List(MpnRow(0, supplierA.toString, "AC-152", "Acrylic"), MpnRow(1, supplierB.toString, "X9", ""))
    state.addRow.mpns.map(_.key) shouldBe List(0, 1, 2)

  test("fromResponse -> request round-trips the stored product"):
    ProductForm.fromResponse(response).request shouldBe Some(
      ProductRequest("TES-001", "Tessuto blu", Some("Tessuti"), None, "linear_meters", "1234.5000", response.mpns),
    )

  // ---- product type suggestions ------------------------------------------------------------------

  test("productTypes lists distinct non-blank types, case-insensitively, sorted"):
    val products =
      List(Some("Tessuti"), Some("ferramenta"), None, Some(" tessuti "), Some(" "), Some("Accessori")).map(t => response.copy(productType = t))
    ProductForm.productTypes(products) shouldBe List("Accessori", "ferramenta", "Tessuti")
end ProductFormSpec
