package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the kind switch of the material sheet's movement form and the fields it carries over. */
final class MovementKindSpec extends AnyFunSuite with Matchers:

  private val material: MaterialResponse =
    MaterialResponse(
      UUID.fromString("00000000-0000-0000-0000-0000000000f1").nn,
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

  private val today = "2026-11-20"

  test("the switch lists Carico | Scarico | Rettifica, and every key maps back to its kind"):
    MovementKind.options.map(_._2) shouldBe List("Carico", "Scarico", "Rettifica")
    MovementKind.options.foreach((key, _) => MovementKind.fromKey(key).map(_.key) shouldBe Some(key))
    MovementKind.fromKey("consumption") shouldBe None

  test("submit labels and confirmations name the kind, with the right gender"):
    MovementKind.Purchase.submitLabel shouldBe "Registra carico"
    MovementKind.Adjustment.submitLabel shouldBe "Registra rettifica"
    MovementKind.Purchase.recordedText("TES-001") shouldBe "Carico registrato su TES-001"
    MovementKind.Unload.recordedText("TES-001") shouldBe "Scarico registrato su TES-001"
    MovementKind.Adjustment.recordedText("TES-001") shouldBe "Rettifica registrata su TES-001"

  test("a blank draft has the asked kind; picking the current kind keeps the draft as typed"):
    MovementKind.values.foreach(kind => MovementForm.blank(kind, material, today).kind shouldBe kind)
    val typed = MovementForm.Draft.Unload(MovementForm.unload(material, today).copy(quantity = "2"))
    MovementForm.switchKind(typed, MovementKind.Unload, material, today) shouldBe typed

  test("switching kind hands the shared fields to the new form"):
    val unload = MovementForm.Draft.Unload(MovementForm.unload(material, today).copy(quantity = "2", reason = "Campionatura"))
    MovementForm.switchKind(unload, MovementKind.Adjustment, material, today) match
      case MovementForm.Draft.Adjustment(state) =>
        state.delta shouldBe "-2"
        state.reason shouldBe "Campionatura"
      case other => fail(s"expected an adjustment, got $other")

  test("a blank form has nothing unsaved, even after a kind switch; a typed value has"):
    val blankPurchase = MovementForm.blank(MovementKind.Purchase, material, today)
    MovementForm.unsaved(blankPurchase, material, today) shouldBe None
    MovementForm.unsaved(MovementForm.switchKind(blankPurchase, MovementKind.Unload, material, today), material, today) shouldBe None
    val typed = MovementForm.Draft.Purchase(MovementForm.purchase(material, today).copy(note = "DDT 12"))
    MovementForm.unsaved(typed, material, today) shouldBe Some(typed)

  test("after a movement the focus goes to the MPN of a purchase with MPNs, else to the quantity"):
    MovementForm.blank(MovementKind.Purchase, material, today).firstField shouldBe MovementForm.Field.Quantity
    val withMpn = material.copy(mpns = List(MpnDto("AC-152", material.id, None)))
    MovementForm.blank(MovementKind.Purchase, withMpn, today).firstField shouldBe MovementForm.Field.Mpn
    MovementForm.blank(MovementKind.Adjustment, withMpn, today).firstField shouldBe MovementForm.Field.Quantity

  test("a purchase hands quantity, cost, and day to an adjustment, as an incoming variation; the note is not a reason"):
    val purchase = MovementForm.purchase(material, today).copy(quantity = " 2,5 ", unitCost = "3,50", on = "2026-11-18", note = "DDT 12")
    val adjustment = MovementForm.adjustment(material, today).carried(purchase.carry)
    adjustment.delta shouldBe "2,5"
    adjustment.unitCost shouldBe "3,50"
    adjustment.on shouldBe "2026-11-18"
    adjustment.reason shouldBe ""
    adjustment.costEnabled shouldBe true

  test("an unload becomes a negative variation with its reason, and back"):
    val unload = MovementForm.unload(material, today).copy(quantity = "2", on = "2026-11-19", reason = "Campionatura")
    val adjustment = MovementForm.adjustment(material, today).carried(unload.carry)
    adjustment.delta shouldBe "-2"
    adjustment.reason shouldBe "Campionatura"
    adjustment.on shouldBe "2026-11-19"
    val back = MovementForm.unload(material, today).carried(adjustment.carry)
    back shouldBe unload

  test("an adjustment hands the magnitude of its variation to a purchase, with its cost; the MPN and note stay the purchase's"):
    val adjustment = MovementForm.adjustment(material, today).copy(delta = "+10", unitCost = "4", on = "2026-11-17", reason = "Inventario")
    val purchase = MovementForm.purchase(material, today).carried(adjustment.carry)
    purchase.quantity shouldBe "10"
    purchase.unitCost shouldBe "4"
    purchase.on shouldBe "2026-11-17"
    purchase.note shouldBe ""
    MovementForm.unload(material, today).carried(adjustment.copy(delta = "-3").carry).quantity shouldBe "3"

  test("carrying a blank form keeps the next one blank (and pristine)"):
    val adjustment = MovementForm.adjustment(material, today).carried(MovementForm.unload(material, today).carry)
    adjustment shouldBe MovementForm.adjustment(material, today)
    adjustment.pristine shouldBe true
    MovementForm.purchase(material, today).carried(MovementForm.adjustment(material, today).carry) shouldBe MovementForm.purchase(material, today)
end MovementKindSpec
