package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the kind switch of the stock movement dialog and the fields it carries over. */
final class StockDialogSpec extends AnyFunSuite with Matchers:

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

  test("every switch kind maps to its form and back, in Carico | Scarico | Rettifica order"):
    StockDialog.movementKinds.map(_._2) shouldBe List("Carico", "Scarico", "Rettifica")
    StockDialog.movementKinds.foreach { (kind, _) =>
      val dialog = StockDialog.movement(material, kind)
      dialog.flatMap(_.movementKind) shouldBe Some(kind)
      dialog.map(_.material.internalCode) shouldBe Some(material.internalCode)
    }
    StockDialog.movement(material, "purchase") shouldBe Some(StockDialog.Purchase(material))
    StockDialog.movement(material, "unload") shouldBe Some(StockDialog.Unload(material))
    StockDialog.movement(material, "adjustment") shouldBe Some(StockDialog.Adjust(material))

  test("the movements list has no kind, and an unknown kind opens nothing"):
    StockDialog.Movements(material).movementKind shouldBe None
    StockDialog.movement(material, "consumption") shouldBe None

  test("switching to another kind opens its form; picking the current kind (or an unknown one) rebuilds nothing"):
    StockDialog.switchKind(StockDialog.Purchase(material), "unload") shouldBe Some(StockDialog.Unload(material))
    StockDialog.switchKind(StockDialog.Unload(material), "adjustment") shouldBe Some(StockDialog.Adjust(material))
    StockDialog.switchKind(StockDialog.Purchase(material), "purchase") shouldBe None
    StockDialog.switchKind(StockDialog.Adjust(material), "consumption") shouldBe None

  private val today = "2026-11-20"

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
end StockDialogSpec
