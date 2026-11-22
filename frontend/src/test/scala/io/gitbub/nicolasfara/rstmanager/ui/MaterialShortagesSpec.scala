package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import MaterialShortages.*

/** Unit tests for the planning view of the material requirements: shortages per slice and what to order per material. */
final class MaterialShortagesSpec extends AnyFunSuite with Matchers:

  private def uuid(s: String): UUID = UUID.fromString(s).nn

  private val fabric = uuid("00000000-0000-0000-0000-0000000000f1")
  private val screw = uuid("00000000-0000-0000-0000-0000000000f2")
  private val unknown = uuid("00000000-0000-0000-0000-0000000000f3")
  private val order1 = uuid("00000000-0000-0000-0000-0000000000a1")
  private val order2 = uuid("00000000-0000-0000-0000-0000000000a2")
  private val mfg1 = uuid("00000000-0000-0000-0000-0000000000b1")
  private val mfg2 = uuid("00000000-0000-0000-0000-0000000000b2")

  private val response = MaterialRequirementsResponse(
    "2026-11-22",
    List(
      MaterialRequirementResponse(fabric, "TES-001", "Tessuto", "linear_meters", "10.000", "14.500", "-4.500"),
      MaterialRequirementResponse(screw, "VIT-1", "Vite", "pieces", "0.000", "2.000", "-2.000"),
    ),
    List(
      MaterialShortageResponse(order1, "ORD-1", mfg1, "MFG-1", fabric, "12.000", "2.000"),
      MaterialShortageResponse(order1, "ORD-1", mfg1, "MFG-1", screw, "2.000", "2.000"),
      MaterialShortageResponse(order2, "ORD-2", mfg2, "MFG-2", fabric, "2.500", "2.500"),
    ),
    List(UnknownMaterialDemandResponse(order2, "ORD-2", mfg2, "MFG-2", unknown, "1.500")),
  )

  test("bySlice groups the shortages by order and manufacturing, with the material code and unit"):
    val slices = bySlice(response)
    slices.keySet shouldBe Set((order1, mfg1), (order2, mfg2))
    slices((order1, mfg1)) shouldBe List(SliceShortage("TES-001", "linear_meters", "2.000"), SliceShortage("VIT-1", "pieces", "2.000"))
    sliceTitle(slices((order1, mfg1))) shouldBe "Materiale da ordinare: TES-001 (mancano 2 m); VIT-1 (mancano 2 pz)"

  test("toOrder sums the missing quantity per material and lists each order once"):
    toOrder(response) shouldBe List(
      ToOrder(fabric, "TES-001", "Tessuto", "linear_meters", "4.500", List("ORD-1", "ORD-2")),
      ToOrder(screw, "VIT-1", "Vite", "pieces", "2.000", List("ORD-1")),
    )
    toOrder(response).map(toOrderText) shouldBe List(
      "TES-001 — Tessuto: mancano 4,5 m (ordini ORD-1, ORD-2)",
      "VIT-1 — Vite: mancano 2 pz (ordine ORD-1)",
    )

  test("the demand on unknown materials is listed apart"):
    unknownTexts(response) shouldBe List("Materiale non in magazzino 00000000: richiesti 1,5 (ordine ORD-2, lavorazione MFG-2)")
    toOrder(response.copy(shortages = Nil)) shouldBe Nil
end MaterialShortagesSpec
