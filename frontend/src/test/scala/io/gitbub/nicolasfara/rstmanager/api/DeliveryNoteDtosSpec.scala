package io.gitbub.nicolasfara.rstmanager.api

import java.util.UUID

import io.circe.parser.decode
import io.circe.syntax.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import Dtos.*

/** Wire format of the delivery note DTOs and of the fields they added to the MPN and movement DTOs (legacy payloads must still decode). */
final class DeliveryNoteDtosSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares these DTOs structurally, so it needs CanEqual instances.
  private given CanEqual[MpnDto, MpnDto] = CanEqual.derived
  private given CanEqual[DeliveryNoteLineResponse, DeliveryNoteLineResponse] = CanEqual.derived

  private val supplierId = UUID.fromString("00000000-0000-0000-0000-0000000000a1").nn
  private val materialId = UUID.fromString("00000000-0000-0000-0000-0000000000f1").nn
  private val noteId = UUID.fromString("00000000-0000-0000-0000-0000000000e1").nn

  test("an MPN without the expected price still decodes, and the price round-trips when present"):
    decode[MpnDto](s"""{"code":"AC-152","supplierId":"$supplierId","description":null}""") shouldBe Right(MpnDto("AC-152", supplierId, None, None))
    val priced = MpnDto("AC-152", supplierId, Some("Acrilico"), Some("3.5000"))
    priced.asJson.noSpaces shouldBe s"""{"code":"AC-152","supplierId":"$supplierId","description":"Acrilico","expectedUnitCost":"3.5000"}"""
    decode[MpnDto](priced.asJson.noSpaces) shouldBe Right(priced)

  test("movement responses decode the delivery note of a purchase, and default it when absent"):
    val base =
      """{"id":"00000000-0000-0000-0000-0000000000d1","sequence":1,"kind":"purchase","on":"2026-10-20","quantity":"1.000","delta":"1.000",
        |"unitCost":"1.0000","value":"1.00","supplier":null,"mpnCode":"AC-152","reason":null,"note":null,"voided":false,"voidedOn":null,
        |"voidReason":null,"balance":"1.000"""".stripMargin
    decode[MovementResponse](base + "}").map(m => (m.deliveryNoteId, m.deliveryNoteNumber)) shouldBe Right((None, None))
    decode[MovementResponse](base + s""","deliveryNoteId":"$noteId","deliveryNoteNumber":"1234/A"}""")
      .map(m => (m.deliveryNoteId, m.deliveryNoteNumber)) shouldBe Right((Some(noteId), Some("1234/A")))

  test("delivery note requests encode the contract fields"):
    val request =
      DeliveryNoteRequest("1234/A", "2026-10-30", supplierId, None, List(DeliveryNoteLineRequest(materialId, "CN-1", true, "2.500", "3.5000")))
    request.asJson.noSpaces shouldBe
      s"""{"number":"1234/A","date":"2026-10-30","supplierId":"$supplierId","note":null,"lines":[{"materialId":"$materialId","mpnCode":"CN-1",""" +
      """"newMpn":true,"quantity":"2.500","unitCost":"3.5000"}]}"""
    CancelDeliveryNoteRequest("doppio").asJson.noSpaces shouldBe """{"reason":"doppio"}"""

  test("delivery note responses decode, optional fields absent or null"):
    val json =
      s"""{"id":"$noteId","number":"1234/A","date":"2026-10-30","supplierId":"$supplierId","supplierName":"Tessiture Rossi","supplierActive":true,
         |"status":"cancelled","registeredOn":"2026-10-30","changedOn":null,"cancelledOn":"2026-10-31","cancelReason":"doppio",
         |"lines":[{"materialId":"$materialId","internalCode":"TES-001","description":"Tessuto","unit":"linear_meters","mpnCode":"AC-152",
         |"quantity":"2.500","unitCost":"3.5000","total":"8.75","applied":false}],"total":"8.75"}""".stripMargin
    val note = decode[DeliveryNoteResponse](json)
    note.map(n => (n.status, n.note, n.changedOn, n.cancelledOn, n.cancelReason, n.total)) shouldBe
      Right(("cancelled", None, None, Some("2026-10-31"), Some("doppio"), "8.75"))
    note.map(_.lines) shouldBe
      Right(List(DeliveryNoteLineResponse(materialId, "TES-001", "Tessuto", "linear_meters", "AC-152", "2.500", "3.5000", "8.75", false)))
end DeliveryNoteDtosSpec
