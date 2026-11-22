package io.gitbub.nicolasfara.rstmanager.api

import java.util.UUID

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import Dtos.*

/** Wire format of the manufacturing multiplier ×N, the per-unit catalog materials and the material requirements. */
final class ManufacturingQuantityDtosSpec extends AnyFunSuite with Matchers:

  private given CanEqual[CatalogMaterialResponse, CatalogMaterialResponse] = CanEqual.derived
  private given CanEqual[MaterialRequirementsResponse, MaterialRequirementsResponse] = CanEqual.derived

  private val id = "00000000-0000-0000-0000-000000000001"
  private def uuid(s: String): UUID = UUID.fromString(s).nn

  test("a manufacturing read back without quantity (older server) decodes as done once"):
    val json =
      s"""{"id":"$id","code":"M","completionDate":"2026-08-01","status":"not_started","startedAt":null,"endedAt":null,"pauseReason":null,
         |"tasks":[],"dependencies":[],"description":null}""".stripMargin
    decode[ManufacturingResponse](json).map(_.quantity) shouldBe Right(1)
    decode[ManufacturingResponse](json.replace("\"description\":null", "\"description\":null,\"quantity\":4")).map(_.quantity) shouldBe Right(4)

  test("a catalog manufacturing without materials decodes them as empty; with materials they decode in full"):
    val json = s"""{"id":"$id","code":"M","name":"N","description":null,"taskIds":[],"tasks":[],"dependencies":[],"totalRequiredMinutes":0}"""
    decode[ManufacturingCatalogResponse](json).map(_.materials) shouldBe Right(Nil)
    val withMaterials = json.dropRight(1) +
      s""","materials":[{"materialId":"$id","internalCode":"TES-001","description":"Tessuto","unit":"linear_meters","archived":true,"quantity":"2.500"}]}"""
    decode[ManufacturingCatalogResponse](withMaterials).map(_.materials) shouldBe
      Right(List(CatalogMaterialResponse(uuid(id), Some("TES-001"), Some("Tessuto"), Some("linear_meters"), true, "2.500")))

  test("the requests carry quantity and per-unit materials only when set"):
    val dto = ManufacturingDto("M", "2026-08-01T00:00:00.000Z", "not_started", Nil, Nil, None, None, None, None)
    dto.asJson.hcursor.downField("quantity").focus.flatMap(_.asNull) shouldBe Some(())
    dto.copy(quantity = Some(3)).asJson.hcursor.downField("quantity").as[Int] shouldBe Right(3)
    val request = ManufacturingCatalogRequest("M", "N", None, Nil, Nil, None, None, Some(List(CatalogMaterialDto(uuid(id), "1.000"))))
    request.asJson.noSpaces should include(s""""materials":[{"materialId":"$id","quantity":"1.000"}]""")

  test("the material requirements decode, negative availability included"):
    val json =
      s"""{"at":"2026-11-22","materials":[{"materialId":"$id","internalCode":"VIT-1","description":"Vite","unit":"pieces","stock":"-1.000",
         |"committed":"2.000","available":"-3.000"}],"shortages":[{"orderId":"$id","orderNumber":"ORD-1","manufacturingId":"$id",
         |"manufacturingCode":"MFG-1","materialId":"$id","required":"2.000","missing":"2.000"}],"unknownMaterials":[]}""".stripMargin
    decode[MaterialRequirementsResponse](json) shouldBe Right(
      MaterialRequirementsResponse(
        "2026-11-22",
        List(MaterialRequirementResponse(uuid(id), "VIT-1", "Vite", "pieces", "-1.000", "2.000", "-3.000")),
        List(MaterialShortageResponse(uuid(id), "ORD-1", uuid(id), "MFG-1", uuid(id), "2.000", "2.000")),
        Nil,
      ),
    )
end ManufacturingQuantityDtosSpec
