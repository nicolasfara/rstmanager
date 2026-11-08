package io.gitbub.nicolasfara.rstmanager.api

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import Dtos.*

/** Wire format of the material import response, as sent by `POST /warehouse/materials/import`. */
final class MaterialImportDtosSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares the DTO structurally, so it needs a CanEqual instance.
  private given CanEqual[MaterialImportResponse, MaterialImportResponse] = CanEqual.derived

  test("the import response decodes the contract fields"):
    val json = """{"dryRun":true,"created":["ANELLA4X30","PUNTANERA"],"skipped":["TES-001"],"newSuppliers":["Nautea"],"mpns":3}"""
    decode[MaterialImportResponse](json) shouldBe
      Right(MaterialImportResponse(true, List("ANELLA4X30", "PUNTANERA"), List("TES-001"), List("Nautea"), 3))

  test("the import response round-trips, empty lists included"):
    val response = MaterialImportResponse(false, Nil, List("A", "B"), Nil, 0)
    response.asJson.noSpaces shouldBe """{"dryRun":false,"created":[],"skipped":["A","B"],"newSuppliers":[],"mpns":0}"""
    decode[MaterialImportResponse](response.asJson.noSpaces) shouldBe Right(response)

  test("the import rejection decodes as an ApiError with one detail per problem"):
    val json =
      """{"code":"import-rejected","message":"Caricamento annullato: nessun materiale è stato salvato.",
        |"details":["Riga 3, CODICE: obbligatorio","Righe 171, 172 (FILOCERATO): UNITA' DI MISURA diverse: kg, pz"]}""".stripMargin
    decode[ApiError](json).map(e => (e.code, e.details.size)) shouldBe Right(("import-rejected", 2))
end MaterialImportDtosSpec
