package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Quantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialCode, MaterialDescription, MaterialId, MaterialInfo, MaterialRecord }
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp.StoredMaterial
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ Manufacturing, ManufacturingMaterial }
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskDuration, TaskId, TaskName }
import io.github.nicolasfara.rstmanager.work.service.ManufacturingHttpApi.{
  CatalogMaterialDto,
  CatalogMaterialResponse,
  ManufacturingRequest,
  ManufacturingResponse,
}
import io.github.nicolasfara.rstmanager.work.service.ManufacturingHttpApi.given
import io.github.nicolasfara.rstmanager.work.service.OrderDtos.{ ManufacturingDto, MaterialCatalog }
import io.github.nicolasfara.rstmanager.work.service.OrderDtos.given

import cats.data.ValidatedNec
import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import sttp.model.StatusCode

/** The catalog materials per unit and the ×N multiplier of the order manufacturings, at the HTTP DTO edge. */
class ManufacturingQuantityMaterialsDtosTest extends AnyFlatSpecLike:
  private val id: UUID = UUID.fromString("00000000-0000-0000-0000-000000000501").nn
  private val taskId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000301").nn
  private val profileId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000901").nn
  private val screwsId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000902").nn
  private val oldId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000903").nn
  private val unknownId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000904").nn

  private def stored(id: UUID, code: String, unit: UnitOfMeasure, archived: Boolean): (MaterialId, StoredMaterial) =
    val info = MaterialInfo(
      MaterialId(id),
      code.refineUnsafe[MaterialCode],
      s"Materiale $code".refineUnsafe[MaterialDescription],
      None,
      None,
      unit,
      UnitPrice.applyUnsafe(10000L),
      Nil,
    )
    MaterialId(id) -> StoredMaterial(
      MaterialRecord.created(info, LocalDate.parse("2026-06-01")),
      Option.when(archived)(LocalDate.parse("2026-07-01")),
    )

  private val materials: Map[MaterialId, StoredMaterial] = Map(
    stored(profileId, "PRF-1", UnitOfMeasure.LinearMeters, archived = false),
    stored(screwsId, "VIT-1", UnitOfMeasure.Pieces, archived = false),
    stored(oldId, "OLD-1", UnitOfMeasure.Kilograms, archived = true),
  )

  private val catalog: MaterialCatalog = MaterialCatalog(materials)

  private def request(lines: List[CatalogMaterialDto]): ManufacturingRequest =
    ManufacturingRequest("MFG-1", "Tenda", None, List(taskId), Nil, None, None, Some(lines))

  private def errors[A](value: ValidatedNec[String, A]): List[String] = value.fold(_.toChain.toList, result => fail(s"Unexpectedly valid: $result"))

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private val task: Task = Task(TaskId(taskId), "Taglio".refineUnsafe[TaskName], None, TaskDuration(60))

  "ManufacturingRequest.toDomain" should "parse the per-unit materials against the warehouse" in:
    val template = valid(request(List(CatalogMaterialDto(profileId, "2.5"), CatalogMaterialDto(screwsId, "4"))).toDomain(id, catalog, Nil))
    template.materials shouldEqual List(
      ManufacturingMaterial(MaterialId(profileId), Quantity(2500L)),
      ManufacturingMaterial(MaterialId(screwsId), Quantity(4000L)),
    )

  it should "default to no materials when the field is missing" in:
    val legacy = decode[ManufacturingRequest](s"""{"code":"MFG-1","name":"Tenda","description":null,"taskIds":["$taskId"],"dependencies":[]}""")
    legacy.map(_.materials) shouldEqual Right(None)
    legacy.map(value => valid(value.toDomain(id, catalog, Nil)).materials) shouldEqual Right(Nil)

  it should "reject unknown, archived, fractional pieces, zero and repeated materials, in Italian" in:
    val lines = List(
      CatalogMaterialDto(unknownId, "1"),
      CatalogMaterialDto(oldId, "1"),
      CatalogMaterialDto(screwsId, "1.5"),
      CatalogMaterialDto(profileId, "0"),
      CatalogMaterialDto(profileId, "1"),
    )
    errors(request(lines).toDomain(id, catalog, Nil)) shouldEqual List(
      s"materials[0].materialId: il materiale $unknownId non esiste",
      "materials[1].materialId: il materiale OLD-1 è archiviato e non può essere aggiunto alla lavorazione",
      "materials[2].quantity: il materiale VIT-1 è gestito a unità intere (pezzi/confezioni), la quantità deve essere un numero intero",
      "materials[3].quantity: la quantità deve essere maggiore di zero",
      "materials: il materiale PRF-1 compare in più righe; indica la quantità totale in una sola riga",
    )

  it should "let an update keep a material the template already had, even if archived since" in:
    val current = List(ManufacturingMaterial(MaterialId(oldId), Quantity(1000L)))
    valid(request(List(CatalogMaterialDto(oldId, "2"))).toDomain(id, catalog, current)).materials shouldEqual
      List(ManufacturingMaterial(MaterialId(oldId), Quantity(2000L)))

  it should "keep the current materials when the request omits them, and clear them only on an empty list" in:
    val current = List(ManufacturingMaterial(MaterialId(oldId), Quantity(1000L)), ManufacturingMaterial(MaterialId(unknownId), Quantity(2000L)))
    val omitted = request(Nil).copy(materials = None)
    valid(omitted.toDomain(id, MaterialCatalog.empty, current)).materials shouldEqual current
    valid(omitted.toDomain(id, MaterialCatalog.empty, Nil)).materials shouldEqual Nil
    valid(request(Nil).toDomain(id, MaterialCatalog.empty, current)).materials shouldEqual Nil

  "ManufacturingHttpApi.updateExisting" should "answer 404 for a missing template without validating the request" in:
    val replaced = Ref.unsafe[IO, Boolean](false)
    val result = ManufacturingHttpApi
      .updateExisting(id, IO.pure(Option.empty[Manufacturing]))(_ => replaced.set(true).as(Right("replaced")))
      .unsafeRunSync()
    result.leftMap(_._1) shouldEqual Left(StatusCode.NotFound)
    replaced.get.unsafeRunSync() shouldBe false

  it should "replace an existing template and turn a failing read into a 500" in:
    val template = valid(request(Nil).toDomain(id, MaterialCatalog.empty, Nil))
    ManufacturingHttpApi.updateExisting(id, IO.pure(Some(template)))(found => IO.pure(Right(found.id))).unsafeRunSync() shouldEqual
      Right(template.id)
    ManufacturingHttpApi
      .updateExisting(id, IO.raiseError[Option[Manufacturing]](new RuntimeException("database down")))(_ => IO.pure(Right(())))
      .unsafeRunSync()
      .leftMap(_._1) shouldEqual Left(StatusCode.InternalServerError)

  "ManufacturingResponse" should "show the materials with their warehouse master data and survive a JSON round trip" in:
    val template: Manufacturing =
      valid(request(List(CatalogMaterialDto(oldId, "1.25"))).toDomain(id, catalog, List(ManufacturingMaterial(MaterialId(oldId), Quantity(1L)))))
    val response = ManufacturingResponse.fromDomain(template, List(task), materials)
    response.materials shouldEqual List(CatalogMaterialResponse(oldId, Some("OLD-1"), Some("Materiale OLD-1"), Some("kilograms"), true, "1.250"))
    response.asJson.as[ManufacturingResponse] shouldEqual Right(response)
    ManufacturingResponse.fromDomain(template, List(task), Map.empty).materials shouldEqual
      List(CatalogMaterialResponse(oldId, None, None, None, false, "1.250"))

  "ManufacturingDto" should "take the ×N multiplier as is (default 1) and show it in the response" in:
    val base = ManufacturingDto.example
    val manufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000601").nn
    val scaled =
      valid(base.copy(quantity = Some(4)).toDomain("manufacturing", manufacturingId, catalog.copy(materials = materials ++ exampleMaterials)))
    (scaled.info.quantity: Int) shouldEqual 4
    scaled.info.tasks.toList.map(_.expectedDuration.value) shouldEqual base.tasks.map(_.expectedMinutes)
    ManufacturingDto.fromDomain(scaled, Map.empty).quantity shouldEqual 4
    (valid(base.copy(quantity = None).toDomain("manufacturing", manufacturingId, MaterialCatalog(exampleMaterials))).info.quantity: Int) shouldEqual 1

  it should "reject a multiplier out of 1 to 999" in:
    val manufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000601").nn
    List(0, 1000).map { n =>
      errors(ManufacturingDto.example.copy(quantity = Some(n)).toDomain("manufacturings[0]", manufacturingId, MaterialCatalog(exampleMaterials)))
    } shouldEqual List.fill(2)(List("manufacturings[0].quantity: La quantità della lavorazione deve essere compresa fra 1 e 999"))

  it should "decode a request without quantity" in:
    val json = ManufacturingDto.example.asJson.mapObject(_.remove("quantity"))
    json.as[ManufacturingDto].map(_.quantity) shouldEqual Right(None)
    json.mapObject(_.add("quantity", Json.fromInt(3))).as[ManufacturingDto].map(_.quantity) shouldEqual Right(Some(3))

  /** The material the order example names. */
  private def exampleMaterials: Map[MaterialId, StoredMaterial] =
    Map(stored(UUID.fromString("00000000-0000-0000-0000-000000000c01").nn, "EX-1", UnitOfMeasure.LinearMeters, archived = false))
end ManufacturingQuantityMaterialsDtosTest
