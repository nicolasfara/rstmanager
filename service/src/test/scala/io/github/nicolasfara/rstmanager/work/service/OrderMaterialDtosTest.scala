package io.github.nicolasfara.rstmanager.work.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Quantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ ProductCode, ProductDescription, ProductId, ProductInfo, ProductRecord }
import io.github.nicolasfara.rstmanager.warehouse.service.ProductApp.StoredProduct
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledMaterial }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderService }
import io.github.nicolasfara.rstmanager.work.service.OrderDtos.*
import io.github.nicolasfara.rstmanager.work.service.OrderDtos.given

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class OrderMaterialDtosTest extends AnyFlatSpecLike:
  private val profileId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000901").nn
  private val screwsId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000902").nn
  private val oldId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000903").nn
  private val unknownId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000904").nn
  private val manufacturingId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000701").nn

  private def stored(id: UUID, code: String, unit: UnitOfMeasure, price: Long, archived: Boolean): (ProductId, StoredProduct) =
    val info = ProductInfo(
      ProductId(id),
      code.refineUnsafe[ProductCode],
      s"Prodotto $code".refineUnsafe[ProductDescription],
      None,
      None,
      unit,
      UnitPrice.applyUnsafe(price),
      Nil,
    )
    ProductId(id) -> StoredProduct(ProductRecord.created(info, LocalDate.parse("2026-06-01")), Option.when(archived)(LocalDate.parse("2026-07-01")))

  private val catalog: MaterialCatalog = MaterialCatalog(
    Map(
      stored(profileId, "PRF-1", UnitOfMeasure.LinearMeters, 185000L, archived = false),
      stored(screwsId, "VIT-1", UnitOfMeasure.Pieces, 500L, archived = false),
      stored(oldId, "OLD-1", UnitOfMeasure.Kilograms, 30000L, archived = true),
    ),
  )

  private def material(id: UUID, quantity: Long, price: Long): ScheduledMaterial =
    ScheduledMaterial(ProductId(id), Quantity.applyUnsafe(quantity), UnitPrice.applyUnsafe(price))

  private def resolve(lines: List[MaterialDto], current: List[ScheduledMaterial]): ValidatedNec[String, List[ScheduledMaterial]] =
    catalog.resolve("materials", lines, current)

  private def errorsOf(result: ValidatedNec[String, List[ScheduledMaterial]]): List[String] =
    result.fold(_.toChain.toList, materials => fail(s"Unexpectedly valid: $materials"))

  "MaterialCatalog" should "freeze the product's current customer price when a line has none" in:
    resolve(List(MaterialDto(profileId, "2.5", None), MaterialDto(screwsId, "12", Some("0.0400"))), Nil).toEither shouldEqual
      Right(List(material(profileId, 2500L, 185000L), material(screwsId, 12000L, 400L)))

  it should "keep the price already frozen in the manufacturing when a line has none" in:
    resolve(List(MaterialDto(profileId, "3", None)), List(material(profileId, 1000L, 170000L))).toEither shouldEqual
      Right(List(material(profileId, 3000L, 170000L)))

  it should "let a line already in the manufacturing keep an archived product, but not add one anew" in:
    resolve(List(MaterialDto(oldId, "1.25", None)), List(material(oldId, 1000L, 25000L))).toEither shouldEqual
      Right(List(material(oldId, 1250L, 25000L)))
    errorsOf(resolve(List(MaterialDto(oldId, "1", None)), Nil)) shouldEqual
      List("materials[0].productId: il prodotto OLD-1 è archiviato e non può essere aggiunto ai materiali")

  it should "report every problem of the lines, in Italian" in:
    val lines = List(
      MaterialDto(unknownId, "1", None),
      MaterialDto(screwsId, "1.5", None),
      MaterialDto(profileId, "0", None),
      MaterialDto(profileId, "abc", Some("1,5")),
    )
    errorsOf(resolve(lines, Nil)) shouldEqual List(
      s"materials[0].productId: il prodotto $unknownId non esiste",
      "materials[1].quantity: il prodotto VIT-1 è gestito a pezzi, la quantità deve essere un numero intero",
      "materials[2].quantity: la quantità deve essere maggiore di zero",
      "materials[3].quantity: 'abc' non è un numero decimale (usa cifre e il punto '.' come separatore decimale)",
      "materials[3].unitPrice: '1,5' non è un numero decimale (usa cifre e il punto '.' come separatore decimale)",
      "materials: il prodotto PRF-1 compare in più righe; indica la quantità totale in una sola riga",
    )

  "MaterialsUpdateRequest" should "become the command replacing the manufacturing's materials" in:
    MaterialsUpdateRequest(List(MaterialDto(profileId, "2.500", None))).toCommand(manufacturingId, catalog, Nil).toEither.map {
      case OrderService.Command.SetManufacturingMaterials(id, materials) => (id.value, materials)
      case other => fail(s"Unexpected command $other")
    } shouldEqual Right((manufacturingId, List(material(profileId, 2500L, 185000L))))

  "ManufacturingDto" should "still accept a request without materials" in:
    val json = ManufacturingDto.example.asJson.mapObject(_.remove("materials"))
    val dto = json.as[ManufacturingDto].fold(error => fail(error.getMessage), identity)

    dto.materials shouldBe None
    dto.toDomain("manufacturing", manufacturingId, MaterialCatalog.empty).map(_.info.materials).toEither shouldEqual Right(Nil)

  it should "carry its materials into the scheduled manufacturing" in:
    val dto = ManufacturingDto.example.copy(materials = Some(List(MaterialDto(profileId, "2", None))))
    dto.toDomain("manufacturing", manufacturingId, catalog).map(_.info.materials).toEither shouldEqual Right(
      List(material(profileId, 2000L, 185000L)),
    )
    dto.productIds shouldEqual List(ProductId(profileId))

  "OrderResponse" should "show the materials with the product master data and the costs" in:
    val request = OrderRequest.example.copy(manufacturings =
      List(ManufacturingDto.example.copy(materials = Some(List(MaterialDto(profileId, "2.5", None), MaterialDto(oldId, "1", Some("3")))))),
    )
    val withOld =
      MaterialCatalog(catalog.products.updated(ProductId(oldId), stored(oldId, "OLD-1", UnitOfMeasure.Kilograms, 0L, archived = false)._2))
    val (data, promised) = request.toDomain(UUID.randomUUID().nn, withOld).fold(errors => fail(errors.toChain.toList.mkString), identity)
    val order = Order.InProgressOrder(data, promised)
    // The archived product is shown as archived, an unknown one by its id only.
    val products = catalog.products.removed(ProductId(profileId))
    val response = OrderResponse.fromDomain(order, products).getOrElse(fail("No response"))

    OrderResponse.productIds(order) shouldEqual List(ProductId(profileId), ProductId(oldId))
    response.materialsCost shouldEqual "49.25"
    response.manufacturings.map(_.materialsCost) shouldEqual List("49.25")
    response.manufacturings.flatMap(_.materials) shouldEqual List(
      MaterialResponse(profileId, None, None, None, archived = false, "2.500", "18.5000", "46.25"),
      MaterialResponse(oldId, Some("OLD-1"), Some("Prodotto OLD-1"), Some("kilograms"), archived = true, "1.000", "3.0000", "3.00"),
    )

  it should "write the new fields in JSON" in:
    val manufacturing: ScheduledManufacturing =
      ManufacturingDto.example
        .copy(materials = None)
        .toDomain("manufacturing", manufacturingId, MaterialCatalog.empty)
        .fold(_ => fail("Invalid"), identity)
    val json = ManufacturingDto.fromDomain(manufacturing, Map.empty).asJson

    json.hcursor.get[List[Json]]("materials") shouldEqual Right(Nil)
    json.hcursor.get[String]("materialsCost") shouldEqual Right("0.00")
    parse("""{"materials":[{"productId":"00000000-0000-0000-0000-000000000901","quantity":"2","unitPrice":null}]}""")
      .flatMap(_.as[MaterialsUpdateRequest]) shouldEqual Right(MaterialsUpdateRequest(List(MaterialDto(profileId, "2", None))))
end OrderMaterialDtosTest
