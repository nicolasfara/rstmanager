package io.github.nicolasfara.rstmanager.service.codecs

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.codecs.CommonCodecs.given
import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, SignedQuantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ ProductId, ProductService }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ SupplierId, SupplierService }

import io.circe.{ Decoder, Json }
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class WarehouseCodecsTest extends AnyFlatSpecLike:
  "UnitOfMeasure codec" should "write and read the stable wire names" in:
    UnitOfMeasure.values.foreach { unit =>
      unit.asJson shouldEqual Json.fromString(unit.wireName)
      Json.fromString(unit.wireName).as[UnitOfMeasure] shouldEqual Right(unit)
    }
    Json.fromString("LinearMeters").as[UnitOfMeasure].isLeft shouldEqual true

  "Fixed-point codecs" should "persist the integer count of units and re-validate it on read" in:
    UnitPrice.applyUnsafe(37000L).asJson shouldEqual Json.fromLong(37000L)
    Json.fromLong(-1L).as[UnitPrice].isLeft shouldEqual true
    Json.fromLong(10000000001L).as[UnitPrice].isLeft shouldEqual true
    Json.fromLong(-1L).as[Quantity].isLeft shouldEqual true
    Json.fromLong(-2500L).as[SignedQuantity].map(_.value) shouldEqual Right(-2500L)
    Json.fromLong(-1234L).as[Amount].map(_.value) shouldEqual Right(-1234L)
    Json.fromString("3.70").as[UnitPrice].isLeft shouldEqual true

  "Warehouse notification codecs" should "round-trip" in:
    val supplierChanged = SupplierService.Notification.SupplierChanged(SupplierId(UUID.fromString("00000000-0000-0000-0000-000000000301").nn))
    val productChanged = ProductService.Notification.ProductChanged(ProductId(UUID.fromString("00000000-0000-0000-0000-000000000401").nn))
    Decoder[SupplierService.Notification].decodeJson(supplierChanged.asJson) shouldEqual Right(supplierChanged)
    Decoder[ProductService.Notification].decodeJson(productChanged.asJson) shouldEqual Right(productChanged)
end WarehouseCodecsTest
