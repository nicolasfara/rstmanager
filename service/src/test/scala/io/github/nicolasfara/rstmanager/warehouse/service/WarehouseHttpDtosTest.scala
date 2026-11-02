package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ ProductId, ProductRecord }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierId }
import io.github.nicolasfara.rstmanager.warehouse.service.ProductApp.StoredProduct
import io.github.nicolasfara.rstmanager.warehouse.service.ProductHttpApi.{ MpnDto, ProductRequest, ProductResponse, ProductSupplierDto, StockDto }
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierHttpApi.{ SupplierRequest, SupplierResponse }

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class WarehouseHttpDtosTest extends AnyFlatSpecLike:
  private val id: UUID = UUID.fromString("00000000-0000-0000-0000-000000000401").nn
  private val firstSupplier: UUID = UUID.fromString("00000000-0000-0000-0000-000000000301").nn
  private val secondSupplier: UUID = UUID.fromString("00000000-0000-0000-0000-000000000302").nn

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)
  private def errors[A](value: ValidatedNec[String, A]): List[String] = value.fold(_.toChain.toList, result => fail(s"Unexpectedly valid: $result"))

  private val request: ProductRequest =
    ProductRequest(
      " tes-001 ",
      "Tessuto acrilico",
      Some(" "),
      None,
      "linear_meters",
      "18.5",
      List(MpnDto(" ac-152 ", firstSupplier, None), MpnDto("AC-152", secondSupplier, Some("Canvas"))),
    )

  "ProductRequest.toDomain" should "parse the unit, the decimal price, and normalize the codes" in:
    val info = valid(request.toDomain(id))
    info.id shouldEqual ProductId(id)
    (info.internalCode: String) shouldEqual "TES-001"
    info.productType shouldEqual None
    info.unit shouldEqual UnitOfMeasure.LinearMeters
    info.customerPrice.value shouldEqual 185000L
    info.mpns.map(mpn => mpn.code: String) shouldEqual List("AC-152", "AC-152")
    info.suppliers shouldEqual List(SupplierId(firstSupplier), SupplierId(secondSupplier))

  it should "report every malformed field at once" in:
    val bad = request.copy(unit = "litres", customerPrice = "18.50001", mpns = List(MpnDto(" ", firstSupplier, None)))
    val reported = errors(bad.toDomain(id))
    reported should have size 3
    reported.exists(_.startsWith("Unità di misura:")) shouldEqual true
    reported.exists(_.startsWith("Prezzo cliente:")) shouldEqual true
    reported should contain("MPN riga 1: Il codice MPN non può essere vuoto")

  it should "reject the same MPN code twice for the same supplier, once normalized" in:
    val duplicated = request.copy(mpns = List(MpnDto("x-1", firstSupplier, None), MpnDto(" X-1", firstSupplier, None)))
    errors(duplicated.toDomain(id)) shouldEqual List("MPN: il codice MPN X-1 compare più volte per lo stesso fornitore")

  "ProductResponse" should "carry the price as a 4-decimal string, the unit wire name, and the derived suppliers" in:
    val info = valid(request.toDomain(id))
    val supplier = valid(Supplier.createSupplier(SupplierId(firstSupplier), "Tessuti Nautici", None, None, None, None, None))
    val created = LocalDate.parse("2026-06-15")
    val response = ProductResponse.fromDomain(
      StoredProduct(ProductRecord.created(info, created), Some(LocalDate.parse("2026-07-01"))),
      Map(SupplierId(firstSupplier) -> StoredSupplier(supplier, deleted = true)),
      LocalDate.parse("2026-07-02"),
    )
    response.customerPrice shouldEqual "18.5000"
    response.unit shouldEqual "linear_meters"
    response.createdOn shouldEqual "2026-06-15"
    response.archived shouldEqual true
    response.archivedOn shouldEqual Some("2026-07-01")
    response.suppliers shouldEqual List(
      ProductSupplierDto(firstSupplier, "Tessuti Nautici", active = false),
      ProductSupplierDto(secondSupplier, secondSupplier.toString, active = false),
    )
    response.asJson.hcursor.downField("customerPrice").focus shouldEqual Some(Json.fromString("18.5000"))
    UnitPrice.fromDecimalString(response.customerPrice).map(_.value) shouldEqual Right(info.customerPrice.value)
    response.stock shouldEqual StockDto("0.000", None, None, "0.00", negative = false)

  "ProductHttpApi.suppliersToCheck" should "require active suppliers only for MPNs new to the stored product" in:
    val stored = valid(request.toDomain(id))
    val third = UUID.fromString("00000000-0000-0000-0000-000000000303").nn
    val edited = valid(request.copy(mpns = request.mpns :+ MpnDto("NEW-1", third, None) :+ MpnDto("AC-999", firstSupplier, None)).toDomain(id))
    ProductHttpApi.suppliersToCheck(None, stored) shouldEqual List(SupplierId(firstSupplier), SupplierId(secondSupplier))
    ProductHttpApi.suppliersToCheck(Some(stored), stored) shouldEqual Nil
    ProductHttpApi.suppliersToCheck(Some(stored), edited) shouldEqual List(SupplierId(third), SupplierId(firstSupplier))

  "SupplierRequest.toDomain" should "normalize the supplier and render it back" in:
    val supplier = valid(SupplierRequest(" Ferramenta ", Some("it 123"), Some("info@ferramenta.it"), None, None, Some("")).toDomain(firstSupplier))
    SupplierResponse
      .fromDomain(supplier) shouldEqual SupplierResponse(firstSupplier, "Ferramenta", Some("IT123"), Some("info@ferramenta.it"), None, None, None)
end WarehouseHttpDtosTest
