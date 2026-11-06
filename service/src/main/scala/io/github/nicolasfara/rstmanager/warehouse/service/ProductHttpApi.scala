package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.{ UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId
import io.github.nicolasfara.rstmanager.warehouse.service.ProductApp.StoredProduct
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/**
 * JSON DTOs, tapir endpoints, and http4s server logic for the warehouse product API under `/warehouse/products`.
 *
 * Prices travel as decimal strings with exactly 4 decimals (`"3.7000"`), quantities with 3 (`"2.500"`), amounts with 2 (`"12.34"`), units as their
 * wire name (`"linear_meters"`), days as `YYYY-MM-DD`. Error messages are in Italian.
 */
object ProductHttpApi:
  import ApiError.ApiFailure
  import WarehouseHttpSupport.*

  /** A manufacturer part number: the same shape in requests and responses. */
  final case class MpnDto(code: String, supplierId: UUID, description: Option[String]):
    /** The MPN, each problem prefixed with `label` (the row of the MPN editor, `MPN riga 2`). */
    def toDomain(label: String): ValidatedNec[String, Mpn] =
      Mpn.createMpn(code, SupplierId(supplierId), description).leftMap(_.map(error => s"$label: $error"))

  object MpnDto:
    def fromDomain(mpn: Mpn): MpnDto = MpnDto(mpn.code, mpn.supplierId.value, mpn.description)

  final case class ProductRequest(
      internalCode: String,
      description: String,
      productType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: String,
      mpns: List[MpnDto],
  ):
    /** Validates the request into product data, including the no-duplicate-MPN rule, so every problem is reported at once. */
    def toDomain(id: UUID): ValidatedNec[String, ProductInfo] =
      (
        UnitOfMeasure.fromWireName(unit).leftMap(error => s"Unità di misura: $error").toValidatedNec,
        UnitPrice.fromDecimalString(customerPrice).leftMap(error => s"Prezzo cliente: $error").toValidatedNec,
        mpns.zipWithIndex.traverse((dto, index) => dto.toDomain(s"MPN riga ${index + 1}")),
      ).tupled
        .andThen((parsedUnit, price, parsedMpns) =>
          ProductInfo.createProductInfo(ProductId(id), internalCode, description, productType, notes, parsedUnit, price, parsedMpns),
        )
        .andThen(_.checkInvariants.leftMap(_.map(describeInvariant)))
  end ProductRequest

  object ProductRequest:
    val example: ProductRequest =
      ProductRequest(
        "TES-001",
        "Tessuto acrilico blu navy, altezza 152 cm",
        Some("Tessuti"),
        Some("Per tendalini e capottine"),
        UnitOfMeasure.LinearMeters.wireName,
        "18.5000",
        List(MpnDto("AC-152-NAVY", UUID.fromString("00000000-0000-0000-0000-000000000301").nn, Some("Acrylic canvas 152 cm navy"))),
      )

  /** A supplier of the product, derived from its MPNs; `active` is false for a supplier deleted after the MPN was added. */
  final case class ProductSupplierDto(id: UUID, name: String, active: Boolean)

  object ProductSupplierDto:
    /** The supplier `id` named from `suppliers`; a supplier missing from it is shown by id, as inactive. */
    def of(id: SupplierId, suppliers: Map[SupplierId, StoredSupplier]): ProductSupplierDto =
      suppliers
        .get(id)
        .fold(ProductSupplierDto(id.value, id.value.toString, active = false))(stored =>
          ProductSupplierDto(id.value, stored.supplier.name, !stored.deleted),
        )

  /**
   * Stock of a product on a day: signed quantity (negative only as an anomaly), highest unit cost recorded so far, value (`quantity × maxUnitCost`),
   * total spent on purchases so far.
   */
  final case class StockDto(quantity: String, maxUnitCost: Option[String], value: Option[String], purchasedTotal: String, negative: Boolean)

  object StockDto:
    def fromDomain(stock: StockSnapshot): StockDto =
      StockDto(
        stock.quantity.toDecimalString,
        stock.maxUnitCost.map(_.toDecimalString),
        stock.value.map(_.toDecimalString),
        stock.purchasedTotal.toDecimalString,
        stock.negative,
      )

  final case class ProductResponse(
      id: UUID,
      internalCode: String,
      description: String,
      productType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: String,
      mpns: List[MpnDto],
      suppliers: List[ProductSupplierDto],
      createdOn: String,
      archived: Boolean,
      archivedOn: Option[String],
      stock: StockDto,
  )

  object ProductResponse:
    /**
     * The response for `product`, naming its suppliers from `suppliers` (a supplier missing from it is shown by id, as inactive), with its stock at
     * the end of `today`.
     */
    def fromDomain(product: StoredProduct, suppliers: Map[SupplierId, StoredSupplier], today: LocalDate): ProductResponse =
      val info = product.info
      ProductResponse(
        info.id.value,
        info.internalCode,
        info.description,
        info.productType,
        info.notes,
        info.unit.wireName,
        info.customerPrice.toDecimalString,
        info.mpns.map(MpnDto.fromDomain),
        info.suppliers.map(ProductSupplierDto.of(_, suppliers)),
        formatDay(product.product.createdOn),
        product.archived,
        product.archivedOn.map(formatDay),
        StockDto.fromDomain(product.product.ledger.snapshotAt(today)),
      )
  end ProductResponse

  private def describeInvariant(error: ProductError): String = error match
    case ProductError.DuplicateMpn(_, code) => duplicateMpnMessage(code)
    case other => other.toString

  given Codec[MpnDto] = deriveCodec
  given Codec[ProductRequest] = deriveCodec
  given Codec[ProductSupplierDto] = deriveCodec
  given Codec[StockDto] = deriveCodec
  given Codec[ProductResponse] = deriveCodec
  given Schema[MpnDto] = Schema.derived
  given Schema[ProductRequest] = Schema.derived
  given Schema[ProductSupplierDto] = Schema.derived
  given Schema[StockDto] = Schema.derived
  given Schema[ProductResponse] = Schema.derived

  private val collection = "warehouse" / "products"

  val create: Secured.SecuredEndpoint[ProductRequest, ProductResponse] =
    Secured.base.post
      .in(collection)
      .tag("Warehouse")
      .summary("Create a product")
      .in(jsonBody[ProductRequest].example(ProductRequest.example))
      .out(jsonBody[ProductResponse])

  val list: Secured.SecuredEndpoint[Option[Boolean], List[ProductResponse]] =
    Secured.base.get
      .in(collection)
      .in(query[Option[Boolean]]("includeArchived").description("Also list archived products (default false)."))
      .tag("Warehouse")
      .summary("List products")
      .out(jsonBody[List[ProductResponse]])

  val read: Secured.SecuredEndpoint[UUID, ProductResponse] =
    Secured.base.get
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Read a product, active or archived")
      .out(jsonBody[ProductResponse])

  val update: Secured.SecuredEndpoint[(UUID, ProductRequest), ProductResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Replace an active product")
      .in(jsonBody[ProductRequest].example(ProductRequest.example))
      .out(jsonBody[ProductResponse])

  val archive: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Archive a product (it stays readable for the history)")
      .out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, archive)

  def routes(store: ProductApp.Store, suppliers: SupplierApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Admin)(createLogic(store, suppliers)),
    security.secure(list, Role.Viewer)(listLogic(store, suppliers)),
    security.secure(read, Role.Viewer)(readLogic(store, suppliers)),
    security.secure(update, Role.Admin)(updateLogic(store, suppliers)),
    security.secure(archive, Role.Admin)(archiveLogic(store)),
  )

  private def createLogic(store: ProductApp.Store, suppliers: SupplierApp.Store)(request: ProductRequest): IO[Either[ApiFailure, ProductResponse]] =
    IO(UUID.randomUUID().nn).flatMap(id => save(store, suppliers, id, request, ProductApp.create))

  private def updateLogic(store: ProductApp.Store, suppliers: SupplierApp.Store)(
      id: UUID,
      request: ProductRequest,
  ): IO[Either[ApiFailure, ProductResponse]] =
    save(store, suppliers, id, request, ProductApp.update)

  /** Validates the request, checks it against the suppliers and the other products, then sends it with `command` and returns the stored product. */
  private def save(
      store: ProductApp.Store,
      suppliers: SupplierApp.Store,
      id: UUID,
      request: ProductRequest,
      command: (ProductApp.Store, ProductInfo) => IO[Either[ProductError, Unit]],
  ): IO[Either[ApiFailure, ProductResponse]] =
    request.toDomain(id).toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right(info) =>
        val saved = for
          problem <- checkReferences(store, suppliers, info)
          result <- problem.fold(command(store, info).map(_.leftMap(productFailure(_, id, Some(info)))))(failure => IO.pure(failure.asLeft))
          response <- result.flatTraverse(_ => readLogic(store, suppliers)(id))
        yield response
        saved.handleError(error => internal(error).asLeft)

  /**
   * The cross-aggregate rules of a product: the supplier of every MPN new to the product exists and is active (an MPN the stored product already has
   * keeps its supplier even after the supplier is deleted), and no other active product has the same internal code or one of its (supplier, MPN code)
   * pairs. Archived products release their code and MPNs.
   */
  private def checkReferences(store: ProductApp.Store, suppliers: SupplierApp.Store, info: ProductInfo): IO[Option[ApiFailure]] =
    for
      stored <- ProductApp.find(store, info.id)
      unavailable <- suppliersToCheck(stored.map(_.info), info).traverseFilter(supplierId =>
        SupplierApp.getAny(suppliers, supplierId).map {
          case Some(StoredSupplier(_, false)) => None
          case Some(StoredSupplier(supplier, true)) => Some(s"${supplier.name} (eliminato)")
          case None => Some(supplierId.value.toString)
        },
      )
      others <- ProductApp.list(store).map(_.filter(_.info.id != info.id))
    yield
      val codeOwners = others.filter(other => (other.info.internalCode: String) == (info.internalCode: String))
      val takenMpns =
        for
          mpn <- info.mpns
          other <- others
          if other.info.mpns.exists(owned => owned.supplierId == mpn.supplierId && (owned.code: String) == (mpn.code: String))
        yield s"${mpn.code} -> ${other.info.internalCode}"
      if unavailable.nonEmpty then Some(notFound("Uno o più fornitori degli MPN non esistono o sono stati eliminati.", unavailable))
      else if codeOwners.nonEmpty then
        Some(
          conflict(
            "product-code-taken",
            s"Il codice ${info.internalCode} è già usato da un altro prodotto.",
            codeOwners.map(other => s"${other.info.internalCode} - ${other.info.description}"),
          ),
        )
      else if takenMpns.nonEmpty then Some(conflict("mpn-taken", "Uno o più MPN sono già usati da un altro prodotto.", takenMpns))
      else None

  /**
   * The suppliers that must be active for `info` to be saved over `stored`: those of the (supplier, MPN code) pairs `stored` does not already have,
   * so that deleting a supplier never blocks editing a product that already listed it.
   */
  def suppliersToCheck(stored: Option[ProductInfo], info: ProductInfo): List[SupplierId] =
    val storedMpns = stored.toList.flatMap(_.mpns)
    info.mpns
      .filterNot(mpn => storedMpns.exists(owned => owned.supplierId == mpn.supplierId && (owned.code: String) == (mpn.code: String)))
      .map(_.supplierId)
      .distinct

  private def listLogic(store: ProductApp.Store, suppliers: SupplierApp.Store)(
      includeArchived: Option[Boolean],
  ): IO[Either[ApiFailure, List[ProductResponse]]] =
    ProductApp
      .listAll(store)
      .map(_.filter(product => includeArchived.getOrElse(false) || !product.archived).sortBy(product => product.info.internalCode: String))
      .flatMap(products => hydrate(suppliers, products))
      .attempt
      .map(_.leftMap(internal))

  private[service] def readLogic(store: ProductApp.Store, suppliers: SupplierApp.Store)(id: UUID): IO[Either[ApiFailure, ProductResponse]] =
    ProductApp
      .find(store, ProductId(id))
      .flatMap {
        case Some(product) => hydrate(suppliers, List(product)).map(_.headOption.toRight(productNotFound(id)))
        case None => IO.pure(productNotFound(id).asLeft)
      }
      .handleError(error => internal(error).asLeft)

  private def archiveLogic(store: ProductApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    ProductApp
      .find(store, ProductId(id))
      .flatMap {
        case None => IO.pure(productNotFound(id).asLeft)
        case Some(product) => ProductApp.archive(store, ProductId(id)).map(_.leftMap(productFailure(_, id, Some(product.info))))
      }
      .handleError(error => internal(error).asLeft)

  /** The suppliers referenced by `ids`, active or deleted, each read once. */
  private[service] def suppliersById(suppliers: SupplierApp.Store, ids: List[SupplierId]): IO[Map[SupplierId, StoredSupplier]] =
    ids.distinct.traverseFilter(id => SupplierApp.getAny(suppliers, id).map(_.map(id -> _))).map(_.toMap)

  /** Responses for `products`, with their stock today, reading each referenced supplier (active or deleted) once. */
  private def hydrate(suppliers: SupplierApp.Store, products: List[StoredProduct]): IO[List[ProductResponse]] =
    for
      day <- today
      found <- suppliersById(suppliers, products.flatMap(_.info.suppliers))
    yield products.map(ProductResponse.fromDomain(_, found, day))
end ProductHttpApi
