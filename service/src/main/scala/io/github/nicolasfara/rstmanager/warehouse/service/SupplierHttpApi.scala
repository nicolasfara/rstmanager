package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.{ Locale, UUID }

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.*

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** JSON DTOs, tapir endpoints, and http4s server logic for the supplier CRUD API under `/warehouse/suppliers`. Error messages are in Italian. */
object SupplierHttpApi:
  import ApiError.ApiFailure
  import WarehouseHttpSupport.{ conflict, internal, notFound, validation }

  final case class SupplierRequest(
      name: String,
      vatNumber: Option[String],
      email: Option[String],
      phone: Option[String],
      address: Option[String],
      notes: Option[String],
  ):
    def toDomain(id: UUID): ValidatedNec[String, Supplier] =
      Supplier.createSupplier(SupplierId(id), name, vatNumber, email, phone, address, notes)

  object SupplierRequest:
    val example: SupplierRequest =
      SupplierRequest(
        "Tessuti Nautici Srl",
        Some("IT01234567890"),
        Some("ordini@tessutinautici.it"),
        Some("+39 0541 123456"),
        Some("Via del Porto 3, 47921 Rimini"),
        Some("Consegna in 5 giorni lavorativi"),
      )

  final case class SupplierResponse(
      id: UUID,
      name: String,
      vatNumber: Option[String],
      email: Option[String],
      phone: Option[String],
      address: Option[String],
      notes: Option[String],
  )

  object SupplierResponse:
    def fromDomain(supplier: Supplier): SupplierResponse =
      SupplierResponse(
        supplier.id.value,
        supplier.name,
        supplier.vatNumber,
        supplier.email.map(email => email: String),
        supplier.phone,
        supplier.address,
        supplier.notes,
      )

  private def supplierNotFound(id: UUID): ApiFailure = notFound("Il fornitore richiesto non esiste.", List(id.toString))

  private def failure(error: SupplierError, id: UUID): ApiFailure = error match
    case SupplierError.SupplierAlreadyExists => conflict("supplier-already-exists", "Esiste già un fornitore con questo id.", Nil)
    case SupplierError.SupplierNotFound => supplierNotFound(id)
    case SupplierError.SupplierIdMismatch =>
      conflict("supplier-id-mismatch", "L'id del fornitore non corrisponde al fornitore indicato nel percorso.", Nil)

  given Codec[SupplierRequest] = deriveCodec
  given Codec[SupplierResponse] = deriveCodec
  given Schema[SupplierRequest] = Schema.derived
  given Schema[SupplierResponse] = Schema.derived

  private val collection = "warehouse" / "suppliers"

  val create: Secured.SecuredEndpoint[SupplierRequest, SupplierResponse] =
    Secured.base.post
      .in(collection)
      .tag("Warehouse")
      .summary("Create a supplier")
      .in(jsonBody[SupplierRequest].example(SupplierRequest.example))
      .out(jsonBody[SupplierResponse])

  val list: Secured.SecuredEndpoint[Unit, List[SupplierResponse]] =
    Secured.base.get.in(collection).tag("Warehouse").summary("List the active suppliers").out(jsonBody[List[SupplierResponse]])

  val read: Secured.SecuredEndpoint[UUID, SupplierResponse] =
    Secured.base.get.in(collection / path[UUID]("id")).tag("Warehouse").summary("Read an active supplier").out(jsonBody[SupplierResponse])

  val update: Secured.SecuredEndpoint[(UUID, SupplierRequest), SupplierResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Replace a supplier")
      .in(jsonBody[SupplierRequest].example(SupplierRequest.example))
      .out(jsonBody[SupplierResponse])

  val delete: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Delete a supplier no product references")
      .out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, delete)

  def routes(store: SupplierApp.Store, products: ProductApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Admin)(createLogic(store)),
    security.secure(list, Role.Viewer)(_ => listLogic(store)),
    security.secure(read, Role.Viewer)(readLogic(store)),
    security.secure(update, Role.Admin)(updateLogic(store)),
    security.secure(delete, Role.Admin)(deleteLogic(store, products)),
  )

  private def createLogic(store: SupplierApp.Store)(request: SupplierRequest): IO[Either[ApiFailure, SupplierResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      request.toDomain(id).toEither match
        case Left(errors) => IO.pure(validation(errors).asLeft)
        case Right(supplier) =>
          SupplierApp.create(store, supplier).attempt.map {
            case Left(error) => internal(error).asLeft
            case Right(Left(error)) => failure(error, id).asLeft
            case Right(Right(())) => SupplierResponse.fromDomain(supplier).asRight
          }
    }

  private def listLogic(store: SupplierApp.Store): IO[Either[ApiFailure, List[SupplierResponse]]] =
    SupplierApp
      .list(store)
      .attempt
      .map(_.bimap(internal, _.sortBy(supplier => (supplier.name: String).toLowerCase(Locale.ROOT).nn).map(SupplierResponse.fromDomain)))

  private def readLogic(store: SupplierApp.Store)(id: UUID): IO[Either[ApiFailure, SupplierResponse]] =
    SupplierApp.get(store, SupplierId(id)).attempt.map {
      case Left(error) => internal(error).asLeft
      case Right(Some(supplier)) => SupplierResponse.fromDomain(supplier).asRight
      case Right(None) => supplierNotFound(id).asLeft
    }

  private def updateLogic(store: SupplierApp.Store)(id: UUID, request: SupplierRequest): IO[Either[ApiFailure, SupplierResponse]] =
    request.toDomain(id).toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right(supplier) =>
        SupplierApp.update(store, supplier).attempt.map {
          case Left(error) => internal(error).asLeft
          case Right(Left(error)) => failure(error, id).asLeft
          case Right(Right(())) => SupplierResponse.fromDomain(supplier).asRight
        }

  /** A supplier referenced by any product, archived ones included, cannot be deleted: the product would lose the supplier of its MPNs. */
  private def deleteLogic(store: SupplierApp.Store, products: ProductApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    ProductApp
      .referencingSupplier(products, SupplierId(id))
      .flatMap {
        case usedBy if usedBy.nonEmpty =>
          val details = usedBy.map(product => s"${product.info.internalCode} - ${product.info.description}").sorted
          IO.pure(conflict("supplier-in-use", "Il fornitore è usato da uno o più prodotti di magazzino.", details).asLeft)
        case _ => SupplierApp.delete(store, SupplierId(id)).map(_.leftMap(failure(_, id)))
      }
      .handleError(error => internal(error).asLeft)
end SupplierHttpApi
