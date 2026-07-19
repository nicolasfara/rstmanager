package io.github.nicolasfara.rstmanager.customer.service

import java.util.{ Locale, UUID }

import io.github.nicolasfara.rstmanager.customer.domain.*
import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/** JSON DTOs, tapir endpoints, and http4s server logic for the customer CRUD API. */
object CustomerHttpApi:
  import ApiError.ApiFailure

  final case class CustomerRequest(
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      postalCode: String,
      country: String,
      fiscalCode: String,
      customerType: String,
      businessName: Option[String],
      pec: Option[String],
      notes: Option[String],
      boatModel: Option[String],
      boatName: Option[String],
      boatBerth: Option[String],
      port: Option[String],
  ):
    def toDomain(id: UUID): ValidatedNec[String, Customer] =
      customerTypeToDomain(customerType).andThen { parsedType =>
        Customer.createCustomer(
          id,
          name,
          surname,
          email,
          phone,
          street,
          city,
          postalCode,
          country,
          fiscalCode,
          parsedType,
          businessName,
          pec,
          notes,
          boatModel,
          boatName,
          boatBerth,
          port,
        )
      }

  object CustomerRequest:
    val example: CustomerRequest =
      CustomerRequest(
        "Giulia",
        "Bianchi",
        "giulia.bianchi@example.com",
        "+393331234567",
        "Via Roma 10",
        "Bologna",
        "40121",
        "IT",
        "RSSMRA85M01H501Z",
        "individual",
        None,
        Some("giulia.bianchi@pec.example.com"),
        Some("Cliente storico"),
        Some("Sun Odyssey 410"),
        Some("Aurora"),
        Some("B12"),
        Some("Marina di Rimini"),
      )

  final case class CustomerResponse(
      id: UUID,
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      postalCode: String,
      country: String,
      fiscalCode: String,
      customerType: String,
      businessName: Option[String],
      pec: Option[String],
      notes: Option[String],
      boatModel: Option[String],
      boatName: Option[String],
      boatBerth: Option[String],
      port: Option[String],
  )

  object CustomerResponse:
    def fromDomain(customer: Customer): CustomerResponse =
      CustomerResponse(
        customer.id,
        customer.contactInfo.name,
        customer.contactInfo.surname,
        customer.contactInfo.email,
        customer.contactInfo.phone,
        customer.address.street,
        customer.address.city,
        customer.address.postalCode,
        customer.address.country,
        customer.fiscalCode,
        customer.customerType.toString.toLowerCase(Locale.ROOT).nn,
        customer.businessName,
        customer.pec,
        customer.notes,
        customer.boat.model,
        customer.boat.name,
        customer.boat.berth,
        customer.boat.port,
      )

  private def customerTypeToDomain(value: String): ValidatedNec[String, CustomerType] =
    val normalized: String = value.trim.nn.toLowerCase(Locale.ROOT).nn
    normalized match
      case "individual" => CustomerType.Individual.validNec
      case "company" => CustomerType.Company.validNec
      case other => s"customerType '$other' is not supported. Use individual or company.".invalidNec

  private def conflict(error: CustomerError): ApiFailure = error match
    case CustomerError.CustomerAlreadyExists => ApiError.conflict("customer-already-exists", "A customer with this id already exists.")
    case CustomerError.CustomerNotFound => ApiError.notFound("Customer", "")

  given Codec[CustomerRequest] = deriveCodec
  given Codec[CustomerResponse] = deriveCodec
  given Schema[CustomerRequest] = Schema.derived
  given Schema[CustomerResponse] = Schema.derived

  private val collection = "customers"

  val create: Secured.SecuredEndpoint[CustomerRequest, CustomerResponse] =
    Secured.base.post
      .in(collection)
      .tag("Customers")
      .summary("Create a customer")
      .in(jsonBody[CustomerRequest].example(CustomerRequest.example))
      .out(jsonBody[CustomerResponse])

  val list: Secured.SecuredEndpoint[Unit, List[CustomerResponse]] =
    Secured.base.get.in(collection).tag("Customers").summary("List customers").out(jsonBody[List[CustomerResponse]])

  val read: Secured.SecuredEndpoint[UUID, CustomerResponse] =
    Secured.base.get.in(collection / path[UUID]("id")).tag("Customers").summary("Read a customer").out(jsonBody[CustomerResponse])

  val update: Secured.SecuredEndpoint[(UUID, CustomerRequest), CustomerResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Customers")
      .summary("Replace a customer")
      .in(jsonBody[CustomerRequest].example(CustomerRequest.example))
      .out(jsonBody[CustomerResponse])

  val delete: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete.in(collection / path[UUID]("id")).tag("Customers").summary("Delete a customer").out(statusCode(StatusCode.NoContent))

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, delete)

  def routes(store: CustomerApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Admin)(createLogic(store)),
    security.secure(list, Role.Viewer)(_ => listLogic(store)),
    security.secure(read, Role.Viewer)(readLogic(store)),
    security.secure(update, Role.Admin)(updateLogic(store)),
    security.secure(delete, Role.Admin)(deleteLogic(store)),
  )

  private def createLogic(store: CustomerApp.Store)(request: CustomerRequest): IO[Either[ApiFailure, CustomerResponse]] =
    IO(UUID.randomUUID().nn).flatMap { id =>
      request.toDomain(id).toEither match
        case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
        case Right(customer) =>
          CustomerApp.create(store, customer).attempt.map {
            case Left(error) => ApiError.internal(error).asLeft
            case Right(Left(error)) => conflict(error).asLeft
            case Right(Right(())) => CustomerResponse.fromDomain(customer).asRight
          }
    }

  private def listLogic(store: CustomerApp.Store): IO[Either[ApiFailure, List[CustomerResponse]]] =
    CustomerApp.list(store).attempt.map(_.bimap(ApiError.internal, _.map(CustomerResponse.fromDomain)))

  private def readLogic(store: CustomerApp.Store)(id: UUID): IO[Either[ApiFailure, CustomerResponse]] =
    CustomerApp.get(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Some(customer)) => CustomerResponse.fromDomain(customer).asRight
      case Right(None) => ApiError.notFound("Customer", id.toString).asLeft
    }

  private def updateLogic(store: CustomerApp.Store)(id: UUID, request: CustomerRequest): IO[Either[ApiFailure, CustomerResponse]] =
    request.toDomain(id).toEither match
      case Left(errors) => IO.pure(ApiError.validation(errors).asLeft)
      case Right(customer) =>
        CustomerApp.update(store, customer).attempt.map {
          case Left(error) => ApiError.internal(error).asLeft
          case Right(Left(CustomerError.CustomerNotFound)) => ApiError.notFound("Customer", id.toString).asLeft
          case Right(Left(error)) => conflict(error).asLeft
          case Right(Right(())) => CustomerResponse.fromDomain(customer).asRight
        }

  private def deleteLogic(store: CustomerApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    CustomerApp.delete(store, id).attempt.map {
      case Left(error) => ApiError.internal(error).asLeft
      case Right(Left(CustomerError.CustomerNotFound)) => ApiError.notFound("Customer", id.toString).asLeft
      case Right(Left(error)) => conflict(error).asLeft
      case Right(Right(())) => ().asRight
    }
end CustomerHttpApi
