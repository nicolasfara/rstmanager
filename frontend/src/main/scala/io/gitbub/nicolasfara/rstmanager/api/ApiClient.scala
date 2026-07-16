package io.gitbub.nicolasfara.rstmanager.api

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*

import io.circe.{ Decoder, Encoder }
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom

import Dtos.*

/**
 * Thin typed wrapper over the Fetch API for the RST Manager REST endpoints.
 *
 * Every call returns a `Future[Either[ApiError, A]]`: a `Left` carries the parsed [[Dtos.ApiError]] body (or a synthetic one for transport/decoding
 * failures), a `Right` the decoded payload.
 */
object ApiClient:
  type Result[A] = Either[ApiError, A]

  private val baseUrl = "/api/v1"

  // ---- Low-level ---------------------------------------------------------------------------------

  private def rawSend(method: dom.HttpMethod, path: String, body: Option[String]): Future[(Int, String)] =
    val init = new dom.RequestInit {}
    init.method = method
    body.foreach { payload =>
      val headers = new dom.Headers()
      headers.append("Content-Type", "application/json")
      init.headers = headers
      init.body = payload
    }
    val responseF: Future[dom.Response] = dom.fetch(baseUrl + path, init)
    responseF.flatMap { response =>
      val textF: Future[String] = response.text()
      textF.map(text => (response.status, text))
    }

  private def parseError(status: Int, text: String): ApiError =
    decode[ApiError](text).getOrElse(ApiError("http-error", s"HTTP $status", if text.isEmpty then Nil else List(text)))

  private def isSuccess(status: Int): Boolean = status >= 200 && status < 300

  private def errorMessage(err: Throwable): String = err.getMessage match
    case message: String => message
    case _ => "Errore di rete"

  private def sendJson[A: Decoder](method: dom.HttpMethod, path: String, body: Option[String]): Future[Result[A]] =
    rawSend(method, path, body).map { (status, text) =>
      if isSuccess(status) then decode[A](text).left.map(err => ApiError("decode-error", s"Cannot decode response: ${err.getMessage}", List(text)))
      else Left(parseError(status, text))
    }.recover { case err => Left(ApiError("network-error", errorMessage(err), Nil)) }

  private def sendUnit(method: dom.HttpMethod, path: String, body: Option[String]): Future[Result[Unit]] =
    rawSend(method, path, body).map { (status, text) =>
      if isSuccess(status) then Right(()) else Left(parseError(status, text))
    }.recover { case err => Left(ApiError("network-error", errorMessage(err), Nil)) }

  private def jsonBody[A: Encoder](value: A): Option[String] = Some(value.asJson.noSpaces)

  // ---- Employees ---------------------------------------------------------------------------------

  def listEmployees(): Future[Result[List[EmployeeResponse]]] = sendJson(dom.HttpMethod.GET, "/employees", None)
  def createEmployee(request: EmployeeRequest): Future[Result[EmployeeResponse]] =
    sendJson(dom.HttpMethod.POST, "/employees", jsonBody(request))
  def updateEmployee(id: UUID, request: EmployeeRequest): Future[Result[EmployeeResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/employees/$id", jsonBody(request))
  def deleteEmployee(id: UUID): Future[Result[Unit]] = sendUnit(dom.HttpMethod.DELETE, s"/employees/$id", None)

  // ---- Customers ---------------------------------------------------------------------------------

  def listCustomers(): Future[Result[List[CustomerResponse]]] = sendJson(dom.HttpMethod.GET, "/customers", None)
  def createCustomer(request: CustomerRequest): Future[Result[CustomerResponse]] =
    sendJson(dom.HttpMethod.POST, "/customers", jsonBody(request))
  def updateCustomer(id: UUID, request: CustomerRequest): Future[Result[CustomerResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/customers/$id", jsonBody(request))
  def deleteCustomer(id: UUID): Future[Result[Unit]] = sendUnit(dom.HttpMethod.DELETE, s"/customers/$id", None)

  // ---- Tasks (catalog) ---------------------------------------------------------------------------

  def listTasks(): Future[Result[List[TaskResponse]]] = sendJson(dom.HttpMethod.GET, "/tasks", None)
  def createTask(request: TaskRequest): Future[Result[TaskResponse]] =
    sendJson(dom.HttpMethod.POST, "/tasks", jsonBody(request))
  def updateTask(id: UUID, request: TaskRequest): Future[Result[TaskResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/tasks/$id", jsonBody(request))
  def deleteTask(id: UUID): Future[Result[Unit]] = sendUnit(dom.HttpMethod.DELETE, s"/tasks/$id", None)

  // ---- Manufacturings (catalog) ------------------------------------------------------------------

  def listManufacturingCatalog(): Future[Result[List[ManufacturingCatalogResponse]]] = sendJson(dom.HttpMethod.GET, "/manufacturings", None)
  def readManufacturingCatalog(id: UUID): Future[Result[ManufacturingCatalogResponse]] = sendJson(dom.HttpMethod.GET, s"/manufacturings/$id", None)
  def createManufacturingCatalog(request: ManufacturingCatalogRequest): Future[Result[ManufacturingCatalogResponse]] =
    sendJson(dom.HttpMethod.POST, "/manufacturings", jsonBody(request))
  def updateManufacturingCatalog(id: UUID, request: ManufacturingCatalogRequest): Future[Result[ManufacturingCatalogResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/manufacturings/$id", jsonBody(request))
  def deleteManufacturingCatalog(id: UUID): Future[Result[Unit]] = sendUnit(dom.HttpMethod.DELETE, s"/manufacturings/$id", None)

  // ---- Orders ------------------------------------------------------------------------------------

  def listOrders(): Future[Result[List[OrderResponse]]] = sendJson(dom.HttpMethod.GET, "/orders", None)
  def createOrder(request: OrderRequest): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.POST, "/orders", jsonBody(request))
  def updateOrder(id: UUID, request: OrderUpdateRequest): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/orders/$id", jsonBody(request))
  def orderTransition(id: UUID, request: TransitionRequest): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.POST, s"/orders/$id/transitions", jsonBody(request))
  def updateScheduledTask(
      orderId: UUID,
      manufacturingId: UUID,
      taskId: UUID,
      request: TaskProgressUpdateRequest,
  ): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/orders/$orderId/manufacturings/$manufacturingId/tasks/$taskId", jsonBody(request))
  def addManufacturing(orderId: UUID, request: ManufacturingDto): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.POST, s"/orders/$orderId/manufacturings", jsonBody(request))
  def removeManufacturing(orderId: UUID, manufacturingId: UUID): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.DELETE, s"/orders/$orderId/manufacturings/$manufacturingId", None)
  def updateManufacturing(orderId: UUID, manufacturingId: UUID, request: ManufacturingUpdateRequest): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/orders/$orderId/manufacturings/$manufacturingId", jsonBody(request))
  def updateOrderDependencies(orderId: UUID, request: OrderDependenciesUpdateRequest): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/orders/$orderId/dependencies", jsonBody(request))
  def updateTaskDependencies(orderId: UUID, manufacturingId: UUID, request: TaskDependenciesUpdateRequest): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/orders/$orderId/manufacturings/$manufacturingId/dependencies", jsonBody(request))
  def addManufacturingTask(orderId: UUID, manufacturingId: UUID, request: AddTaskRequest): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.POST, s"/orders/$orderId/manufacturings/$manufacturingId/tasks", jsonBody(request))
  def removeManufacturingTask(orderId: UUID, manufacturingId: UUID, taskId: UUID): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.DELETE, s"/orders/$orderId/manufacturings/$manufacturingId/tasks/$taskId", None)
  def setPreferredEmployee(orderId: UUID, manufacturingId: UUID, employeeId: Option[UUID]): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/orders/$orderId/manufacturings/$manufacturingId/employee", jsonBody(SetPreferredEmployeeRequest(employeeId)))
  def deleteOrder(id: UUID, reason: Option[String]): Future[Result[Unit]] =
    val query = reason.filter(_.nonEmpty).fold("")(r => s"?reason=${js.URIUtils.encodeURIComponent(r)}")
    sendUnit(dom.HttpMethod.DELETE, s"/orders/$id$query", None)

  // ---- Planning ----------------------------------------------------------------------------------

  def currentPlanning(): Future[Result[PlanningStateDto]] = sendJson(dom.HttpMethod.GET, "/planning/attempts/current", None)
  def createPlanningAttempt(request: PlanningAttemptRequest): Future[Result[PlanningAttemptResponse]] =
    sendJson(dom.HttpMethod.POST, "/planning/attempts", jsonBody(request))
end ApiClient
