package io.gitbub.nicolasfara.rstmanager.api

import java.util.UUID

import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.timers.setTimeout
import scala.util.{ Failure, Success }

import io.circe.{ Decoder, Encoder }
import io.circe.parser.decode
import io.circe.syntax.*
import io.gitbub.nicolasfara.rstmanager.auth.AuthService
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

  /** In-flight GET requests keyed by path. Concurrent identical GETs share the same Future instead of fanning out. */
  private val inFlight = collection.mutable.Map.empty[String, Future[(Int, String)]]

  /** Short-lived response cache for GET requests. Reduces redundant fetches when multiple subscribers react to the same tick. */
  private val cacheTtlMs = 500.0
  private case class CacheEntry(status: Int, text: String, expiresAt: Double)
  private val responseCache = collection.mutable.Map.empty[String, CacheEntry]

  /** Drops all cached responses; called on login/logout so no data leaks across sessions. */
  def clearCache(): Unit =
    responseCache.clear()
    inFlight.clear()

  /** A request body with its media type: JSON for most calls, the raw bytes of a file for uploads. */
  private final case class Payload(contentType: String, content: dom.BodyInit)

  private def jsonPayload(body: Option[String]): Option[Payload] = body.map(json => Payload("application/json", json))

  /** Media type of an Excel workbook (`.xlsx`). */
  private val xlsxMediaType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

  /** The fetch options of a request: `method`, the bearer token (when there is one), and the `payload` with its `Content-Type`. */
  private def requestInit(method: dom.HttpMethod, token: Option[String], payload: Option[Payload]): dom.RequestInit =
    val init = new dom.RequestInit {}
    init.method = method
    val headers = new dom.Headers()
    token.foreach(value => headers.append("Authorization", s"Bearer $value"))
    payload.foreach { p =>
      headers.append("Content-Type", p.contentType)
      init.body = p.content
    }
    init.headers = headers
    init

  /** Token acquisition happens here, past the cache/dedup short-circuits, so `updateToken` runs only when a request actually goes out. */
  private def authedFetch(method: dom.HttpMethod, path: String, payload: Option[Payload]): Future[(Int, String)] =
    AuthService.bearerToken().flatMap { tokenOpt =>
      dom.fetch(baseUrl + path, requestInit(method, tokenOpt, payload)).flatMap(response => response.text().map(text => (response.status, text)))
    }

  /** Backoff for GETs hitting a backend that is (re)starting behind the proxy: ~15s in total, the spinner stays up meanwhile. */
  private val transientRetryDelaysMs = List(500, 1000, 2000, 4000, 8000)

  /** Gateway statuses the proxy emits when the backend is unreachable; the request never got a real answer, so a GET is safe to repeat. */
  private def isTransient(status: Int): Boolean = status == 502 || status == 503 || status == 504

  private def delay(ms: Int): Future[Unit] =
    val promise = Promise[Unit]()
    setTimeout(ms.toDouble)(promise.success(()))
    promise.future

  private def fetchWithRetry(path: String, delays: List[Int]): Future[(Int, String)] =
    val attempt = authedFetch(dom.HttpMethod.GET, path, None)
    delays match
      case Nil => attempt
      case next :: rest =>
        attempt.transformWith {
          case Success((status, _)) if isTransient(status) => delay(next).flatMap(_ => fetchWithRetry(path, rest))
          case Failure(_) => delay(next).flatMap(_ => fetchWithRetry(path, rest))
          case done => Future.fromTry(done)
        }

  /** How long a downloaded file's blob URL stays alive: revoking it right after the click can cancel the download in some browsers. */
  private val blobUrlLifetimeMs = 10000.0

  /**
   * Downloads the file at `path` (authenticated `GET`, never cached) and lets the browser save it as `fileName`: the response body becomes a blob
   * URL, opened through a temporary `<a download>`. A non-2xx response is parsed as an [[Dtos.ApiError]], and a 401 drops the session, as for every
   * call.
   */
  def download(path: String, fileName: String): Future[Result[Unit]] =
    AuthService
      .bearerToken()
      .flatMap(tokenOpt => dom.fetch(baseUrl + path, requestInit(dom.HttpMethod.GET, tokenOpt, None)))
      .flatMap { response =>
        val result: Future[Result[Unit]] =
          if isSuccess(response.status) then response.blob().map(saveBlob(_, fileName))
          else
            response.text().map { text =>
              handleUnauthorized(response.status)
              Left(parseError(response.status, text))
            }
        result
      }
      .recover { case err => Left(ApiError("network-error", errorMessage(err), Nil)) }

  /** Hands `blob` to the browser as a download named `fileName`. */
  private def saveBlob(blob: dom.Blob, fileName: String): Result[Unit] =
    dom.document.createElement("a") match
      case anchor: dom.HTMLAnchorElement =>
        val url = dom.URL.createObjectURL(blob)
        anchor.href = url
        anchor.download = fileName
        anchor.style.display = "none"
        dom.document.body.appendChild(anchor)
        anchor.click()
        dom.document.body.removeChild(anchor)
        js.timers.setTimeout(blobUrlLifetimeMs)(dom.URL.revokeObjectURL(url))
        Right(())
      case _ => Left(ApiError("download-error", "Il browser non consente di scaricare il file.", Nil))

  private def rawSend(method: dom.HttpMethod, path: String, body: Option[String]): Future[(Int, String)] =
    // String comparison sidesteps the missing CanEqual instance on dom.HttpMethod under -language:strictEquality.
    if method.toString == "GET" then
      responseCache.get(path) match
        case Some(entry) if js.Date.now() < entry.expiresAt => Future.successful((entry.status, entry.text))
        case _ =>
          inFlight.get(path) match
            case Some(existing) => existing
            case None =>
              val resultF = fetchWithRetry(path, transientRetryDelaysMs)
              inFlight(path) = resultF
              resultF.onComplete {
                case Success((status, text)) =>
                  if isSuccess(status) then responseCache(path) = CacheEntry(status, text, js.Date.now() + cacheTtlMs)
                  inFlight.remove(path)
                case Failure(_) => inFlight.remove(path)
              }
              resultF
    else
      evictResource(path)
      authedFetch(method, path, jsonPayload(body))

  /** On any mutation, evicts all cached entries for the same base resource (first path segment): `/warehouse/...` drops every warehouse read. */
  private def evictResource(path: String): Unit =
    val baseSegment = "/" + path.stripPrefix("/").takeWhile(_ != '/')
    responseCache.keys.filter(_.startsWith(baseSegment)).toList.foreach(responseCache.remove)

  private def parseError(status: Int, text: String): ApiError =
    decode[ApiError](text).getOrElse(ApiError("http-error", s"HTTP $status", if text.isEmpty then Nil else List(text)))

  private def isSuccess(status: Int): Boolean = status >= 200 && status < 300

  private def errorMessage(err: Throwable): String = Option[String](err.getMessage).getOrElse("Errore di rete")

  /** A 401 means the token no longer satisfies the server: drop the session and go through the hosted login again. */
  private def handleUnauthorized(status: Int): Unit =
    if status == 401 then AuthService.forceReauth()

  /** A 2xx response decoded as `A`; any other status parsed as an [[Dtos.ApiError]] (a 401 also drops the session). */
  private def decodeResponse[A: Decoder](status: Int, text: String): Result[A] =
    if isSuccess(status) then decode[A](text).left.map(err => ApiError("decode-error", s"Cannot decode response: ${err.getMessage}", List(text)))
    else
      handleUnauthorized(status)
      Left(parseError(status, text))

  private def sendJson[A: Decoder](method: dom.HttpMethod, path: String, body: Option[String]): Future[Result[A]] =
    rawSend(method, path, body)
      .map((status, text) => decodeResponse[A](status, text))
      .recover { case err => Left(ApiError("network-error", errorMessage(err), Nil)) }

  private def sendUnit(method: dom.HttpMethod, path: String, body: Option[String]): Future[Result[Unit]] =
    rawSend(method, path, body).map { (status, text) =>
      if isSuccess(status) then Right(())
      else
        handleUnauthorized(status)
        Left(parseError(status, text))
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
  def setTaskPreferredEmployee(orderId: UUID, manufacturingId: UUID, taskId: UUID, employeeId: Option[UUID]): Future[Result[OrderResponse]] =
    sendJson(
      dom.HttpMethod.PUT,
      s"/orders/$orderId/manufacturings/$manufacturingId/tasks/$taskId/employee",
      jsonBody(SetPreferredEmployeeRequest(employeeId)),
    )

  /**
   * Replaces the material lines of a manufacturing (Operator; only while the order is in progress or suspended, 409 `order-not-editable` otherwise).
   * A line without `unitPrice` keeps its frozen price, or a new line freezes the material's current customer price.
   */
  def setManufacturingMaterials(orderId: UUID, manufacturingId: UUID, materials: List[MaterialLineDto]): Future[Result[OrderResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/orders/$orderId/manufacturings/$manufacturingId/materials", jsonBody(MaterialsUpdateRequest(materials)))
  def deleteOrder(id: UUID, reason: Option[String]): Future[Result[Unit]] =
    val query = reason.filter(_.nonEmpty).fold("")(r => s"?reason=${js.URIUtils.encodeURIComponent(r)}")
    sendUnit(dom.HttpMethod.DELETE, s"/orders/$id$query", None)

  // ---- Planning ----------------------------------------------------------------------------------

  def currentPlanning(): Future[Result[PlanningStateDto]] = sendJson(dom.HttpMethod.GET, "/planning/attempts/current", None)
  def createPlanningAttempt(request: PlanningAttemptRequest): Future[Result[PlanningAttemptResponse]] =
    sendJson(dom.HttpMethod.POST, "/planning/attempts", jsonBody(request))
  def simulateOrder(request: OrderSimulationRequest): Future[Result[OrderSimulationResponse]] =
    sendJson(dom.HttpMethod.POST, "/planning/simulate", jsonBody(request))

  // ---- Warehouse ---------------------------------------------------------------------------------
  // Every warehouse endpoint lives under `/warehouse`, so any warehouse mutation evicts all cached warehouse reads (see `rawSend`).

  def listSuppliers(): Future[Result[List[SupplierResponse]]] = sendJson(dom.HttpMethod.GET, "/warehouse/suppliers", None)
  def createSupplier(request: SupplierRequest): Future[Result[SupplierResponse]] =
    sendJson(dom.HttpMethod.POST, "/warehouse/suppliers", jsonBody(request))
  def updateSupplier(id: UUID, request: SupplierRequest): Future[Result[SupplierResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/warehouse/suppliers/$id", jsonBody(request))
  def deleteSupplier(id: UUID): Future[Result[Unit]] = sendUnit(dom.HttpMethod.DELETE, s"/warehouse/suppliers/$id", None)

  def listMaterials(includeArchived: Boolean): Future[Result[List[MaterialResponse]]] =
    val query = if includeArchived then "?includeArchived=true" else ""
    sendJson(dom.HttpMethod.GET, s"/warehouse/materials$query", None)
  def createMaterial(request: MaterialRequest): Future[Result[MaterialResponse]] =
    sendJson(dom.HttpMethod.POST, "/warehouse/materials", jsonBody(request))
  def updateMaterial(id: UUID, request: MaterialRequest): Future[Result[MaterialResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/warehouse/materials/$id", jsonBody(request))

  /**
   * Bulk import of materials from an Excel workbook (Admin): the `.xlsx` bytes are the request body, not a multipart form. With `dryRun` the server
   * only validates the file and says what an upload would do; otherwise it saves everything or, on any problem, nothing (422 `import-rejected` with
   * one detail per problem). A real import may also create suppliers: both live under `/warehouse`, so the eviction drops every cached warehouse
   * read. It runs when the response arrives, since the import can take a while and reads cached meanwhile would be stale.
   */
  def importMaterials(file: dom.File, dryRun: Boolean): Future[Result[MaterialImportResponse]] =
    val path = s"/warehouse/materials/import${queryString(List("dryRun" -> Some(dryRun.toString)))}"
    val bytes: dom.Blob = file
    val response = authedFetch(dom.HttpMethod.POST, path, Some(Payload(xlsxMediaType, bytes)))
    if !dryRun then response.onComplete(_ => evictResource(path))
    response
      .map((status, text) => decodeResponse[MaterialImportResponse](status, text))
      .recover { case err => Left(ApiError("network-error", errorMessage(err), Nil)) }

  /** `DELETE` on a material archives it: it stays readable (with `includeArchived`) for the warehouse history. */
  def archiveMaterial(id: UUID): Future[Result[Unit]] = sendUnit(dom.HttpMethod.DELETE, s"/warehouse/materials/$id", None)

  def recordPurchase(materialId: UUID, request: PurchaseRequest): Future[Result[MovementRecordedResponse]] =
    sendJson(dom.HttpMethod.POST, s"/warehouse/materials/$materialId/purchases", jsonBody(request))
  def recordUnload(materialId: UUID, request: UnloadRequest): Future[Result[MovementRecordedResponse]] =
    sendJson(dom.HttpMethod.POST, s"/warehouse/materials/$materialId/unloads", jsonBody(request))
  def recordAdjustment(materialId: UUID, request: AdjustmentRequest): Future[Result[MovementRecordedResponse]] =
    sendJson(dom.HttpMethod.POST, s"/warehouse/materials/$materialId/adjustments", jsonBody(request))

  /** Voids a movement (Admin); the server requires a non-blank `reason`. */
  def voidMovement(materialId: UUID, movementId: UUID, reason: String): Future[Result[Unit]] =
    sendUnit(dom.HttpMethod.DELETE, s"/warehouse/materials/$materialId/movements/$movementId${queryString(List("reason" -> Some(reason)))}", None)

  /** The movements of a material from `from` to `to` (`YYYY-MM-DD`, inclusive, both optional) with the running balance. */
  def listMovements(materialId: UUID, from: Option[String], to: Option[String]): Future[Result[MovementsResponse]] =
    sendJson(dom.HttpMethod.GET, s"/warehouse/materials/$materialId/movements${queryString(List("from" -> from, "to" -> to))}", None)

  /** The delivery notes ("DDT") dated from `from` to `to` (`YYYY-MM-DD`, inclusive) of `supplierId`, every filter optional; newest first. */
  def listDeliveryNotes(from: Option[String], to: Option[String], supplierId: Option[UUID]): Future[Result[List[DeliveryNoteResponse]]] =
    val query = queryString(List("from" -> from, "to" -> to, "supplierId" -> supplierId.map(_.toString)))
    sendJson(dom.HttpMethod.GET, s"/warehouse/delivery-notes$query", None)
  def getDeliveryNote(id: UUID): Future[Result[DeliveryNoteResponse]] = sendJson(dom.HttpMethod.GET, s"/warehouse/delivery-notes/$id", None)

  /** Registers a delivery note (Operator): it loads the stock of every material of its lines. */
  def createDeliveryNote(request: DeliveryNoteRequest): Future[Result[DeliveryNoteResponse]] =
    sendJson(dom.HttpMethod.POST, "/warehouse/delivery-notes", jsonBody(request))

  /** Replaces a delivery note (Operator): the server voids the stock loads of its previous version and records the new ones. */
  def updateDeliveryNote(id: UUID, request: DeliveryNoteRequest): Future[Result[DeliveryNoteResponse]] =
    sendJson(dom.HttpMethod.PUT, s"/warehouse/delivery-notes/$id", jsonBody(request))

  /** Cancels a delivery note (Admin), removing the stock it loaded; the server requires a non-blank `reason`. */
  def cancelDeliveryNote(id: UUID, reason: String): Future[Result[DeliveryNoteResponse]] =
    sendJson(dom.HttpMethod.POST, s"/warehouse/delivery-notes/$id/cancel", jsonBody(CancelDeliveryNoteRequest(reason)))

  /** The warehouse at the end of day `at` (`YYYY-MM-DD`, not after today). */
  def warehouseState(at: String): Future[Result[WarehouseStateResponse]] =
    sendJson(dom.HttpMethod.GET, s"/warehouse/state${queryString(List("at" -> Some(at)))}", None)

  /** Downloads the Excel workbook of the warehouse at the end of day `at` (`YYYY-MM-DD`) as `magazzino-<at>.xlsx`. */
  def exportWarehouse(at: String): Future[Result[Unit]] =
    download(s"/warehouse/export.xlsx${queryString(List("at" -> Some(at)))}", s"magazzino-${at.trim}.xlsx")

  /** Incoming/outgoing stock value per `granularity` (`week` or `month`) bucket from `from` to `to`. */
  def warehouseFlows(from: String, to: String, granularity: String): Future[Result[FlowReportResponse]] =
    val query = queryString(List("from" -> Some(from), "to" -> Some(to), "granularity" -> Some(granularity)))
    sendJson(dom.HttpMethod.GET, s"/warehouse/flows$query", None)

  /** What was bought from each supplier, per MPN, from `from` to `to`. */
  def supplierSpend(from: String, to: String): Future[Result[SupplierSpendReportResponse]] =
    sendJson(dom.HttpMethod.GET, s"/warehouse/supplier-spend${queryString(List("from" -> Some(from), "to" -> Some(to)))}", None)

  /** `?a=1&b=2` from the present, non-blank parameters (values URI-encoded); empty when none is left. */
  private def queryString(params: List[(String, Option[String])]): String =
    val present = params.collect { case (name, Some(value)) if value.trim.nn.nonEmpty => s"$name=${js.URIUtils.encodeURIComponent(value.trim.nn)}" }
    if present.isEmpty then "" else present.mkString("?", "&", "")
end ApiClient
