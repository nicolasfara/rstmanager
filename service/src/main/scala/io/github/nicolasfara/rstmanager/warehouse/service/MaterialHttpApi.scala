package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.{ UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierError, SupplierId }
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp.StoredMaterial
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier

import cats.data.{ EitherT, ValidatedNec }
import cats.effect.{ IO, Outcome, Ref }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/**
 * JSON DTOs, tapir endpoints, and http4s server logic for the warehouse material API under `/warehouse/materials`, including the bulk import from
 * Excel (`POST /warehouse/materials/import`, see [[MaterialImport]]).
 *
 * Prices travel as decimal strings with exactly 4 decimals (`"3.7000"`), quantities with 3 (`"2.500"`), amounts with 2 (`"12.34"`), units as their
 * wire name (`"linear_meters"`), days as `YYYY-MM-DD`. Error messages are in Italian.
 */
object MaterialHttpApi:
  import ApiError.ApiFailure
  import WarehouseHttpSupport.*

  /**
   * A manufacturer part number: the same shape in requests and responses. `expectedUnitCost` (4 decimals) is the purchase price the supplier is
   * expected to charge, which a delivery note line starts from; absent (or blank) when there is none.
   */
  final case class MpnDto(code: String, supplierId: UUID, description: Option[String], expectedUnitCost: Option[String]):
    /** The MPN, each problem prefixed with `label` (the row of the MPN editor, `MPN riga 2`). */
    def toDomain(label: String): ValidatedNec[String, Mpn] =
      (
        normalized(expectedUnitCost).traverse(cost => field(s"$label (prezzo acquisto previsto)", UnitPrice.fromDecimalString(cost).toValidatedNec)),
        Mpn.createMpn(code, SupplierId(supplierId), description, None).leftMap(_.map(error => s"$label: $error")),
      ).mapN((cost, mpn) => mpn.copy(expectedUnitCost = cost))

  object MpnDto:
    def fromDomain(mpn: Mpn): MpnDto = MpnDto(mpn.code, mpn.supplierId.value, mpn.description, mpn.expectedUnitCost.map(_.toDecimalString))

  final case class MaterialRequest(
      internalCode: String,
      description: String,
      materialType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: String,
      mpns: List[MpnDto],
  ):
    /** Validates the request into material data, including the no-duplicate-MPN rule, so every problem is reported at once. */
    def toDomain(id: UUID): ValidatedNec[String, MaterialInfo] =
      (
        UnitOfMeasure.fromWireName(unit).leftMap(error => s"Unità di misura: $error").toValidatedNec,
        UnitPrice.fromDecimalString(customerPrice).leftMap(error => s"Prezzo cliente: $error").toValidatedNec,
        mpns.zipWithIndex.traverse((dto, index) => dto.toDomain(s"MPN riga ${index + 1}")),
      ).tupled
        .andThen((parsedUnit, price, parsedMpns) =>
          MaterialInfo.createMaterialInfo(MaterialId(id), internalCode, description, materialType, notes, parsedUnit, price, parsedMpns),
        )
        .andThen(_.checkInvariants.leftMap(_.map(describeInvariant)))
  end MaterialRequest

  object MaterialRequest:
    val example: MaterialRequest =
      MaterialRequest(
        "TES-001",
        "Tessuto acrilico blu navy, altezza 152 cm",
        Some("Tessuti"),
        Some("Per tendalini e capottine"),
        UnitOfMeasure.LinearMeters.wireName,
        "18.5000",
        List(
          MpnDto("AC-152-NAVY", UUID.fromString("00000000-0000-0000-0000-000000000301").nn, Some("Acrylic canvas 152 cm navy"), Some("3.5000")),
        ),
      )

  /** A supplier of the material, derived from its MPNs; `active` is false for a supplier deleted after the MPN was added. */
  final case class MaterialSupplierDto(id: UUID, name: String, active: Boolean)

  object MaterialSupplierDto:
    /** The supplier `id` named from `suppliers`; a supplier missing from it is shown by id, as inactive. */
    def of(id: SupplierId, suppliers: Map[SupplierId, StoredSupplier]): MaterialSupplierDto =
      suppliers
        .get(id)
        .fold(MaterialSupplierDto(id.value, id.value.toString, active = false))(stored =>
          MaterialSupplierDto(id.value, stored.supplier.name, !stored.deleted),
        )

  /**
   * Stock of a material on a day: signed quantity (negative only as an anomaly), highest unit cost recorded so far, value (`quantity × maxUnitCost`),
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

  final case class MaterialResponse(
      id: UUID,
      internalCode: String,
      description: String,
      materialType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: String,
      mpns: List[MpnDto],
      suppliers: List[MaterialSupplierDto],
      createdOn: String,
      archived: Boolean,
      archivedOn: Option[String],
      stock: StockDto,
  )

  object MaterialResponse:
    /**
     * The response for `material`, naming its suppliers from `suppliers` (a supplier missing from it is shown by id, as inactive), with its stock at
     * the end of `today`.
     */
    def fromDomain(material: StoredMaterial, suppliers: Map[SupplierId, StoredSupplier], today: LocalDate): MaterialResponse =
      val info = material.info
      MaterialResponse(
        info.id.value,
        info.internalCode,
        info.description,
        info.materialType,
        info.notes,
        info.unit.wireName,
        info.customerPrice.toDecimalString,
        info.mpns.map(MpnDto.fromDomain),
        info.suppliers.map(MaterialSupplierDto.of(_, suppliers)),
        formatDay(material.material.createdOn),
        material.archived,
        material.archivedOn.map(formatDay),
        StockDto.fromDomain(material.material.ledger.snapshotAt(today)),
      )
  end MaterialResponse

  /**
   * The outcome of a material import: the internal codes created (or, on a dry run, to be created), the codes skipped because an active material
   * already has them, the suppliers created (to be created) by name, and the number of MPNs of the created materials. Codes are sorted.
   */
  final case class MaterialImportResponse(dryRun: Boolean, created: List[String], skipped: List[String], newSuppliers: List[String], mpns: Int)

  object MaterialImportResponse:
    def of(plan: MaterialImport.ImportPlan, dryRun: Boolean): MaterialImportResponse =
      MaterialImportResponse(
        dryRun,
        plan.materials.map(material => material.info.internalCode: String).sorted,
        plan.skipped.map(_.code).sorted,
        plan.suppliers.map(supplier => supplier.name: String),
        plan.materials.map(_.info.mpns.size).sum,
      )

  private def describeInvariant(error: MaterialError): String = error match
    case MaterialError.DuplicateMpn(_, code) => duplicateMpnMessage(code)
    case other => other.toString

  given Codec[MpnDto] = deriveCodec
  given Codec[MaterialRequest] = deriveCodec
  given Codec[MaterialSupplierDto] = deriveCodec
  given Codec[StockDto] = deriveCodec
  given Codec[MaterialResponse] = deriveCodec
  given Codec[MaterialImportResponse] = deriveCodec
  given Schema[MpnDto] = Schema.derived
  given Schema[MaterialRequest] = Schema.derived
  given Schema[MaterialSupplierDto] = Schema.derived
  given Schema[StockDto] = Schema.derived
  given Schema[MaterialResponse] = Schema.derived
  given Schema[MaterialImportResponse] = Schema.derived

  private val collection = "warehouse" / "materials"

  val create: Secured.SecuredEndpoint[MaterialRequest, MaterialResponse] =
    Secured.base.post
      .in(collection)
      .tag("Warehouse")
      .summary("Create a material")
      .in(jsonBody[MaterialRequest].example(MaterialRequest.example))
      .out(jsonBody[MaterialResponse])

  val list: Secured.SecuredEndpoint[Option[Boolean], List[MaterialResponse]] =
    Secured.base.get
      .in(collection)
      .in(query[Option[Boolean]]("includeArchived").description("Also list archived materials (default false)."))
      .tag("Warehouse")
      .summary("List materials")
      .out(jsonBody[List[MaterialResponse]])

  val read: Secured.SecuredEndpoint[UUID, MaterialResponse] =
    Secured.base.get
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Read a material, active or archived")
      .out(jsonBody[MaterialResponse])

  val update: Secured.SecuredEndpoint[(UUID, MaterialRequest), MaterialResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Replace an active material")
      .in(jsonBody[MaterialRequest].example(MaterialRequest.example))
      .out(jsonBody[MaterialResponse])

  val archive: Secured.SecuredEndpoint[UUID, Unit] =
    Secured.base.delete
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Archive a material (it stays readable for the history)")
      .out(statusCode(StatusCode.NoContent))

  val importXlsx: Secured.SecuredEndpoint[(Option[Boolean], Array[Byte]), MaterialImportResponse] =
    Secured.base.post
      .in(collection / "import")
      .in(query[Option[Boolean]]("dryRun").description("Only check the file and report what would be created (default false)."))
      .in(
        xlsxBody.description(
          s"The .xlsx workbook (at most ${MaterialImport.maxBytes / (1024 * 1024)} MB): first sheet, headers on row 1 " +
            s"(${MaterialImport.Column.values.map(_.header).mkString(", ")}), one row per MPN.",
        ),
      )
      .tag("Warehouse")
      .summary("Import materials from an Excel workbook, all or nothing (422 import-rejected lists every problem by row)")
      .out(jsonBody[MaterialImportResponse])

  def endpoints: List[AnyEndpoint] = List(create, list, read, update, archive, importXlsx)

  def routes(store: MaterialApp.Store, suppliers: SupplierApp.Store, security: ApiSecurity): List[ServerEndpoint[Any, IO]] = List(
    security.secure(create, Role.Admin)(createLogic(store, suppliers)),
    security.secure(list, Role.Viewer)(listLogic(store, suppliers)),
    security.secure(read, Role.Viewer)(readLogic(store, suppliers)),
    security.secure(update, Role.Admin)(updateLogic(store, suppliers)),
    security.secure(archive, Role.Admin)(archiveLogic(store)),
    security.secure(importXlsx, Role.Admin)(importLogic(store, suppliers)),
  )

  /** The import was rejected: nothing was written. `problems` are listed by row. */
  private def importRejected(problems: List[String]): ApiFailure =
    StatusCode.UnprocessableEntity -> ApiError("import-rejected", "Caricamento annullato: nessun materiale è stato salvato.", problems)

  /** The import failed while writing and what it had written was undone (the details say what could not be). */
  private def importFailed(status: StatusCode, details: List[String]): ApiFailure =
    status -> ApiError("import-failed", "Caricamento interrotto e annullato.", details)

  /**
   * Reads and checks the workbook (see [[MaterialImport.plan]]) against the active materials and suppliers; on a dry run answers with what would be
   * created, otherwise writes the new suppliers and then the materials ([[writeImport]]).
   */
  private def importLogic(store: MaterialApp.Store, suppliers: SupplierApp.Store)(
      dryRun: Option[Boolean],
      body: Array[Byte],
  ): IO[Either[ApiFailure, MaterialImportResponse]] =
    if body.length > MaterialImport.maxBytes then
      IO.pure(importRejected(List(s"File: il file supera ${MaterialImport.maxBytes / (1024 * 1024)} MB, il massimo consentito")).asLeft)
    else
      val imported =
        for
          read <- IO.blocking(MaterialImport.readSheet(body))
          result <- read match
            case Left(problems) => IO.pure(importRejected(problems.toChain.toList.sortBy(_.line).map(_.message)).asLeft)
            case Right(rows) =>
              for
                day <- today
                materials <- MaterialApp.list(store)
                active <- SupplierApp.list(suppliers)
                ids <- newIds(MaterialImport.draft(rows))
                planned = MaterialImport.plan(rows, MaterialImport.ImportContext(day, materials.map(_.info), active, ids))
                outcome <- planned.toEither match
                  case Left(problems) => IO.pure(importRejected(problems.toChain.toList.map(_.message)).asLeft)
                  case Right(plan) if dryRun.getOrElse(false) => IO.pure(MaterialImportResponse.of(plan, dryRun = true).asRight)
                  case Right(plan) => writeImport(store, suppliers, plan)
              yield outcome
        yield result
      imported.handleError(error => internal(error).asLeft)

  /** A fresh id for every code and supplier name of the draft. */
  private def newIds(draft: MaterialImport.ImportDraft): IO[MaterialImport.ImportIds] =
    (
      draft.codes.traverse(code => IO(MaterialId(UUID.randomUUID().nn)).map(code -> _)),
      draft.supplierKeys.traverse(key => IO(SupplierId(UUID.randomUUID().nn)).map(key -> _)),
    ).mapN((materials, suppliers) => MaterialImport.ImportIds(materials.toMap, suppliers.toMap))

  /** What an import has written so far (or may have: a write that failed with an error), to undo it. */
  private final case class Written(suppliers: List[Supplier], materials: List[MaterialInfo])

  /**
   * Writes the new suppliers, then the materials, one at a time; uncancelable, so that a dropped connection cannot leave the warehouse half loaded.
   * On a rejected or failed write (a concurrent change, the database) everything written is undone ([[compensate]]) and the import fails.
   */
  private def writeImport(
      store: MaterialApp.Store,
      suppliers: SupplierApp.Store,
      plan: MaterialImport.ImportPlan,
  ): IO[Either[ApiFailure, MaterialImportResponse]] =
    IO.uncancelable { _ =>
      Ref.of[IO, Written](Written(Nil, Nil)).flatMap { written =>
        def tracked[E](write: IO[Either[E, Unit]], mark: Written => Written): EitherT[IO, E, Unit] =
          EitherT(write.guaranteeCase {
            case Outcome.Succeeded(result) => result.flatMap(_.fold(_ => IO.unit, _ => written.update(mark)))
            case Outcome.Errored(_) | Outcome.Canceled() => written.update(mark)
          })
        val writes =
          plan.suppliers.traverse_(supplier =>
            tracked(SupplierApp.create(suppliers, supplier), done => done.copy(suppliers = supplier :: done.suppliers))
              .leftMap(error => s"Fornitore ${supplier.name}: ${SupplierHttpApi.failure(error, supplier.id.value)._2.message}"),
          ) >>
            plan.materials.traverse_(material =>
              tracked(MaterialApp.create(store, material.info), done => done.copy(materials = material.info :: done.materials)).leftMap(error =>
                s"Materiale ${material.info.internalCode}: ${materialProblems(error, material.info.id.value, Some(material.info)).mkString("; ")}",
              ),
            )
        writes.value.attempt.flatMap {
          case Right(Right(())) => IO.pure(MaterialImportResponse.of(plan, dryRun = false).asRight)
          case Right(Left(cause)) =>
            written.get.flatMap(compensate(store, suppliers, _)).map(undone => importFailed(StatusCode.Conflict, cause :: undone).asLeft)
          case Left(error) =>
            written.get
              .flatMap(compensate(store, suppliers, _))
              .map(undone => importFailed(StatusCode.InternalServerError, s"Errore imprevisto: $error" :: undone).asLeft)
        }
      }
    }

  /**
   * Undoes an interrupted import: archives the materials it created (which frees their codes and MPNs) and deletes the suppliers it created, both
   * soft. A material or supplier that turns out not to exist was never written; what cannot be undone is listed.
   */
  private def compensate(store: MaterialApp.Store, suppliers: SupplierApp.Store, written: Written): IO[List[String]] =
    for
      materials <- written.materials.traverse(info =>
        MaterialApp.archive(store, info.id).attempt.map {
          case Right(Right(())) | Right(Left(MaterialError.MaterialNotFound)) => None
          case Right(Left(error)) => Some(s"Non annullato: il materiale ${info.internalCode} non è stato archiviato ($error)")
          case Left(error) => Some(s"Non annullato: il materiale ${info.internalCode} non è stato archiviato ($error)")
        },
      )
      removed <- written.suppliers.traverse(supplier =>
        SupplierApp.delete(suppliers, supplier.id).attempt.map {
          case Right(Right(())) | Right(Left(SupplierError.SupplierNotFound)) => None
          case Right(Left(error)) => Some(s"Non annullato: il fornitore ${supplier.name} non è stato eliminato ($error)")
          case Left(error) => Some(s"Non annullato: il fornitore ${supplier.name} non è stato eliminato ($error)")
        },
      )
    yield (materials ++ removed).flatten

  private def createLogic(store: MaterialApp.Store, suppliers: SupplierApp.Store)(
      request: MaterialRequest,
  ): IO[Either[ApiFailure, MaterialResponse]] =
    IO(UUID.randomUUID().nn).flatMap(id => save(store, suppliers, id, request, MaterialApp.create))

  private def updateLogic(store: MaterialApp.Store, suppliers: SupplierApp.Store)(
      id: UUID,
      request: MaterialRequest,
  ): IO[Either[ApiFailure, MaterialResponse]] =
    save(store, suppliers, id, request, MaterialApp.update)

  /** Validates the request, checks it against the suppliers and the other materials, then sends it with `command` and returns the stored material. */
  private def save(
      store: MaterialApp.Store,
      suppliers: SupplierApp.Store,
      id: UUID,
      request: MaterialRequest,
      command: (MaterialApp.Store, MaterialInfo) => IO[Either[MaterialError, Unit]],
  ): IO[Either[ApiFailure, MaterialResponse]] =
    request.toDomain(id).toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right(info) =>
        val saved = for
          problem <- checkReferences(store, suppliers, info)
          result <- problem.fold(command(store, info).map(_.leftMap(materialFailure(_, id, Some(info)))))(failure => IO.pure(failure.asLeft))
          response <- result.flatTraverse(_ => readLogic(store, suppliers)(id))
        yield response
        saved.handleError(error => internal(error).asLeft)

  /**
   * The cross-aggregate rules of a material: the supplier of every MPN new to the material exists and is active (an MPN the stored material already
   * has keeps its supplier even after the supplier is deleted), and no other active material has the same internal code or one of its (supplier, MPN
   * code) pairs. Archived materials release their code and MPNs.
   */
  private def checkReferences(store: MaterialApp.Store, suppliers: SupplierApp.Store, info: MaterialInfo): IO[Option[ApiFailure]] =
    for
      stored <- MaterialApp.find(store, info.id)
      unavailable <- suppliersToCheck(stored.map(_.info), info).traverseFilter(supplierId =>
        SupplierApp.getAny(suppliers, supplierId).map {
          case Some(StoredSupplier(_, false)) => None
          case Some(StoredSupplier(supplier, true)) => Some(s"${supplier.name} (eliminato)")
          case None => Some(supplierId.value.toString)
        },
      )
      others <- MaterialApp.list(store).map(_.filter(_.info.id != info.id))
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
            "material-code-taken",
            s"Il codice ${info.internalCode} è già usato da un altro materiale.",
            codeOwners.map(other => s"${other.info.internalCode} - ${other.info.description}"),
          ),
        )
      else if takenMpns.nonEmpty then Some(conflict("mpn-taken", "Uno o più MPN sono già usati da un altro materiale.", takenMpns))
      else None

  /**
   * The suppliers that must be active for `info` to be saved over `stored`: those of the (supplier, MPN code) pairs `stored` does not already have,
   * so that deleting a supplier never blocks editing a material that already listed it.
   */
  def suppliersToCheck(stored: Option[MaterialInfo], info: MaterialInfo): List[SupplierId] =
    val storedMpns = stored.toList.flatMap(_.mpns)
    info.mpns
      .filterNot(mpn => storedMpns.exists(owned => owned.supplierId == mpn.supplierId && (owned.code: String) == (mpn.code: String)))
      .map(_.supplierId)
      .distinct

  private def listLogic(store: MaterialApp.Store, suppliers: SupplierApp.Store)(
      includeArchived: Option[Boolean],
  ): IO[Either[ApiFailure, List[MaterialResponse]]] =
    MaterialApp
      .listAll(store)
      .map(_.filter(material => includeArchived.getOrElse(false) || !material.archived).sortBy(material => material.info.internalCode: String))
      .flatMap(materials => hydrate(suppliers, materials))
      .attempt
      .map(_.leftMap(internal))

  private[service] def readLogic(store: MaterialApp.Store, suppliers: SupplierApp.Store)(id: UUID): IO[Either[ApiFailure, MaterialResponse]] =
    MaterialApp
      .find(store, MaterialId(id))
      .flatMap {
        case Some(material) => hydrate(suppliers, List(material)).map(_.headOption.toRight(materialNotFound(id)))
        case None => IO.pure(materialNotFound(id).asLeft)
      }
      .handleError(error => internal(error).asLeft)

  private def archiveLogic(store: MaterialApp.Store)(id: UUID): IO[Either[ApiFailure, Unit]] =
    MaterialApp
      .find(store, MaterialId(id))
      .flatMap {
        case None => IO.pure(materialNotFound(id).asLeft)
        case Some(material) => MaterialApp.archive(store, MaterialId(id)).map(_.leftMap(materialFailure(_, id, Some(material.info))))
      }
      .handleError(error => internal(error).asLeft)

  /** The suppliers referenced by `ids`, active or deleted, each read once. */
  private[service] def suppliersById(suppliers: SupplierApp.Store, ids: List[SupplierId]): IO[Map[SupplierId, StoredSupplier]] =
    ids.distinct.traverseFilter(id => SupplierApp.getAny(suppliers, id).map(_.map(id -> _))).map(_.toMap)

  /** Responses for `materials`, with their stock today, reading each referenced supplier (active or deleted) once. */
  private def hydrate(suppliers: SupplierApp.Store, materials: List[StoredMaterial]): IO[List[MaterialResponse]] =
    for
      day <- today
      found <- suppliersById(suppliers, materials.flatMap(_.info.suppliers))
    yield materials.map(MaterialResponse.fromDomain(_, found, day))
end MaterialHttpApi
