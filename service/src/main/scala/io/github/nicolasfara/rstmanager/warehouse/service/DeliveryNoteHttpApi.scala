package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.{ Locale, UUID }

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.{ DeliveryNoteReconciliation, DesiredDeliveryNote, Quantity, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialAggregate, MaterialId, MaterialInfo, MpnCode }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialApp.StoredMaterial
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier

import cats.data.{ NonEmptyChain, Validated, ValidatedNec }
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/**
 * JSON DTOs, tapir endpoints, and http4s server logic for the delivery notes (DDT) under `/warehouse/delivery-notes`: the history (Viewer),
 * registering and changing a note (Operator, who may also add to a material the MPN a line names), and cancelling it (Admin, like the voids).
 *
 * A note is checked against the suppliers, the materials, and the other notes before it is written, and the purchases it brings are dry-run on the
 * materials, so a problem comes back at once as a 422 listing each line (`Riga N: …`) rather than as a failed reconciliation. Once the note is
 * written, its purchases are reconciled on the materials within the same request ([[WarehouseDeliveryNoteReconciler.reconcile]]); a line whose
 * purchase could not be recorded yet is listed with `applied: false`, and the next reconciliation retries it.
 *
 * Quantities travel as decimal strings with 3 decimals, unit costs with 4, amounts with 2, days as `YYYY-MM-DD`, units as their wire name. Error
 * messages are in Italian.
 */
object DeliveryNoteHttpApi:
  import ApiError.ApiFailure
  import WarehouseHttpSupport.*

  /**
   * One line of a note: `quantity` of material `materialId` under the MPN `mpnCode` of the note's supplier at `unitCost`. With `newMpn` the MPN, when
   * the material does not have it yet, is added to the material with `unitCost` as its expected unit cost.
   */
  final case class DeliveryNoteLineRequest(materialId: UUID, mpnCode: String, newMpn: Boolean, quantity: String, unitCost: String):
    /** The parsed line, each problem prefixed with `Riga n`. */
    def parse(n: Int): ValidatedNec[String, ParsedLine] =
      (
        field(s"Riga $n: MPN", MaterialInfo.normalizedCode(mpnCode).refineValidatedNec[MpnCode]),
        field(
          s"Riga $n: Quantità",
          Quantity
            .fromDecimalString(quantity)
            .toValidatedNec
            .andThen(parsed => Validated.condNec(parsed.value > 0L, parsed, "la quantità deve essere maggiore di zero")),
        ),
        field(s"Riga $n: Costo unitario", UnitPrice.fromDecimalString(unitCost).toValidatedNec),
      ).mapN((code, parsedQuantity, cost) => ParsedLine(n, MaterialId(materialId), code, newMpn, parsedQuantity, cost))
  end DeliveryNoteLineRequest

  final case class DeliveryNoteRequest(number: String, date: String, supplierId: UUID, note: Option[String], lines: List[DeliveryNoteLineRequest]):
    /** The parsed request, reporting every malformed field at once. */
    def parse: ValidatedNec[String, ParsedDeliveryNote] =
      (
        field("Numero", number.trim.nn.refineValidatedNec[DeliveryNoteNumber]),
        parseDay(date, "Data"),
        if lines.isEmpty then "Righe: il DDT deve avere almeno una riga".invalidNec
        else lines.zipWithIndex.traverse((line, index) => line.parse(index + 1)),
      ).mapN((parsedNumber, day, parsedLines) => ParsedDeliveryNote(parsedNumber, day, SupplierId(supplierId), normalized(note), parsedLines))

  object DeliveryNoteRequest:
    val example: DeliveryNoteRequest =
      DeliveryNoteRequest(
        "1234/A",
        "2026-11-02",
        UUID.fromString("00000000-0000-0000-0000-000000000301").nn,
        Some("Consegna parziale"),
        List(
          DeliveryNoteLineRequest(UUID.fromString("00000000-0000-0000-0000-000000000401").nn, "AC-152-NAVY", newMpn = false, "25.500", "3.5000"),
          DeliveryNoteLineRequest(UUID.fromString("00000000-0000-0000-0000-000000000402").nn, "VIT-M6-20", newMpn = true, "100", "0.0125"),
        ),
      )

  final case class CancelDeliveryNoteRequest(reason: String):
    def parse: ValidatedNec[String, String :| CancellationReason] = field("Motivo", reason.trim.nn.refineValidatedNec[CancellationReason])

  object CancelDeliveryNoteRequest:
    val example: CancelDeliveryNoteRequest = CancelDeliveryNoteRequest("DDT registrato due volte")

  /** A line as parsed from the request, `n` being its 1-based position (`Riga n` in the messages). */
  final case class ParsedLine(n: Int, materialId: MaterialId, mpnCode: String :| MpnCode, newMpn: Boolean, quantity: Quantity, unitCost: UnitPrice)

  /** A request as parsed, before it is checked against the suppliers, the materials, and the other notes. */
  final case class ParsedDeliveryNote(
      number: String :| DeliveryNoteNumber,
      date: LocalDate,
      supplierId: SupplierId,
      note: Option[String],
      lines: List[ParsedLine],
  )

  /**
   * What a note is checked against.
   *
   * @param supplier
   *   The supplier of the request, active or deleted, if it exists.
   * @param materials
   *   Every material, archived ones included.
   * @param notes
   *   Every delivery note, cancelled ones included.
   * @param existing
   *   The note being changed, when the request changes one.
   */
  final case class CheckContext(
      today: LocalDate,
      supplier: Option[StoredSupplier],
      materials: Map[MaterialId, StoredMaterial],
      notes: List[DeliveryNoteAggregate],
      existing: Option[DeliveryNoteRecord],
  )

  /**
   * The note `parsed` describes, stored under `id`, once it satisfies the cross-aggregate rules: a day not after today; an active supplier (the
   * supplier the changed note already had may have been deleted since); a number no other note of the supplier (not cancelled) has in the same year;
   * existing materials; for each line an MPN of the supplier the material has, or a new one (`newMpn`) no other active material, nor another material
   * of the note, uses; whole quantities for the materials counted in pieces.
   */
  def check(id: DeliveryNoteId, parsed: ParsedDeliveryNote, context: CheckContext): ValidatedNec[String, DeliveryNoteInfo] =
    val supplierName = context.supplier.fold(parsed.supplierId.value.toString)(_.supplier.name: String)
    val supplier = context.supplier match
      case None => "Fornitore: il fornitore indicato non esiste".invalidNec
      case Some(StoredSupplier(_, true)) if !context.existing.exists(_.info.supplierId == parsed.supplierId) =>
        s"Fornitore: il fornitore $supplierName è stato eliminato".invalidNec
      case Some(_) => ().validNec
    val day = Validated.condNec(
      !parsed.date.isAfter(context.today),
      (),
      s"Data: la data ${displayDay(parsed.date)} è successiva a oggi (${displayDay(context.today)})",
    )
    val duplicates = context.notes.collect {
      case DeliveryNoteAggregate.Registered(other)
          if other.info.id != id && other.info.supplierId == parsed.supplierId && other.info.date.getYear == parsed.date.getYear &&
            numberKey(other.info.number) == numberKey(parsed.number) =>
        other.info
    }
    val number = duplicates.headOption.fold(().validNec[String])(other =>
      s"Numero: esiste già il DDT n. ${other.number} del ${displayDay(other.date)} del fornitore $supplierName".invalidNec,
    )
    val lines = parsed.lines.traverse(line => checkLine(line, parsed, context))
    (supplier, day, number, lines).mapN((_, _, _, checked) =>
      DeliveryNoteInfo(id, parsed.number, parsed.date, parsed.supplierId, parsed.note, checked),
    )
  end check

  private def checkLine(line: ParsedLine, parsed: ParsedDeliveryNote, context: CheckContext): ValidatedNec[String, DeliveryNoteLine] =
    context.materials.get(line.materialId) match
      case None => s"Riga ${line.n}: il materiale indicato non esiste".invalidNec
      case Some(stored) =>
        val info = stored.info
        val code: String = line.mpnCode
        val pieces = Validated.condNec(
          info.unit.allowsFractions || line.quantity.isWhole,
          (),
          s"Riga ${line.n}: Quantità: il materiale ${info.internalCode} è gestito a pezzi, la quantità deve essere un numero intero",
        )
        val mpn =
          if info.mpn(parsed.supplierId, code).isDefined then ().validNec
          else if !line.newMpn then s"Riga ${line.n}: MPN: l'MPN $code del fornitore non è fra gli MPN del materiale ${info.internalCode}".invalidNec
          else
            val takenBy = context.materials.values.toList.filter(other =>
              !other.archived && other.info.id != info.id && other.info.mpn(parsed.supplierId, code).isDefined,
            )
            val sharedWith = parsed.lines.filter(other => other.materialId != line.materialId && (other.mpnCode: String) == code)
            takenBy.headOption
              .map(other => s"Riga ${line.n}: MPN: l'MPN $code del fornitore è già usato dal materiale ${other.info.internalCode}")
              .orElse(
                sharedWith.headOption.map(other => s"Riga ${line.n}: MPN: l'MPN $code è indicato anche per un altro materiale (riga ${other.n})"),
              )
              .fold(().validNec[String])(_.invalidNec)
        (pieces, mpn).mapN((_, _) => DeliveryNoteLine(line.materialId, line.mpnCode, line.quantity, info.unit, line.unitCost))
  end checkLine

  /** Numbers compare without case, so `1234/a` and `1234/A` are the same note. */
  private def numberKey(number: String): String = number.toLowerCase(Locale.ROOT).nn

  /**
   * The dry-run of note `desired` (stored under `id`) on the materials: the decision each material would take, on its current state, for the
   * purchases the reconciliation would send it. A rejection is reported on the first line of the material (`Riga N: …`), or on the material when the
   * note no longer names it (`Materiale CODICE: …`).
   */
  def dryRun(
      id: DeliveryNoteId,
      desired: DesiredDeliveryNote,
      firstLines: Map[MaterialId, Int],
      materials: Map[MaterialId, StoredMaterial],
      today: LocalDate,
  ): ValidatedNec[String, Unit] =
    val records = materials.values.toList.map(_.material)
    DeliveryNoteReconciliation.plan(desired, DeliveryNoteReconciliation.applied(id, records)).traverse_ { (materialId, command) =>
      materials.get(materialId).fold(().validNec[String]) { stored =>
        val state = stored.archivedOn.fold(MaterialAggregate.Active(stored.material))(MaterialAggregate.Archived(stored.material, _))
        val label = firstLines.get(materialId).fold(s"Materiale ${stored.info.internalCode}")(n => s"Riga $n")
        state.setDeliveryNotePurchases(command.ref, command.lines, today).toEither match
          case Right(_) => ().validNec
          case Left(errors) =>
            val problems = errors.toChain.toList.flatMap(materialProblems(_, materialId.value, Some(stored.info))).distinct
            NonEmptyChain.fromSeq(problems.map(problem => s"$label: $problem")).fold(().validNec[String])(_.invalid)
      }
    }
  end dryRun

  final case class DeliveryNoteLineResponse(
      materialId: UUID,
      internalCode: String,
      description: String,
      unit: String,
      mpnCode: String,
      quantity: String,
      unitCost: String,
      total: String,
      applied: Boolean,
  )

  /**
   * A delivery note.
   *
   * @param status
   *   `registered` or `cancelled`.
   * @param supplierActive
   *   False for a supplier deleted after the note was registered.
   * @param lines
   *   The lines of the note; `applied` is false while the purchase of the line's material is not yet what the note says (or, for a cancelled note,
   *   not yet cleared): the next reconciliation retries it.
   */
  final case class DeliveryNoteResponse(
      id: UUID,
      number: String,
      date: String,
      supplierId: UUID,
      supplierName: String,
      supplierActive: Boolean,
      note: Option[String],
      status: String,
      registeredOn: String,
      changedOn: Option[String],
      cancelledOn: Option[String],
      cancelReason: Option[String],
      lines: List[DeliveryNoteLineResponse],
      total: String,
  )

  object DeliveryNoteResponse:
    /** The response for `note` (none for a note that does not exist), naming its supplier and materials from the maps. */
    def fromDomain(
        note: DeliveryNoteAggregate,
        suppliers: Map[SupplierId, StoredSupplier],
        materials: Map[MaterialId, StoredMaterial],
    ): Option[DeliveryNoteResponse] =
      (note.record, DeliveryNoteReconciliation.desired(note)).mapN { (record, desired) =>
        val info = record.info
        val pending =
          DeliveryNoteReconciliation.pending(desired, DeliveryNoteReconciliation.applied(info.id, materials.values.toList.map(_.material)))
        val supplier = suppliers.get(info.supplierId)
        val cancellation = note match
          case DeliveryNoteAggregate.Cancelled(_, cancelledOn, reason) => Some((cancelledOn, reason: String))
          case DeliveryNoteAggregate.Registered(_) | DeliveryNoteAggregate.Empty => None
        DeliveryNoteResponse(
          info.id.value,
          info.number,
          formatDay(info.date),
          info.supplierId.value,
          supplier.fold(info.supplierId.value.toString)(_.supplier.name),
          supplier.exists(!_.deleted),
          info.note,
          if cancellation.isDefined then "cancelled" else "registered",
          formatDay(record.registeredOn),
          record.changedOn.map(formatDay),
          cancellation.map((on, _) => formatDay(on)),
          cancellation.map(_._2),
          info.lines.map { line =>
            val material = materials.get(line.materialId).map(_.info)
            DeliveryNoteLineResponse(
              line.materialId.value,
              material.fold(line.materialId.value.toString)(_.internalCode),
              material.fold("")(_.description),
              line.unit.wireName,
              line.mpnCode,
              line.quantity.toDecimalString,
              line.unitCost.toDecimalString,
              line.total.toDecimalString,
              !pending.contains(line.materialId),
            )
          },
          info.total.toDecimalString,
        )
      }
  end DeliveryNoteResponse

  private def failure(error: DeliveryNoteError, id: UUID): ApiFailure = error match
    case DeliveryNoteError.DeliveryNoteAlreadyExists => conflict("delivery-note-already-exists", "Esiste già un DDT con questo id.", Nil)
    case DeliveryNoteError.DeliveryNoteNotFound => deliveryNoteNotFound(id)
    case DeliveryNoteError.DeliveryNoteIsCancelled => deliveryNoteCancelled
    case DeliveryNoteError.DeliveryNoteIdMismatch =>
      conflict("delivery-note-id-mismatch", "L'id del DDT non corrisponde al DDT indicato nel percorso.", Nil)
    case DeliveryNoteError.NoLines => validation(NonEmptyChain.one("Righe: il DDT deve avere almeno una riga"))
    case DeliveryNoteError.NonPositiveQuantity => validation(NonEmptyChain.one("Quantità: la quantità di ogni riga deve essere maggiore di zero"))
    case DeliveryNoteError.DeliveryNoteInFuture(date, today) =>
      validation(NonEmptyChain.one(s"Data: la data ${displayDay(date)} è successiva a oggi (${displayDay(today)})"))

  private def deliveryNoteNotFound(id: UUID): ApiFailure = notFound("Il DDT richiesto non esiste.", List(id.toString))

  private val deliveryNoteCancelled: ApiFailure = conflict("delivery-note-cancelled", "Il DDT è annullato e non può più essere modificato.", Nil)

  given Codec[DeliveryNoteLineRequest] = deriveCodec
  given Codec[DeliveryNoteRequest] = deriveCodec
  given Codec[CancelDeliveryNoteRequest] = deriveCodec
  given Codec[DeliveryNoteLineResponse] = deriveCodec
  given Codec[DeliveryNoteResponse] = deriveCodec
  given Schema[DeliveryNoteLineRequest] = Schema.derived
  given Schema[DeliveryNoteRequest] = Schema.derived
  given Schema[CancelDeliveryNoteRequest] = Schema.derived
  given Schema[DeliveryNoteLineResponse] = Schema.derived
  given Schema[DeliveryNoteResponse] = Schema.derived

  private val collection = "warehouse" / "delivery-notes"

  val list: Secured.SecuredEndpoint[(Option[String], Option[String], Option[UUID]), List[DeliveryNoteResponse]] =
    Secured.base.get
      .in(collection)
      .in(query[Option[String]]("from").description("First day of the notes, YYYY-MM-DD (inclusive)."))
      .in(query[Option[String]]("to").description("Last day of the notes, YYYY-MM-DD (inclusive)."))
      .in(query[Option[UUID]]("supplierId").description("Only the notes of this supplier."))
      .tag("Warehouse")
      .summary("List the delivery notes (DDT), cancelled ones included, by date (latest first) and number")
      .out(jsonBody[List[DeliveryNoteResponse]])

  val read: Secured.SecuredEndpoint[UUID, DeliveryNoteResponse] =
    Secured.base.get
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Read a delivery note (DDT), registered or cancelled")
      .out(jsonBody[DeliveryNoteResponse])

  val create: Secured.SecuredEndpoint[DeliveryNoteRequest, DeliveryNoteResponse] =
    Secured.base.post
      .in(collection)
      .tag("Warehouse")
      .summary("Register a delivery note (DDT): its lines become purchases of their materials")
      .in(jsonBody[DeliveryNoteRequest].example(DeliveryNoteRequest.example))
      .out(jsonBody[DeliveryNoteResponse])

  val update: Secured.SecuredEndpoint[(UUID, DeliveryNoteRequest), DeliveryNoteResponse] =
    Secured.base.put
      .in(collection / path[UUID]("id"))
      .tag("Warehouse")
      .summary("Change a delivery note (DDT): the purchases of its materials are voided and recorded again")
      .in(jsonBody[DeliveryNoteRequest].example(DeliveryNoteRequest.example))
      .out(jsonBody[DeliveryNoteResponse])

  val cancel: Secured.SecuredEndpoint[(UUID, CancelDeliveryNoteRequest), DeliveryNoteResponse] =
    Secured.base.post
      .in(collection / path[UUID]("id") / "cancel")
      .tag("Warehouse")
      .summary("Cancel a delivery note (DDT): the purchases of its materials are voided")
      .in(jsonBody[CancelDeliveryNoteRequest].example(CancelDeliveryNoteRequest.example))
      .out(jsonBody[DeliveryNoteResponse])

  def endpoints: List[AnyEndpoint] = List(list, read, create, update, cancel)

  def routes(
      store: DeliveryNoteApp.Store,
      materials: MaterialApp.Store,
      suppliers: SupplierApp.Store,
      reconciler: WarehouseDeliveryNoteReconciler,
      security: ApiSecurity,
  ): List[ServerEndpoint[Any, IO]] =
    val api = Logic(store, materials, suppliers, reconciler)
    List(
      security.secure(list, Role.Viewer)(api.list),
      security.secure(read, Role.Viewer)(api.read),
      security.secure(create, Role.Operator)(api.create),
      security.secure(update, Role.Operator)(api.update),
      security.secure(cancel, Role.Admin)(api.cancel),
    )

  /** The server logic over the backends of this process. */
  private final class Logic(
      store: DeliveryNoteApp.Store,
      materials: MaterialApp.Store,
      suppliers: SupplierApp.Store,
      reconciler: WarehouseDeliveryNoteReconciler,
  ):
    def list(from: Option[String], to: Option[String], supplierId: Option[UUID]): IO[Either[ApiFailure, List[DeliveryNoteResponse]]] =
      (parseOptionalDay(from, "Dal"), parseOptionalDay(to, "Al")).tupled.andThen {
        case (Some(start), Some(end)) if start.isAfter(end) => periodReversed(start, end).invalidNec
        case period => period.validNec
      }.toEither match
        case Left(errors) => IO.pure(validation(errors).asLeft)
        case Right((start, end)) =>
          val listed =
            for
              notes <- DeliveryNoteApp.listAll(store)
              selected = notes.filter(_.record.exists { record =>
                val info = record.info
                start.forall(!info.date.isBefore(_)) && end.forall(!info.date.isAfter(_)) && supplierId.forall(id =>
                  SupplierId(id) == info.supplierId,
                )
              })
              responses <- responsesOf(selected)
            yield responses
              .sortBy(response => (numberKey(response.number), response.id.toString))
              .sortBy(_.date)(using Ordering.String.reverse)
              .asRight[ApiFailure]
          listed.handleError(error => internal(error).asLeft)

    def read(id: UUID): IO[Either[ApiFailure, DeliveryNoteResponse]] =
      DeliveryNoteApp
        .find(store, DeliveryNoteId(id))
        .flatMap {
          case None => IO.pure(deliveryNoteNotFound(id).asLeft)
          case Some(note) => responsesOf(List(note)).map(_.headOption.toRight(deliveryNoteNotFound(id)))
        }
        .handleError(error => internal(error).asLeft)

    def create(request: DeliveryNoteRequest): IO[Either[ApiFailure, DeliveryNoteResponse]] =
      request.parse.toEither match
        case Left(errors) => IO.pure(validation(errors).asLeft)
        case Right(parsed) =>
          IO(DeliveryNoteId(UUID.randomUUID().nn))
            .flatMap(id => save(id, parsed, None, DeliveryNoteApp.register))
            .handleError(error => internal(error).asLeft)

    def update(id: UUID, request: DeliveryNoteRequest): IO[Either[ApiFailure, DeliveryNoteResponse]] =
      request.parse.toEither match
        case Left(errors) => IO.pure(validation(errors).asLeft)
        case Right(parsed) =>
          DeliveryNoteApp
            .find(store, DeliveryNoteId(id))
            .flatMap {
              case Some(DeliveryNoteAggregate.Registered(existing)) => save(DeliveryNoteId(id), parsed, Some(existing), DeliveryNoteApp.change)
              case Some(DeliveryNoteAggregate.Cancelled(_, _, _)) => IO.pure(deliveryNoteCancelled.asLeft)
              case Some(DeliveryNoteAggregate.Empty) | None => IO.pure(deliveryNoteNotFound(id).asLeft)
            }
            .handleError(error => internal(error).asLeft)

    /** Dry-runs the cancellation on the materials (a consumed purchase cannot be taken back), then cancels the note and clears its purchases. */
    def cancel(id: UUID, request: CancelDeliveryNoteRequest): IO[Either[ApiFailure, DeliveryNoteResponse]] =
      request.parse.toEither match
        case Left(errors) => IO.pure(validation(errors).asLeft)
        case Right(reason) =>
          val noteId = DeliveryNoteId(id)
          DeliveryNoteApp
            .find(store, noteId)
            .flatMap {
              case Some(DeliveryNoteAggregate.Registered(existing)) =>
                for
                  day <- today
                  stored <- materialsById
                  checked = dryRun(noteId, DesiredDeliveryNote(existing.info.ref, Map.empty), Map.empty, stored, day)
                  result <- checked.toEither match
                    case Left(errors) => IO.pure(validation(errors).asLeft)
                    case Right(()) => written(id, DeliveryNoteApp.cancel(store, noteId, reason))
                yield result
              case Some(DeliveryNoteAggregate.Cancelled(_, _, _)) => IO.pure(deliveryNoteCancelled.asLeft)
              case Some(DeliveryNoteAggregate.Empty) | None => IO.pure(deliveryNoteNotFound(id).asLeft)
            }
            .handleError(error => internal(error).asLeft)

    /** Checks `parsed` as note `id` (changing `existing`, if any) and dry-runs its purchases, then writes it with `command`. */
    private def save(
        id: DeliveryNoteId,
        parsed: ParsedDeliveryNote,
        existing: Option[DeliveryNoteRecord],
        command: (DeliveryNoteApp.Store, DeliveryNoteInfo) => IO[Either[DeliveryNoteError, Unit]],
    ): IO[Either[ApiFailure, DeliveryNoteResponse]] =
      for
        day <- today
        supplier <- SupplierApp.getAny(suppliers, parsed.supplierId)
        stored <- materialsById
        notes <- DeliveryNoteApp.listAll(store)
        checked = check(id, parsed, CheckContext(day, supplier, stored, notes, existing)).andThen { info =>
          val firstLines = parsed.lines.groupMapReduce(_.materialId)(_.n)(_.min(_))
          dryRun(id, DesiredDeliveryNote(info.ref, DeliveryNoteReconciliation.purchaseLines(info)), firstLines, stored, day).as(info)
        }
        result <- checked.toEither match
          case Left(errors) => IO.pure(validation(errors).asLeft)
          case Right(info) => written(id.value, command(store, info))
      yield result

    /** Runs the note command `write`; once accepted, reconciles the purchases of the note and answers with the stored note. */
    private def written(id: UUID, write: IO[Either[DeliveryNoteError, Unit]]): IO[Either[ApiFailure, DeliveryNoteResponse]] =
      write.flatMap {
        case Left(error) => IO.pure(failure(error, id).asLeft)
        case Right(()) => reconciler.reconcile(DeliveryNoteId(id)) >> read(id)
      }

    private def materialsById: IO[Map[MaterialId, StoredMaterial]] =
      MaterialApp.listAll(materials).map(_.map(material => material.info.id -> material).toMap)

    /** The responses of `notes`, reading the materials once and each supplier (active or deleted) once. */
    private def responsesOf(notes: List[DeliveryNoteAggregate]): IO[List[DeliveryNoteResponse]] =
      for
        stored <- materialsById
        found <- MaterialHttpApi.suppliersById(suppliers, notes.flatMap(_.record.toList.map(_.info.supplierId)))
      yield notes.flatMap(DeliveryNoteResponse.fromDomain(_, found, stored))
  end Logic
end DeliveryNoteHttpApi
