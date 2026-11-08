package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.service.http.ApiError
import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, BusinessCalendar, SignedQuantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialError, MaterialInfo, StockLedger, StockMovement }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.data.{ NonEmptyChain, ValidatedNec }
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import sttp.model.StatusCode

/**
 * What the warehouse endpoints share: Italian error payloads (the warehouse UI shows them as they are), the business day of a request, the strict
 * `YYYY-MM-DD` day format of requests and responses, and the Italian way of writing quantities and amounts in messages.
 *
 * Every validation detail is `Etichetta: problema`, the label being the one the warehouse UI gives the field (`Quantità`, `Data`, `Dal`, …).
 */
object WarehouseHttpSupport:
  import ApiError.ApiFailure

  /** How the warehouse names the order of an automatic consumption: its number and the template code of each of its manufacturings. */
  final case class OrderLabel(number: String, manufacturingCodes: Map[ScheduledManufacturingId, String]) derives CanEqual

  object OrderLabel:
    /** The label of `order`, when it has data. */
    def of(order: Order): Option[OrderLabel] =
      order.orderData.map(data => OrderLabel(data.number, data.setOfManufacturing.toList.map(m => m.info.id -> (m.info.code: String)).toMap))

  /** The labels of the orders the consumptions among `movements` belong to; an order that cannot be loaded is left out. */
  def orderLabels(orders: OrderApp.Store, movements: List[StockMovement]): IO[Map[OrderId, OrderLabel]] =
    movements
      .flatMap(_.consumption.map(_.orderId))
      .distinct
      .traverse(id => OrderApp.get(orders, id).map(_.flatMap(OrderLabel.of).map(id -> _)))
      .map(_.flatten.toMap)

  /** Longest period a report may cover, so a malformed request cannot ask for centuries of buckets. */
  val maxReportYears: Int = 5

  /** A request whose content is not valid, listing every problem (`Etichetta: problema`). */
  def validation(errors: NonEmptyChain[String]): ApiFailure =
    StatusCode.UnprocessableEntity -> ApiError("invalid-request", "La richiesta non è valida.", errors.toChain.toList)

  def notFound(message: String, details: List[String]): ApiFailure = StatusCode.NotFound -> ApiError("not-found", message, details)

  def materialNotFound(id: UUID): ApiFailure = notFound("Il materiale richiesto non esiste.", List(id.toString))

  def conflict(code: String, message: String, details: List[String]): ApiFailure = ApiError.conflict(code, message, details)

  def internal(error: Throwable): ApiFailure =
    StatusCode.InternalServerError -> ApiError("internal-error", "Errore imprevisto del servizio di magazzino.", List(error.toString))

  /** The business day of the current instant: the "today" of a request. */
  def today: IO[LocalDate] = IO(BusinessCalendar.day(Instant.now().nn))

  /** A day as the API writes it: `YYYY-MM-DD`. */
  def formatDay(day: LocalDate): String = day.toString

  /** A day as the messages write it: `DD/MM/YYYY`. */
  def displayDay(day: LocalDate): String = day.toString("dd/MM/yyyy")

  /** Italian short name of a unit of measure, as the UI writes it: `pz`, `m`, `m²`, `kg`. */
  def unitShortLabel(unit: UnitOfMeasure): String = unit match
    case UnitOfMeasure.Pieces => "pz"
    case UnitOfMeasure.LinearMeters => "m"
    case UnitOfMeasure.SquareMeters => "m²"
    case UnitOfMeasure.Kilograms => "kg"

  /** A quantity as the messages write it: Italian separators, no trailing decimal zeros (`2500` is `2,5`, `1234000` is `1.234`). */
  def displayQuantity(quantity: SignedQuantity): String = italianDecimal(quantity.value, SignedQuantity.scale, 0)

  /** A quantity followed by the short name of its unit (`2,5 m`). */
  def displayQuantity(quantity: SignedQuantity, unit: UnitOfMeasure): String = s"${displayQuantity(quantity)} ${unitShortLabel(unit)}"

  /** An amount as the messages write it: `1.234,50 €`. */
  def displayEuro(amount: Amount): String = s"${italianDecimal(amount.value, Amount.scale, 2)} €"

  /** A unit price as the messages write it: at least 2 decimals, up to 4 (`3,70 €`, `3,7512 €`). */
  def displayUnitPrice(price: UnitPrice): String = s"${italianDecimal(price.value, UnitPrice.scale, 2)} €"

  /**
   * A count of `10^-scale` units written the Italian way: thousands grouped by `.`, decimals after `,`, trailing decimal zeros dropped down to
   * `minDecimals`.
   */
  private def italianDecimal(units: Long, scale: Int, minDecimals: Int): String =
    val magnitude = BigInt(units).abs
    val divisor = BigInt(10).pow(scale)
    val integer = (magnitude / divisor).toString.reverse.grouped(3).mkString(".").reverse
    val fraction = (magnitude % divisor).toString.reverse.padTo(scale, '0').reverse
    val shown = fraction.reverse.dropWhile(_ == '0').reverse.padTo(minDecimals, '0')
    val sign = if units < 0 then "-" else ""
    if shown.isEmpty then s"$sign$integer" else s"$sign$integer,$shown"

  /** Parses a day written exactly as `YYYY-MM-DD`, reporting a problem as `label: problem`. */
  def parseDay(raw: String, field: String): ValidatedNec[String, LocalDate] =
    val trimmed = raw.trim.nn
    Either
      .catchNonFatal(LocalDate.parse(trimmed))
      .toOption
      .filter(_ => trimmed.matches("""\d{4}-\d{2}-\d{2}"""))
      .toValidNec(s"$field: '$raw' non è una data valida (usa il formato AAAA-MM-GG)")

  /** Parses an optional day; absent or blank is `None`. */
  def parseOptionalDay(raw: Option[String], field: String): ValidatedNec[String, Option[LocalDate]] =
    raw.map(_.trim.nn).filter(_.nonEmpty).traverse(parseDay(_, field))

  /** A report period: both ends present, `from` not after `to`, at most [[maxReportYears]] years. */
  def parsePeriod(from: Option[String], to: Option[String]): ValidatedNec[String, (LocalDate, LocalDate)] =
    (
      from.toValidNec("Dal: la data di inizio è obbligatoria").andThen(parseDay(_, "Dal")),
      to.toValidNec("Al: la data di fine è obbligatoria").andThen(parseDay(_, "Al")),
    ).tupled.andThen((start, end) =>
      if start.isAfter(end) then periodReversed(start, end).invalidNec
      else if !end.isBefore(start.plusYears(maxReportYears)) then s"Al: il periodo non può superare $maxReportYears anni".invalidNec
      else (start, end).validNec,
    )

  /** The problem of a period whose first day `start` comes after its last day `end`. */
  def periodReversed(start: LocalDate, end: LocalDate): String =
    s"Dal: la data di inizio (${displayDay(start)}) è successiva a quella di fine (${displayDay(end)})"

  /** A quantity of `material` in messages, with its unit when the material is known. */
  private def materialQuantity(quantity: SignedQuantity, material: Option[MaterialInfo]): String =
    material.fold(displayQuantity(quantity))(info => displayQuantity(quantity, info.unit))

  /** How a material is named in messages: its internal code, or a generic name when it is not known. */
  def materialLabel(material: Option[MaterialInfo]): String = material.fold("richiesto")(info => info.internalCode: String)

  /** The Italian error for a rejected material decision; `material` (when loaded) names it in the message. */
  def materialFailure(error: MaterialError, id: UUID, material: Option[MaterialInfo]): ApiFailure =
    val label = materialLabel(material)
    error match
      case MaterialError.MaterialAlreadyExists => conflict("material-already-exists", "Esiste già un materiale con questo id.", Nil)
      case MaterialError.MaterialNotFound => materialNotFound(id)
      case MaterialError.MaterialIsArchived =>
        conflict("material-archived", s"Il materiale $label è archiviato e non può più essere modificato.", Nil)
      case MaterialError.DuplicateMpn(_, code) => validation(NonEmptyChain.one(duplicateMpnMessage(code)))
      case MaterialError.UnitChangeWithMovements =>
        conflict(
          "material-unit-locked",
          s"L'unità di misura del materiale $label non può cambiare: sono già registrati movimenti di magazzino.",
          Nil,
        )
      case MaterialError.MaterialIdMismatch =>
        conflict("material-id-mismatch", "L'id del materiale non corrisponde al materiale indicato nel percorso.", Nil)
      case MaterialError.UnknownMpn(_, code) =>
        validation(NonEmptyChain.one(s"MPN: l'MPN $code del fornitore indicato non è fra gli MPN del materiale $label"))
      case MaterialError.MovementInFuture(on, today) =>
        validation(NonEmptyChain.one(s"Data: la data ${displayDay(on)} è successiva a oggi (${displayDay(today)})"))
      case MaterialError.NonPositiveQuantity => validation(NonEmptyChain.one("Quantità: la quantità deve essere diversa da zero"))
      case MaterialError.FractionalPieces =>
        validation(NonEmptyChain.one(s"Quantità: il materiale $label è gestito a pezzi, la quantità deve essere un numero intero"))
      case MaterialError.CostOnOutgoingAdjustment =>
        validation(NonEmptyChain.one("Costo unitario: il costo unitario si può indicare solo per una rettifica in aumento"))
      case MaterialError.DuplicateMovement(_) => conflict("movement-already-exists", "Esiste già un movimento con questo id.", Nil)
      case MaterialError.InsufficientStock(on, available) =>
        conflict(
          "insufficient-stock",
          s"Giacenza insufficiente per il materiale $label: dal ${displayDay(on)} in poi la giacenza non può scendere sotto zero.",
          List(s"Disponibile dal ${displayDay(on)}: ${materialQuantity(available, material)}"),
        )
      case MaterialError.StockLimitExceeded =>
        conflict(
          "stock-limit-exceeded",
          s"La giacenza del materiale $label supererebbe il massimo consentito (${materialQuantity(StockLedger.maxBalance, material)}).",
          Nil,
        )
      case MaterialError.MovementNotFound(movementId) =>
        notFound(s"Il movimento richiesto non esiste per il materiale $label.", List(movementId.value.toString))
      case MaterialError.MovementAlreadyVoided(_) => conflict("movement-already-voided", "Il movimento è già stato annullato.", Nil)
      case MaterialError.ConsumptionNotVoidable(_) =>
        conflict(
          "consumption-not-voidable",
          "Il movimento è uno scarico automatico di una lavorazione d'ordine e non si può annullare a mano: " +
            "riapri la lavorazione o modificane i materiali, il magazzino si riallinea da solo.",
          Nil,
        )
    end match
  end materialFailure

  def duplicateMpnMessage(code: String): String = s"MPN: il codice MPN $code compare più volte per lo stesso fornitore"
end WarehouseHttpSupport
