package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.math.BigDecimal.RoundingMode

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ AdjustmentRequest, MaterialResponse, PurchaseRequest, UnloadRequest }
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit

/**
 * Pure (DOM-free) state of the stock movement forms — purchase ("Carico"), unload ("Scarico"), and adjustment ("Rettifica") — with their validation
 * messages and request building.
 *
 * Each state carries what it is validated against (the material unit, its MPNs, and "today"), fixed when the form opens, so the page only keeps one
 * state in a `Var` and every rule is unit-testable without Laminar. The server re-checks everything (and the stock non-negativity, which needs the
 * full ledger).
 */
object MovementForm:

  /** Decimals of a quantity on the API (thousandths of a unit). */
  val quantityScale: Int = 3

  /** Decimals of a unit cost on the API (ten-thousandths of a euro). */
  val costScale: Int = 4

  /** An MPN the material can be bought as: `key` identifies it in the select, `label` shows code and supplier. */
  final case class MpnOption(key: String, code: String, supplierId: UUID, label: String) derives CanEqual

  /** The purchase options of `material`: one per MPN, labelled `CODE — Supplier` (a deleted supplier is marked). */
  def mpnOptions(material: MaterialResponse): List[MpnOption] =
    material.mpns.map { mpn =>
      val supplierName = material.suppliers.find(_.id == mpn.supplierId) match
        case Some(supplier) if supplier.active => supplier.name
        case Some(supplier) => s"${supplier.name} (eliminato)"
        case None => mpn.supplierId.toString.take(8)
      MpnOption(s"${mpn.supplierId}|${mpn.code}", mpn.code, mpn.supplierId, s"${mpn.code} — $supplierName")
    }

  // ---- Shared field rules ------------------------------------------------------------------------

  private def isPieces(unit: String): Boolean = MeasureUnit.fromWire(unit).exists(!_.allowsFractions)

  /** True when a canonical decimal string has no fractional part (`"3.000"`). */
  def isIntegral(canonical: String): Boolean = canonical.dropWhile(_ != '.').drop(1).forall(_ == '0')

  private def signum(canonical: String): Int = BigDecimal(canonical).signum

  private val piecesError = "Il materiale è gestito a pezzi: la quantità deve essere un numero intero"

  /** A positive quantity in `unit` as the API expects it (`"2,5"` → `"2.500"`), or the message to show. */
  def parseQuantity(text: String, unit: String): Either[String, String] =
    if text.trim.nn.isEmpty then Left("Quantità obbligatoria")
    else
      WarehouseFormats.parseDecimal(text, quantityScale) match
        case None => Left("Quantità non valida (es. 2,5, al massimo 3 decimali)")
        case Some(quantity) if signum(quantity) <= 0 => Left("La quantità deve essere maggiore di zero")
        case Some(quantity) if isPieces(unit) && !isIntegral(quantity) => Left(piecesError)
        case Some(quantity) => Right(quantity)

  /** A non-zero signed quantity in `unit` (`"-2"` → `"-2.000"`), or the message to show. */
  def parseDelta(text: String, unit: String): Either[String, String] =
    if text.trim.nn.isEmpty then Left("Variazione obbligatoria")
    else
      WarehouseFormats.parseDecimal(text, quantityScale, true) match
        case None => Left("Variazione non valida (es. -2,5 oppure 10, al massimo 3 decimali)")
        case Some(delta) if signum(delta) == 0 => Left("La variazione non può essere zero")
        case Some(delta) if isPieces(unit) && !isIntegral(delta) => Left(piecesError)
        case Some(delta) => Right(delta)

  /** A unit cost (`"3,5"` → `"3.5000"`), or the message to show. */
  def parseCost(text: String): Either[String, String] =
    if text.trim.nn.isEmpty then Left("Costo unitario obbligatorio")
    else WarehouseFormats.parseDecimal(text, costScale).toRight("Costo unitario non valido (es. 3,50, al massimo 4 decimali)")

  /** The movement day (`YYYY-MM-DD`, not after `today`), or the message to show. */
  def parseDay(text: String, today: String): Either[String, String] =
    if text.trim.nn.isEmpty then Left("Data obbligatoria")
    else
      (IsoDay.parse(text), IsoDay.parse(today)) match
        case (None, _) => Left("Data non valida")
        case (Some(day), Some(now)) if IsoDay.compare(day, now) > 0 => Left("La data non può essere successiva a oggi")
        case (Some(day), _) => Right(IsoDay.format(day))

  private def parseReason(text: String): Either[String, String] =
    Some(text.trim.nn).filter(_.nonEmpty).toRight("Motivo obbligatorio")

  private def opt(text: String): Option[String] = Some(text.trim.nn).filter(_.nonEmpty)

  private def errorsOf(checks: Either[String, ?]*): List[String] = checks.toList.collect { case Left(error) => error }

  /** `quantity × unitCost` (both canonical) in euro, rounded HALF_UP to the cent like the server's valuation: `"2.500"`, `"3.3333"` → `"8.33"`. */
  def total(quantity: String, unitCost: String): String =
    (BigDecimal(quantity) * BigDecimal(unitCost)).setScale(2, RoundingMode.HALF_UP).bigDecimal.toPlainString.nn

  // ---- Purchase ----------------------------------------------------------------------------------

  final case class PurchaseState(
      unit: String,
      mpns: List[MpnOption],
      today: String,
      mpnKey: String,
      quantity: String,
      unitCost: String,
      on: String,
      note: String,
  ):
    def selectedMpn: Option[MpnOption] = mpns.find(_.key == mpnKey)

    /** True until the user types a quantity or a cost: validation messages stay hidden on a fresh form. */
    def pristine: Boolean = quantity.trim.nn.isEmpty && unitCost.trim.nn.isEmpty

    private def mpnCheck: Either[String, MpnOption] =
      if mpns.isEmpty then Left("Il materiale non ha MPN: aggiungine uno in anagrafica prima di registrare un carico")
      else selectedMpn.toRight("Seleziona l'MPN acquistato")

    def errors: List[String] = errorsOf(mpnCheck, parseQuantity(quantity, unit), parseCost(unitCost), parseDay(on, today))

    /** The purchase total shown while typing, once quantity and cost are both valid. */
    def totalPreview: Option[String] =
      for
        q <- parseQuantity(quantity, unit).toOption
        c <- parseCost(unitCost).toOption
      yield total(q, c)

    def request: Option[PurchaseRequest] =
      (for
        mpn <- mpnCheck
        q <- parseQuantity(quantity, unit)
        c <- parseCost(unitCost)
        day <- parseDay(on, today)
      yield PurchaseRequest(mpn.code, mpn.supplierId, q, c, Some(day), opt(note))).toOption
  end PurchaseState

  /** A fresh purchase of `material` dated `today`; a material with a single MPN has it preselected. */
  def purchase(material: MaterialResponse, today: String): PurchaseState =
    val options = mpnOptions(material)
    val preselected = options match
      case only :: Nil => only.key
      case _ => ""
    PurchaseState(material.unit, options, today, preselected, "", "", today, "")

  // ---- Unload ------------------------------------------------------------------------------------

  final case class UnloadState(unit: String, today: String, quantity: String, on: String, reason: String):
    def pristine: Boolean = quantity.trim.nn.isEmpty && reason.trim.nn.isEmpty

    def errors: List[String] = errorsOf(parseQuantity(quantity, unit), parseDay(on, today), parseReason(reason))

    def request: Option[UnloadRequest] =
      (for
        q <- parseQuantity(quantity, unit)
        day <- parseDay(on, today)
        why <- parseReason(reason)
      yield UnloadRequest(q, Some(day), why)).toOption

  def unload(material: MaterialResponse, today: String): UnloadState = UnloadState(material.unit, today, "", today, "")

  // ---- Adjustment --------------------------------------------------------------------------------

  final case class AdjustmentState(unit: String, today: String, delta: String, unitCost: String, on: String, reason: String):
    def pristine: Boolean = delta.trim.nn.isEmpty && unitCost.trim.nn.isEmpty && reason.trim.nn.isEmpty

    /** A unit cost may be given only for an incoming (positive) correction; otherwise the cost input is disabled and ignored. */
    def costEnabled: Boolean = parseDelta(delta, unit).exists(d => signum(d) > 0)

    /** The optional cost: absent when blank or not allowed. */
    private def costCheck: Either[String, Option[String]] =
      if !costEnabled || unitCost.trim.nn.isEmpty then Right(None) else parseCost(unitCost).map(Some(_))

    def errors: List[String] = errorsOf(parseDelta(delta, unit), costCheck, parseDay(on, today), parseReason(reason))

    def request: Option[AdjustmentRequest] =
      (for
        d <- parseDelta(delta, unit)
        cost <- costCheck
        day <- parseDay(on, today)
        why <- parseReason(reason)
      yield AdjustmentRequest(d, cost, Some(day), why)).toOption
  end AdjustmentState

  def adjustment(material: MaterialResponse, today: String): AdjustmentState = AdjustmentState(material.unit, today, "", "", today, "")
end MovementForm
