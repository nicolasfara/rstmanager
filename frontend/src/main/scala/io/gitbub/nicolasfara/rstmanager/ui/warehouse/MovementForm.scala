package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.math.BigDecimal.RoundingMode

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ AdjustmentRequest, MaterialResponse, PurchaseRequest, UnloadRequest }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, messages, FieldIssue }
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit

/** The kinds of movement the material sheet records by hand, in the order of its "Carico | Scarico | Rettifica" switch. */
enum MovementKind(val key: String, val label: String) derives CanEqual:
  case Purchase extends MovementKind("purchase", "Carico")
  case Unload extends MovementKind("unload", "Scarico")
  case Adjustment extends MovementKind("adjustment", "Rettifica")

  /** The submit button: "Registra carico" / "Registra scarico" / "Registra rettifica". */
  def submitLabel: String = s"Registra ${label.toLowerCase}"

  /** The confirmation toast after recording on the material `code`: "Carico registrato su TES-001", "Rettifica registrata su TES-001". */
  def recordedText(code: String): String = this match
    case Adjustment => s"Rettifica registrata su $code"
    case _ => s"$label registrato su $code"
end MovementKind

object MovementKind:
  /** The options of the kind switch, as `(key, label)`. */
  val options: List[(String, String)] = values.toList.map(kind => kind.key -> kind.label)

  /** The kind with switch key `key`, if any. */
  def fromKey(key: String): Option[MovementKind] = values.find(_.key == key)

/**
 * Pure (DOM-free) state of the stock movement forms — purchase ("Carico"), unload ("Scarico"), and adjustment ("Rettifica") — with their validation
 * problems (each tied to a [[Field]], so the page shows it under its input and focuses the first one on an invalid submit) and request building.
 *
 * Each state carries what it is validated against (the material unit, its MPNs, and "today"), fixed when the form opens; the page keeps one [[Draft]]
 * in a `Var`, so every rule (the kind switch and its carry-over included) is unit-testable without Laminar. The server re-checks everything (and the
 * stock non-negativity, which needs the full ledger).
 */
object MovementForm:

  /** Decimals of a quantity on the API (thousandths of a unit). */
  val quantityScale: Int = 3

  /** Decimals of a unit cost on the API (ten-thousandths of a euro). */
  val costScale: Int = 4

  /** What a validation problem is about: one input of the movement form (the adjustment's variation is its [[Field.Quantity]]). */
  enum Field derives CanEqual:
    case Mpn
    case Quantity
    case UnitCost
    case Day
    case Reason

  /** The DOM id of the input of `field` (the anchor of its label and of [[FormFocus]]); the ids are shared by the three kinds. */
  def fieldId(field: Field): String = field match
    case Field.Mpn => "movement-mpn"
    case Field.Quantity => "movement-quantity"
    case Field.UnitCost => "movement-cost"
    case Field.Day => "movement-day"
    case Field.Reason => "movement-reason"

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

  private val piecesError = "Il materiale è gestito a unità intere: la quantità deve essere un numero intero"

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

  private def issuesOf(checks: (Field, Either[String, ?])*): List[FieldIssue[Field]] =
    checks.toList.collect { case (field, Left(message)) => FieldIssue(field, message) }

  // ---- Carry-over between kinds ------------------------------------------------------------------

  /**
   * The fields the three forms share, handed to the next form when the "Carico | Scarico | Rettifica" switch changes kind: the quantity as a
   * magnitude (`outgoing` for an unload or a negative correction, so a correction keeps its direction), the unit cost (purchase and correction), the
   * day, and the reason (unload and correction; a purchase note is not a reason, so it is not carried).
   */
  final case class Carry(quantity: String, outgoing: Boolean, unitCost: String, on: String, reason: String) derives CanEqual

  /** `"-2"` → `("2", true)`, `"+3"` → `("3", false)`: the magnitude of a signed text and whether it is negative. */
  private def splitSign(text: String): (String, Boolean) =
    val trimmed = text.trim.nn
    if trimmed.startsWith("-") then (trimmed.drop(1).trim.nn, true)
    else if trimmed.startsWith("+") then (trimmed.drop(1).trim.nn, false)
    else (trimmed, false)

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
  ) derives CanEqual:
    def selectedMpn: Option[MpnOption] = mpns.find(_.key == mpnKey)

    /** True until the user types a quantity or a cost: validation messages stay hidden on a fresh form. */
    def pristine: Boolean = quantity.trim.nn.isEmpty && unitCost.trim.nn.isEmpty

    private def mpnCheck: Either[String, MpnOption] =
      if mpns.isEmpty then Left("Il materiale non ha MPN: aggiungine uno in anagrafica prima di registrare un carico")
      else selectedMpn.toRight("Seleziona l'MPN acquistato")

    def issues: List[FieldIssue[Field]] =
      issuesOf(
        Field.Mpn -> mpnCheck,
        Field.Quantity -> parseQuantity(quantity, unit),
        Field.UnitCost -> parseCost(unitCost),
        Field.Day -> parseDay(on, today),
      )

    def errors: List[String] = issues.messages

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

    def carry: Carry = Carry(quantity.trim.nn, false, unitCost, on, "")

    /** This form with the shared fields of the previous kind (the MPN and the note are its own). */
    def carried(from: Carry): PurchaseState = copy(quantity = from.quantity, unitCost = from.unitCost, on = from.on)
  end PurchaseState

  /** A fresh purchase of `material` dated `today`; a material with a single MPN has it preselected. */
  def purchase(material: MaterialResponse, today: String): PurchaseState =
    val options = mpnOptions(material)
    val preselected = options match
      case only :: Nil => only.key
      case _ => ""
    PurchaseState(material.unit, options, today, preselected, "", "", today, "")

  // ---- Unload ------------------------------------------------------------------------------------

  final case class UnloadState(unit: String, today: String, quantity: String, on: String, reason: String) derives CanEqual:
    def pristine: Boolean = quantity.trim.nn.isEmpty && reason.trim.nn.isEmpty

    def issues: List[FieldIssue[Field]] =
      issuesOf(Field.Quantity -> parseQuantity(quantity, unit), Field.Day -> parseDay(on, today), Field.Reason -> parseReason(reason))

    def errors: List[String] = issues.messages

    def request: Option[UnloadRequest] =
      (for
        q <- parseQuantity(quantity, unit)
        day <- parseDay(on, today)
        why <- parseReason(reason)
      yield UnloadRequest(q, Some(day), why)).toOption

    def carry: Carry = Carry(quantity.trim.nn, true, "", on, reason)

    /** This form with the shared fields of the previous kind (an unload has no cost). */
    def carried(from: Carry): UnloadState = copy(quantity = from.quantity, on = from.on, reason = from.reason)
  end UnloadState

  def unload(material: MaterialResponse, today: String): UnloadState = UnloadState(material.unit, today, "", today, "")

  // ---- Adjustment --------------------------------------------------------------------------------

  final case class AdjustmentState(unit: String, today: String, delta: String, unitCost: String, on: String, reason: String) derives CanEqual:
    def pristine: Boolean = delta.trim.nn.isEmpty && unitCost.trim.nn.isEmpty && reason.trim.nn.isEmpty

    /** A unit cost may be given only for an incoming (positive) correction; otherwise the cost input is disabled and ignored. */
    def costEnabled: Boolean = parseDelta(delta, unit).exists(d => signum(d) > 0)

    /** The optional cost: absent when blank or not allowed. */
    private def costCheck: Either[String, Option[String]] =
      if !costEnabled || unitCost.trim.nn.isEmpty then Right(None) else parseCost(unitCost).map(Some(_))

    def issues: List[FieldIssue[Field]] =
      issuesOf(
        Field.Quantity -> parseDelta(delta, unit),
        Field.UnitCost -> costCheck,
        Field.Day -> parseDay(on, today),
        Field.Reason -> parseReason(reason),
      )

    def errors: List[String] = issues.messages

    def request: Option[AdjustmentRequest] =
      (for
        d <- parseDelta(delta, unit)
        cost <- costCheck
        day <- parseDay(on, today)
        why <- parseReason(reason)
      yield AdjustmentRequest(d, cost, Some(day), why)).toOption

    def carry: Carry =
      val (magnitude, negative) = splitSign(delta)
      Carry(magnitude, negative, unitCost, on, reason)

    /** This form with the shared fields of the previous kind: the quantity becomes a negative variation when it came from an unload. */
    def carried(from: Carry): AdjustmentState =
      val signed = if from.outgoing && from.quantity.nonEmpty then s"-${from.quantity}" else from.quantity
      copy(delta = signed, unitCost = from.unitCost, on = from.on, reason = from.reason)
  end AdjustmentState

  def adjustment(material: MaterialResponse, today: String): AdjustmentState = AdjustmentState(material.unit, today, "", "", today, "")

  // ---- The form of the sheet: one draft of the chosen kind ---------------------------------------

  /** The movement being typed on the material sheet: the state of its current kind. */
  enum Draft derives CanEqual:
    case Purchase(state: PurchaseState)
    case Unload(state: UnloadState)
    case Adjustment(state: AdjustmentState)

    def kind: MovementKind = this match
      case Purchase(_) => MovementKind.Purchase
      case Unload(_) => MovementKind.Unload
      case Adjustment(_) => MovementKind.Adjustment

    /** Every validation problem of the current kind, in field order. */
    def issues: List[FieldIssue[Field]] = this match
      case Purchase(state) => state.issues
      case Unload(state) => state.issues
      case Adjustment(state) => state.issues

    /** The inline message of `field`, if it has a problem. */
    def inlineError(field: Field): Option[String] = issues.messageFor(field)

    /** The fields handed to the next kind by the switch. */
    def carry: Carry = this match
      case Purchase(state) => state.carry
      case Unload(state) => state.carry
      case Adjustment(state) => state.carry

    /** This draft with the shared fields `from` another kind. */
    def carried(from: Carry): Draft = this match
      case Purchase(state) => Purchase(state.carried(from))
      case Unload(state) => Unload(state.carried(from))
      case Adjustment(state) => Adjustment(state.carried(from))

    /** The first input of the form, focused after a movement is recorded: the MPN of a purchase (when it has a choice), the quantity otherwise. */
    def firstField: Field = this match
      case Purchase(state) if state.mpns.nonEmpty => Field.Mpn
      case _ => Field.Quantity
  end Draft

  /** A blank draft of `kind` for `material`, dated `today` (a purchase of a single-MPN material has it preselected). */
  def blank(kind: MovementKind, material: MaterialResponse, today: String): Draft = kind match
    case MovementKind.Purchase => Draft.Purchase(purchase(material, today))
    case MovementKind.Unload => Draft.Unload(unload(material, today))
    case MovementKind.Adjustment => Draft.Adjustment(adjustment(material, today))

  /**
   * The draft after the switch picks `kind`: unchanged when it is already that kind, otherwise a blank form of `kind` with the shared fields typed so
   * far ([[Carry]]).
   */
  def switchKind(draft: Draft, kind: MovementKind, material: MaterialResponse, today: String): Draft =
    if draft.kind == kind then draft else blank(kind, material, today).carried(draft.carry)

  /**
   * What the sheet would lose by leaving: the draft, unless it is still the blank form of its kind (a kind switch alone loses nothing). The page
   * guards on this value, so a blank form never asks before leaving.
   */
  def unsaved(draft: Draft, material: MaterialResponse, today: String): Option[Draft] =
    Option.unless(draft == blank(draft.kind, material, today))(draft)
end MovementForm
