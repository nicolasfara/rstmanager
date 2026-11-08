package io.github.nicolasfara.rstmanager.warehouse.domain.material

import io.github.nicolasfara.rstmanager.warehouse.domain.{ BusinessCalendar, Quantity, SignedQuantity, UnitPrice, Valuation }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialAggregate.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialError.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.*
import edomata.syntax.all.*
import io.github.iltotore.iron.:|

/**
 * Event-sourced aggregate holding one warehouse material.
 *
 * Lifecycle:
 *   - [[MaterialAggregate.Empty]] before the material exists.
 *   - [[MaterialAggregate.Active]] holds the current [[MaterialRecord]].
 *   - [[MaterialAggregate.Archived]] keeps the last record and the day it was archived, for the history, while rejecting further changes.
 *
 * Stock movements are decided against the material's [[StockLedger]]: a movement is dated on a business day between the material's creation and the
 * day of the command (`today`), and a manual movement that takes stock out (an unload, an outgoing adjustment, the void of an incoming movement) may
 * not leave the balance below zero on its day or any later one, so backdated movements cannot break the history.
 *
 * Automatic consumptions by order manufacturings ([[MaterialAggregate.setConsumption]]) are the exception: what the order used is recorded even
 * beyond the stock (a negative balance is an anomaly the reports flag), and also on an archived material, since the order may still name it.
 *
 * The purchases of a delivery note ([[MaterialAggregate.setDeliveryNotePurchases]]) are set as a whole: the note brings an absolute list of
 * purchases, recorded again whenever it changes, and only changing (or cancelling) the note changes them.
 */
enum MaterialAggregate derives CanEqual:
  case Empty
  case Active(material: MaterialRecord)
  case Archived(material: MaterialRecord, archivedOn: LocalDate)

  /** Creates the material on business day `on`, provided its data satisfies [[MaterialInfo.checkInvariants]]. */
  def create(info: MaterialInfo, on: LocalDate): Decision[MaterialError, MaterialEvent, MaterialAggregate] = this.decide {
    case Empty => info.checkInvariants.toDecision *> MaterialCreated(info, on).accept
    case _ => Decision.reject(MaterialAlreadyExists)
  }.validate(_.mustBeActive)

  /**
   * Replaces the material data on business day `on`, provided it satisfies [[MaterialInfo.checkInvariants]] and keeps the unit of measure when stock
   * movements exist.
   */
  def update(info: MaterialInfo, on: LocalDate): Decision[MaterialError, MaterialEvent, MaterialAggregate] = this.decide {
    case Active(material) =>
      (
        info.checkInvariants,
        Validated.condNec(info.unit == material.info.unit || !material.hasMovements, (), UnitChangeWithMovements),
      ).tupled.toDecision *> MaterialUpdated(info, on).accept
    case Archived(_, _) => Decision.reject(MaterialIsArchived)
    case Empty => Decision.reject(MaterialNotFound)
  }.validate(_.mustBeActive)

  /** Archives the material on business day `on`. */
  def archive(on: LocalDate): Decision[MaterialError, MaterialEvent, MaterialAggregate] = this.decide {
    case Active(_) => Decision.accept(MaterialArchived(on))
    case Archived(_, _) => Decision.reject(MaterialIsArchived)
    case Empty => Decision.reject(MaterialNotFound)
  }.validate(_.mustBeArchived)

  /**
   * Records the purchase of `quantity` on day `on` under one of the material's MPNs, at `unitCost`; the total cost is computed here (quantity × unit
   * cost, rounded HALF_UP to the cent) and the supplier is copied into the event.
   */
  def recordPurchase(
      movementId: MovementId,
      mpnCode: String :| MpnCode,
      supplierId: SupplierId,
      quantity: Quantity,
      unitCost: UnitPrice,
      note: Option[String],
      on: LocalDate,
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, MaterialAggregate] = this.decide {
    case Active(material) =>
      val kind = MovementKind.Purchase(mpnCode, supplierId, unitCost, Valuation.amount(quantity, unitCost), note, None)
      (
        Validated.condNec(
          material.info.mpns.exists(mpn => mpn.supplierId == supplierId && (mpn.code: String) == (mpnCode: String)),
          (),
          UnknownMpn(supplierId, mpnCode),
        ),
        movementRules(material, movementId, quantity.toSigned, on, today),
        staysWithinLimit(material.ledger.appended(movementId, on, quantity.toSigned, kind), on),
      ).tupled.toDecision *>
        PurchaseRecorded(movementId, mpnCode, supplierId, quantity, unitCost, Valuation.amount(quantity, unitCost), on, note).accept
    case Archived(_, _) => Decision.reject(MaterialIsArchived)
    case Empty => Decision.reject(MaterialNotFound)
  }.validate(_.mustBeActive)

  /** Takes `quantity` out of stock on day `on` for `reason`, provided the stock suffices on that day and every later one. */
  def unload(
      movementId: MovementId,
      quantity: Quantity,
      reason: String :| MovementReason,
      on: LocalDate,
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, MaterialAggregate] = this.decide {
    case Active(material) =>
      val delta = quantity.toSigned.negate
      (
        movementRules(material, movementId, delta, on, today),
        staysNonNegative(material.ledger, material.ledger.appended(movementId, on, delta, MovementKind.Unload(reason)), on),
      ).tupled.toDecision *> StockUnloaded(movementId, quantity, reason, on).accept
    case Archived(_, _) => Decision.reject(MaterialIsArchived)
    case Empty => Decision.reject(MaterialNotFound)
  }.validate(_.mustBeActive)

  /**
   * Corrects the stock by `delta` on day `on` for `reason`. Only an incoming correction may carry a `unitCost` (it then counts as a price point); an
   * outgoing one needs enough stock on that day and every later one.
   */
  def adjust(
      movementId: MovementId,
      delta: SignedQuantity,
      unitCost: Option[UnitPrice],
      reason: String :| MovementReason,
      on: LocalDate,
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, MaterialAggregate] = this.decide {
    case Active(material) =>
      val after = material.ledger.appended(movementId, on, delta, MovementKind.Adjustment(unitCost, reason))
      (
        movementRules(material, movementId, delta, on, today),
        Validated.condNec(delta.value > 0 || unitCost.isEmpty, (), CostOnOutgoingAdjustment),
        if delta.value < 0 then staysNonNegative(material.ledger, after, on) else staysWithinLimit(after, on),
      ).tupled.toDecision *> StockAdjusted(movementId, delta, unitCost, reason, on).accept
    case Archived(_, _) => Decision.reject(MaterialIsArchived)
    case Empty => Decision.reject(MaterialNotFound)
  }.validate(_.mustBeActive)

  /**
   * Voids movement `movementId` on `today` for `reason`: it stops counting for every day. Voiding an incoming movement needs the stock to stay
   * non-negative without it from its day on.
   */
  def voidMovement(
      movementId: MovementId,
      reason: String :| MovementReason,
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, MaterialAggregate] =
    this.decide {
      case Active(material) =>
        material.ledger.find(movementId) match
          case None => Decision.reject(MovementNotFound(movementId))
          case Some(movement) if movement.voided => Decision.reject(MovementAlreadyVoided(movementId))
          case Some(movement) if movement.consumption.isDefined => Decision.reject(ConsumptionNotVoidable(movementId))
          case Some(movement) if movement.deliveryNote.isDefined => Decision.reject(DeliveryNoteMovementNotVoidable(movementId))
          case Some(movement) =>
            val after = material.ledger.voided(movementId, MovementVoid(reason, today))
            val stock = if movement.incoming then staysNonNegative(material.ledger, after, movement.on) else staysWithinLimit(after, movement.on)
            stock.toDecision *> MovementVoided(movementId, reason, today).accept
      case Archived(_, _) => Decision.reject(MaterialIsArchived)
      case Empty => Decision.reject(MaterialNotFound)
    }.validate(_.mustBeActive)

  /**
   * Sets the quantity the order manufacturing `ref` consumes to `quantity` from day `on`, on an active or archived material.
   *
   * The event is absolute and decided only when the quantity changes: setting the current quantity again decides nothing (no event). The stock is not
   * checked, so a consumption beyond it leaves a negative balance (an anomaly). `on` may not be after `today`; a day before the material's creation
   * is moved to the creation day, so the ledger never holds movements from before the material existed (the material was used before the warehouse
   * tracked the material: it still counts, from its first day).
   */
  def setConsumption(
      ref: ConsumptionRef,
      quantity: Quantity,
      on: LocalDate,
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, MaterialAggregate] =
    this.decide {
      case Active(material) => consumption(material, ref, quantity, on, today)
      case Archived(material, _) => consumption(material, ref, quantity, on, today)
      case Empty => Decision.reject(MaterialNotFound)
    }.validate(_.mustExist)

  /**
   * Sets the purchases delivery note `ref` brings to the material to exactly `lines`, on business day `today`.
   *
   * The event is absolute and decided only when something changes: setting the purchases the note already brings (under the same number) decides
   * nothing. On an active material, an MPN of the lines the material does not have yet is added first ([[MaterialEvent.MpnAdded]]), with the cost of
   * its first line as expected unit cost; every line is dated on or before `today`, has a positive quantity (whole for pieces), and keeps the stock
   * within its limit. When the note already brought purchases, the change (fewer, smaller, or moved purchases) may not leave the stock below zero
   * from the earliest day involved on, unless it leaves it no lower than it was. An archived material only lets the note go: an empty list.
   */
  def setDeliveryNotePurchases(
      ref: DeliveryNoteRef,
      lines: List[DeliveryNotePurchaseLine],
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, MaterialAggregate] =
    this.decide {
      case Active(material) => deliveryNotePurchases(material, ref, lines, today)
      case Archived(material, _) =>
        if lines.nonEmpty && !material.ledger.holdsDeliveryNote(ref, lines.map(_.priced)) then Decision.reject(MaterialIsArchived)
        else deliveryNotePurchases(material, ref, lines, today)
      case Empty => Decision.reject(MaterialNotFound)
    }.validate(_.mustExist)

  private def mustExist: ValidatedNec[MaterialError, MaterialAggregate] = this match
    case Active(_) | Archived(_, _) => this.validNec
    case Empty => MaterialNotFound.invalidNec

  private def mustBeActive: ValidatedNec[MaterialError, Active] = this match
    case active: Active => active.validNec
    case Archived(_, _) => MaterialIsArchived.invalidNec
    case Empty => MaterialNotFound.invalidNec

  private def mustBeArchived: ValidatedNec[MaterialError, Archived] = this match
    case archived: Archived => archived.validNec
    case Active(_) | Empty => MaterialNotFound.invalidNec
end MaterialAggregate

/** `DomainModel` instance for the event-sourced [[MaterialAggregate]]. */
object MaterialAggregate extends DomainModel[MaterialAggregate, MaterialEvent, MaterialError]:
  override def initial: MaterialAggregate = Empty

  /** The fold: movements are appended (and voids marked) on an active or archived material alike; the decisions checked every rule beforehand. */
  override def transition: MaterialEvent => MaterialAggregate => ValidatedNec[MaterialError, MaterialAggregate] = {
    case MaterialCreated(info, createdOn) => _ => Active(MaterialRecord.created(info, createdOn)).validNec
    case MaterialUpdated(info, changedOn) => _.mustBeActive.map(active => Active(active.material.updated(info, changedOn)))
    case MaterialArchived(archivedOn) => _.mustBeActive.map(active => Archived(active.material, archivedOn))
    case PurchaseRecorded(movementId, mpnCode, supplierId, quantity, unitCost, totalCost, on, note) =>
      changeLedger(_.appended(movementId, on, quantity.toSigned, MovementKind.Purchase(mpnCode, supplierId, unitCost, totalCost, note, None)))
    case StockUnloaded(movementId, quantity, reason, on) =>
      changeLedger(_.appended(movementId, on, quantity.toSigned.negate, MovementKind.Unload(reason)))
    case StockAdjusted(movementId, delta, unitCost, reason, on) =>
      changeLedger(_.appended(movementId, on, delta, MovementKind.Adjustment(unitCost, reason)))
    case MovementVoided(movementId, reason, voidedOn) => changeLedger(_.voided(movementId, MovementVoid(reason, voidedOn)))
    case ConsumptionSet(ref, quantity, on) => changeLedger(_.consumptionSet(ref, quantity, on))
    case MpnAdded(mpn, _) => _.mustBeActive.map(active => Active(active.material.withMpn(mpn)))
    case DeliveryNotePurchasesSet(ref, purchases, changedOn) => changeLedger(_.deliveryNoteSet(ref, purchases, changedOn))
  }

  /** The consumption decision on `material`: nothing when `ref` already consumes `quantity`, otherwise the event. */
  private def consumption(
      material: MaterialRecord,
      ref: ConsumptionRef,
      quantity: Quantity,
      on: LocalDate,
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, Unit] =
    if material.ledger.consumedBy(ref) == quantity then Decision.unit
    else
      Validated.condNec(!on.isAfter(today), (), MovementInFuture(on, today)).toDecision *>
        ConsumptionSet(ref, quantity, on).accept

  /**
   * The delivery note decision on `material`: nothing when the note already brings `lines`, otherwise the MPNs to add and the absolute event, once
   * every line and the resulting stock satisfy the rules of [[MaterialAggregate.setDeliveryNotePurchases]].
   */
  private def deliveryNotePurchases(
      material: MaterialRecord,
      ref: DeliveryNoteRef,
      lines: List[DeliveryNotePurchaseLine],
      today: LocalDate,
  ): Decision[MaterialError, MaterialEvent, Unit] =
    val purchases = lines.map(_.priced)
    val before = material.ledger
    if before.holdsDeliveryNote(ref, purchases) then Decision.unit
    else
      val after = before.deliveryNoteSet(ref, purchases, today)
      val previous = before.deliveryNotes.get(ref.id).toList.flatMap(_.purchases)
      val from = (previous ++ purchases).map(_.on).minOption(using BusinessCalendar.dayOrdering)
      val missing = lines
        .filter(line => material.info.mpn(line.supplierId, line.mpnCode).isEmpty)
        .distinctBy(line => (line.supplierId, line.mpnCode: String))
        .map(line => Mpn(line.mpnCode, line.supplierId, None, Some(line.unitCost)))
      (
        lines.traverse_(line => lineRules(material, line, today)),
        from.traverse_(day => staysWithinLimit(after, day)),
        if previous.isEmpty then ().validNec else from.traverse_(day => staysNonNegativeOrNoLower(before, after, day)),
      ).tupled.toDecision *>
        missing.traverse_(mpn => MpnAdded(mpn, today).accept) *>
        DeliveryNotePurchasesSet(ref, purchases, today).accept
    end if
  end deliveryNotePurchases

  /** The rules every delivery note line satisfies: a day not after today, a positive quantity, whole pieces. */
  private def lineRules(material: MaterialRecord, line: DeliveryNotePurchaseLine, today: LocalDate): ValidatedNec[MaterialError, Unit] =
    (
      Validated.condNec(!line.on.isAfter(today), (), MovementInFuture(line.on, today)),
      Validated.condNec(line.quantity.value > 0L, (), NonPositiveQuantity),
      Validated.condNec(material.info.unit.allowsFractions || line.quantity.isWhole, (), FractionalPieces),
    ).tupled.void

  /**
   * Stock taken out from `on` by a changed delivery note: the balance `after` the change stays non-negative on `on` and every later day, or at least
   * no lower than it was (a change that brings stock back is never rejected because a consumption left it negative).
   */
  private def staysNonNegativeOrNoLower(before: StockLedger, after: StockLedger, on: LocalDate): ValidatedNec[MaterialError, Unit] =
    val lowest = after.minBalanceFrom(on).value
    val available = before.minBalanceFrom(on)
    Validated.condNec(lowest >= 0L || lowest >= available.value, (), InsufficientStock(on, available))

  private def changeLedger(change: StockLedger => StockLedger): MaterialAggregate => ValidatedNec[MaterialError, MaterialAggregate] = {
    case Active(material) => Active(material.withLedger(change)).validNec
    case Archived(material, archivedOn) => Archived(material.withLedger(change), archivedOn).validNec
    case Empty => MaterialNotFound.invalidNec
  }

  /**
   * The rules every movement satisfies: a day not after today, a non-zero delta, whole pieces, an unused id. A day before creation is allowed: goods
   * received before the material was registered (an earlier delivery note, the stock already on hand) are recorded on their real day.
   */
  private def movementRules(
      material: MaterialRecord,
      movementId: MovementId,
      delta: SignedQuantity,
      on: LocalDate,
      today: LocalDate,
  ): ValidatedNec[MaterialError, Unit] =
    (
      Validated.condNec(!on.isAfter(today), (), MovementInFuture(on, today)),
      Validated.condNec(delta.value != 0L, (), NonPositiveQuantity),
      Validated.condNec(material.info.unit.allowsFractions || delta.isWhole, (), FractionalPieces),
      Validated.condNec(material.ledger.find(movementId).isEmpty, (), DuplicateMovement(movementId)),
    ).tupled.void

  /** Stock taken out from `on`: the balance `after` the movement stays non-negative on `on` and every later day. */
  private def staysNonNegative(before: StockLedger, after: StockLedger, on: LocalDate): ValidatedNec[MaterialError, Unit] =
    Validated.condNec(after.minBalanceFrom(on).value >= 0L, (), InsufficientStock(on, before.minBalanceFrom(on)))

  /** Stock brought in from `on`: the balance `after` the movement stays within the largest quantity a material can hold. */
  private def staysWithinLimit(after: StockLedger, on: LocalDate): ValidatedNec[MaterialError, Unit] =
    Validated.condNec(after.maxBalanceFrom(on).value <= StockLedger.maxBalance.value, (), StockLimitExceeded)
end MaterialAggregate
