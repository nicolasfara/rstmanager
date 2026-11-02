package io.github.nicolasfara.rstmanager.warehouse.domain.product

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Quantity, SignedQuantity, UnitPrice, Valuation }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.ProductAggregate.*
import io.github.nicolasfara.rstmanager.warehouse.domain.product.ProductError.*
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.*
import edomata.syntax.all.*
import io.github.iltotore.iron.:|

/**
 * Event-sourced aggregate holding one warehouse product.
 *
 * Lifecycle:
 *   - [[ProductAggregate.Empty]] before the product exists.
 *   - [[ProductAggregate.Active]] holds the current [[ProductRecord]].
 *   - [[ProductAggregate.Archived]] keeps the last record and the day it was archived, for the history, while rejecting further changes.
 *
 * Stock movements are decided against the product's [[StockLedger]]: a movement is dated on a business day between the product's creation and the day
 * of the command (`today`), and a manual movement that takes stock out (an unload, an outgoing adjustment, the void of an incoming movement) may not
 * leave the balance below zero on its day or any later one, so backdated movements cannot break the history.
 *
 * Automatic consumptions by order manufacturings ([[ProductAggregate.setConsumption]]) are the exception: what the order used is recorded even beyond
 * the stock (a negative balance is an anomaly the reports flag), and also on an archived product, since the order may still name it.
 */
enum ProductAggregate derives CanEqual:
  case Empty
  case Active(product: ProductRecord)
  case Archived(product: ProductRecord, archivedOn: LocalDate)

  /** Creates the product on business day `on`, provided its data satisfies [[ProductInfo.checkInvariants]]. */
  def create(info: ProductInfo, on: LocalDate): Decision[ProductError, ProductEvent, ProductAggregate] = this.decide {
    case Empty => info.checkInvariants.toDecision *> ProductCreated(info, on).accept
    case _ => Decision.reject(ProductAlreadyExists)
  }.validate(_.mustBeActive)

  /**
   * Replaces the product data on business day `on`, provided it satisfies [[ProductInfo.checkInvariants]] and keeps the unit of measure when stock
   * movements exist.
   */
  def update(info: ProductInfo, on: LocalDate): Decision[ProductError, ProductEvent, ProductAggregate] = this.decide {
    case Active(product) =>
      (
        info.checkInvariants,
        Validated.condNec(info.unit == product.info.unit || !product.hasMovements, (), UnitChangeWithMovements),
      ).tupled.toDecision *> ProductUpdated(info, on).accept
    case Archived(_, _) => Decision.reject(ProductIsArchived)
    case Empty => Decision.reject(ProductNotFound)
  }.validate(_.mustBeActive)

  /** Archives the product on business day `on`. */
  def archive(on: LocalDate): Decision[ProductError, ProductEvent, ProductAggregate] = this.decide {
    case Active(_) => Decision.accept(ProductArchived(on))
    case Archived(_, _) => Decision.reject(ProductIsArchived)
    case Empty => Decision.reject(ProductNotFound)
  }.validate(_.mustBeArchived)

  /**
   * Records the purchase of `quantity` on day `on` under one of the product's MPNs, at `unitCost`; the total cost is computed here (quantity × unit
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
  ): Decision[ProductError, ProductEvent, ProductAggregate] = this.decide {
    case Active(product) =>
      val kind = MovementKind.Purchase(mpnCode, supplierId, unitCost, Valuation.amount(quantity, unitCost), note)
      (
        Validated.condNec(
          product.info.mpns.exists(mpn => mpn.supplierId == supplierId && (mpn.code: String) == (mpnCode: String)),
          (),
          UnknownMpn(supplierId, mpnCode),
        ),
        movementRules(product, movementId, quantity.toSigned, on, today),
        staysWithinLimit(product.ledger.appended(movementId, on, quantity.toSigned, kind), on),
      ).tupled.toDecision *>
        PurchaseRecorded(movementId, mpnCode, supplierId, quantity, unitCost, Valuation.amount(quantity, unitCost), on, note).accept
    case Archived(_, _) => Decision.reject(ProductIsArchived)
    case Empty => Decision.reject(ProductNotFound)
  }.validate(_.mustBeActive)

  /** Takes `quantity` out of stock on day `on` for `reason`, provided the stock suffices on that day and every later one. */
  def unload(
      movementId: MovementId,
      quantity: Quantity,
      reason: String :| MovementReason,
      on: LocalDate,
      today: LocalDate,
  ): Decision[ProductError, ProductEvent, ProductAggregate] = this.decide {
    case Active(product) =>
      val delta = quantity.toSigned.negate
      (
        movementRules(product, movementId, delta, on, today),
        staysNonNegative(product.ledger, product.ledger.appended(movementId, on, delta, MovementKind.Unload(reason)), on),
      ).tupled.toDecision *> StockUnloaded(movementId, quantity, reason, on).accept
    case Archived(_, _) => Decision.reject(ProductIsArchived)
    case Empty => Decision.reject(ProductNotFound)
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
  ): Decision[ProductError, ProductEvent, ProductAggregate] = this.decide {
    case Active(product) =>
      val after = product.ledger.appended(movementId, on, delta, MovementKind.Adjustment(unitCost, reason))
      (
        movementRules(product, movementId, delta, on, today),
        Validated.condNec(delta.value > 0 || unitCost.isEmpty, (), CostOnOutgoingAdjustment),
        if delta.value < 0 then staysNonNegative(product.ledger, after, on) else staysWithinLimit(after, on),
      ).tupled.toDecision *> StockAdjusted(movementId, delta, unitCost, reason, on).accept
    case Archived(_, _) => Decision.reject(ProductIsArchived)
    case Empty => Decision.reject(ProductNotFound)
  }.validate(_.mustBeActive)

  /**
   * Voids movement `movementId` on `today` for `reason`: it stops counting for every day. Voiding an incoming movement needs the stock to stay
   * non-negative without it from its day on.
   */
  def voidMovement(
      movementId: MovementId,
      reason: String :| MovementReason,
      today: LocalDate,
  ): Decision[ProductError, ProductEvent, ProductAggregate] =
    this.decide {
      case Active(product) =>
        product.ledger.find(movementId) match
          case None => Decision.reject(MovementNotFound(movementId))
          case Some(movement) if movement.voided => Decision.reject(MovementAlreadyVoided(movementId))
          case Some(movement) if movement.consumption.isDefined => Decision.reject(ConsumptionNotVoidable(movementId))
          case Some(movement) =>
            val after = product.ledger.voided(movementId, MovementVoid(reason, today))
            val stock = if movement.incoming then staysNonNegative(product.ledger, after, movement.on) else staysWithinLimit(after, movement.on)
            stock.toDecision *> MovementVoided(movementId, reason, today).accept
      case Archived(_, _) => Decision.reject(ProductIsArchived)
      case Empty => Decision.reject(ProductNotFound)
    }.validate(_.mustBeActive)

  /**
   * Sets the quantity the order manufacturing `ref` consumes to `quantity` from day `on`, on an active or archived product.
   *
   * The event is absolute and decided only when the quantity changes: setting the current quantity again decides nothing (no event). The stock is not
   * checked, so a consumption beyond it leaves a negative balance (an anomaly). `on` may not be after `today`; a day before the product's creation is
   * moved to the creation day, so the ledger never holds movements from before the product existed (the material was used before the warehouse
   * tracked the product: it still counts, from its first day).
   */
  def setConsumption(
      ref: ConsumptionRef,
      quantity: Quantity,
      on: LocalDate,
      today: LocalDate,
  ): Decision[ProductError, ProductEvent, ProductAggregate] =
    this.decide {
      case Active(product) => consumption(product, ref, quantity, on, today)
      case Archived(product, _) => consumption(product, ref, quantity, on, today)
      case Empty => Decision.reject(ProductNotFound)
    }.validate(_.mustExist)

  private def mustExist: ValidatedNec[ProductError, ProductAggregate] = this match
    case Active(_) | Archived(_, _) => this.validNec
    case Empty => ProductNotFound.invalidNec

  private def mustBeActive: ValidatedNec[ProductError, Active] = this match
    case active: Active => active.validNec
    case Archived(_, _) => ProductIsArchived.invalidNec
    case Empty => ProductNotFound.invalidNec

  private def mustBeArchived: ValidatedNec[ProductError, Archived] = this match
    case archived: Archived => archived.validNec
    case Active(_) | Empty => ProductNotFound.invalidNec
end ProductAggregate

/** `DomainModel` instance for the event-sourced [[ProductAggregate]]. */
object ProductAggregate extends DomainModel[ProductAggregate, ProductEvent, ProductError]:
  override def initial: ProductAggregate = Empty

  /** The fold: movements are appended (and voids marked) on an active or archived product alike; the decisions checked every rule beforehand. */
  override def transition: ProductEvent => ProductAggregate => ValidatedNec[ProductError, ProductAggregate] = {
    case ProductCreated(info, createdOn) => _ => Active(ProductRecord.created(info, createdOn)).validNec
    case ProductUpdated(info, changedOn) => _.mustBeActive.map(active => Active(active.product.updated(info, changedOn)))
    case ProductArchived(archivedOn) => _.mustBeActive.map(active => Archived(active.product, archivedOn))
    case PurchaseRecorded(movementId, mpnCode, supplierId, quantity, unitCost, totalCost, on, note) =>
      changeLedger(_.appended(movementId, on, quantity.toSigned, MovementKind.Purchase(mpnCode, supplierId, unitCost, totalCost, note)))
    case StockUnloaded(movementId, quantity, reason, on) =>
      changeLedger(_.appended(movementId, on, quantity.toSigned.negate, MovementKind.Unload(reason)))
    case StockAdjusted(movementId, delta, unitCost, reason, on) =>
      changeLedger(_.appended(movementId, on, delta, MovementKind.Adjustment(unitCost, reason)))
    case MovementVoided(movementId, reason, voidedOn) => changeLedger(_.voided(movementId, MovementVoid(reason, voidedOn)))
    case ConsumptionSet(ref, quantity, on) => changeLedger(_.consumptionSet(ref, quantity, on))
  }

  /** The consumption decision on `product`: nothing when `ref` already consumes `quantity`, otherwise the event. */
  private def consumption(
      product: ProductRecord,
      ref: ConsumptionRef,
      quantity: Quantity,
      on: LocalDate,
      today: LocalDate,
  ): Decision[ProductError, ProductEvent, Unit] =
    if product.ledger.consumedBy(ref) == quantity then Decision.unit
    else
      Validated.condNec(!on.isAfter(today), (), MovementInFuture(on, today)).toDecision *>
        ConsumptionSet(ref, quantity, on).accept

  private def changeLedger(change: StockLedger => StockLedger): ProductAggregate => ValidatedNec[ProductError, ProductAggregate] = {
    case Active(product) => Active(product.withLedger(change)).validNec
    case Archived(product, archivedOn) => Archived(product.withLedger(change), archivedOn).validNec
    case Empty => ProductNotFound.invalidNec
  }

  /**
   * The rules every movement satisfies: a day not after today, a non-zero delta, whole pieces, an unused id. A day before creation is allowed: goods
   * received before the product was registered (an earlier delivery note, the stock already on hand) are recorded on their real day.
   */
  private def movementRules(
      product: ProductRecord,
      movementId: MovementId,
      delta: SignedQuantity,
      on: LocalDate,
      today: LocalDate,
  ): ValidatedNec[ProductError, Unit] =
    (
      Validated.condNec(!on.isAfter(today), (), MovementInFuture(on, today)),
      Validated.condNec(delta.value != 0L, (), NonPositiveQuantity),
      Validated.condNec(product.info.unit.allowsFractions || delta.isWhole, (), FractionalPieces),
      Validated.condNec(product.ledger.find(movementId).isEmpty, (), DuplicateMovement(movementId)),
    ).tupled.void

  /** Stock taken out from `on`: the balance `after` the movement stays non-negative on `on` and every later day. */
  private def staysNonNegative(before: StockLedger, after: StockLedger, on: LocalDate): ValidatedNec[ProductError, Unit] =
    Validated.condNec(after.minBalanceFrom(on).value >= 0L, (), InsufficientStock(on, before.minBalanceFrom(on)))

  /** Stock brought in from `on`: the balance `after` the movement stays within the largest quantity a product can hold. */
  private def staysWithinLimit(after: StockLedger, on: LocalDate): ValidatedNec[ProductError, Unit] =
    Validated.condNec(after.maxBalanceFrom(on).value <= StockLedger.maxBalance.value, (), StockLimitExceeded)
end ProductAggregate
