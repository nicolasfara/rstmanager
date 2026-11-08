package io.github.nicolasfara.rstmanager.warehouse.domain.material

import io.github.nicolasfara.rstmanager.warehouse.domain.SignedQuantity
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import com.github.nscala_time.time.Imports.LocalDate

/** Errors raised while deciding or applying warehouse material operations. */
enum MaterialError derives CanEqual:
  /** A create was attempted on an id that already exists (active or archived). */
  case MaterialAlreadyExists

  /** An operation was attempted on an id that does not exist. */
  case MaterialNotFound

  /** A change was attempted on an archived material, which only keeps its history. */
  case MaterialIsArchived

  /** The same (supplier, MPN code) pair appears more than once in the material. */
  case DuplicateMpn(supplierId: SupplierId, code: String)

  /** The unit of measure was changed although stock movements were already recorded in the old unit. */
  case UnitChangeWithMovements

  /** The material carried in a command does not have the id of the material stream it was sent to. */
  case MaterialIdMismatch

  /** A purchase names a (supplier, MPN code) pair that is not one of the material's MPNs. */
  case UnknownMpn(supplierId: SupplierId, code: String)

  /** A movement was dated after `today`, the business day of the command. */
  case MovementInFuture(on: LocalDate, today: LocalDate)

  /** A purchase or unload quantity, or an adjustment delta, is zero. */
  case NonPositiveQuantity

  /** A quantity of a material counted in pieces is not a whole number. */
  case FractionalPieces

  /** A unit cost was given for an outgoing adjustment: only stock that comes in can record a cost. */
  case CostOnOutgoingAdjustment

  /** The movement id is already used by another movement of the material. */
  case DuplicateMovement(movementId: MovementId)

  /** The movement would leave the stock below zero on `on` (or a later day), where at most `available` could go out. */
  case InsufficientStock(on: LocalDate, available: SignedQuantity)

  /** The movement would raise the stock beyond the largest quantity a material can hold. */
  case StockLimitExceeded

  /** No movement of the material has this id. */
  case MovementNotFound(movementId: MovementId)

  /** The movement was already voided. */
  case MovementAlreadyVoided(movementId: MovementId)

  /** The movement is an automatic consumption of an order manufacturing: only the reconciliation with the order changes it. */
  case ConsumptionNotVoidable(movementId: MovementId)

  /** The movement is a purchase of a delivery note: it is corrected by changing or cancelling the note, never voided by hand. */
  case DeliveryNoteMovementNotVoidable(movementId: MovementId)
end MaterialError
