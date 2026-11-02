package io.github.nicolasfara.rstmanager.warehouse.domain.supplier

/** Errors raised while deciding or applying supplier master-data operations. */
enum SupplierError derives CanEqual:
  /** A create was attempted on an id that already exists (active or deleted). */
  case SupplierAlreadyExists

  /** An update or delete was attempted on an id that does not exist (or was deleted). */
  case SupplierNotFound

  /** The supplier carried in a command does not have the id of the supplier stream it was sent to. */
  case SupplierIdMismatch
