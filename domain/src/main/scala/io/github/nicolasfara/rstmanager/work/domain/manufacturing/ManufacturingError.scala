package io.github.nicolasfara.rstmanager.work.domain.manufacturing

/** Errors raised while deciding or applying manufacturing catalog operations. */
enum ManufacturingError derives CanEqual:
  /** A create was attempted on an id that already exists. */
  case ManufacturingAlreadyExists

  /** An update or delete was attempted on an id that does not exist (or was deleted). */
  case ManufacturingNotFound
