package io.github.nicolasfara.rstmanager.work.domain.manufacturing.events

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.Manufacturing

/** Domain events emitted by the manufacturing catalog aggregate. */
enum ManufacturingEvent derives CanEqual:
  /** The manufacturing template was created. */
  case ManufacturingCreated(manufacturing: Manufacturing)

  /** The manufacturing template was replaced with new data. */
  case ManufacturingUpdated(manufacturing: Manufacturing)

  /** The manufacturing template was deleted. */
  case ManufacturingDeleted
