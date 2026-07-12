package io.github.nicolasfara.rstmanager.customer.domain

/** Errors raised while deciding or applying customer master-data operations. */
enum CustomerError derives CanEqual:
  /** A create was attempted on an id that already exists. */
  case CustomerAlreadyExists

  /** An update or delete was attempted on an id that does not exist (or was deleted). */
  case CustomerNotFound
end CustomerError
