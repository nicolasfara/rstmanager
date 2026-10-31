package io.github.nicolasfara.rstmanager.customer.domain

/** Errors raised while deciding or applying customer master-data operations. */
enum CustomerError derives CanEqual:
  /** A create was attempted on an id that already exists. */
  case CustomerAlreadyExists

  /** An update or delete was attempted on an id that does not exist (or was deleted). */
  case CustomerNotFound

  /** A company customer was given no business name. */
  case BusinessNameRequired

  /** The fiscal identifier is not a valid fiscal code (individuals) or VAT number (companies) for the customer type. */
  case TaxCodeDoesNotMatchCustomerType(customerType: CustomerType)

  /** The customer carried in a command does not have the id of the customer stream it was sent to. */
  case CustomerIdMismatch
