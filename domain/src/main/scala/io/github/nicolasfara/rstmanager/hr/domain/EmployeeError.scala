package io.github.nicolasfara.rstmanager.hr.domain

/** Errors raised while deciding or applying employee master-data operations. */
enum EmployeeError derives CanEqual:
  /** A create was attempted on an id that already exists. */
  case EmployeeAlreadyExists

  /** An update or delete was attempted on an id that does not exist (or was deleted). */
  case EmployeeNotFound
end EmployeeError
