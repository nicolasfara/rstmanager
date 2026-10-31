package io.github.nicolasfara.rstmanager.hr.domain

/** Errors raised while deciding or applying employee master-data operations. */
enum EmployeeError derives CanEqual:
  /** A create was attempted on an id that already exists. */
  case EmployeeAlreadyExists

  /** An update or delete was attempted on an id that does not exist (or was deleted). */
  case EmployeeNotFound

  /** The hours overrides contradict each other: overlapping vacations, a working day inside a vacation, or two overrides for the same day. */
  case ConflictingHoursOverrides

  /** The employee carried in a command does not have the id of the employee stream it was sent to. */
  case EmployeeIdMismatch
