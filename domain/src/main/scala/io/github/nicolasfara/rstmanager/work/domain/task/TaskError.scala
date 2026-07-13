package io.github.nicolasfara.rstmanager.work.domain.task

/** Errors raised while deciding or applying task catalog operations. */
enum TaskError derives CanEqual:
  /** A create was attempted on an id that already exists. */
  case TaskAlreadyExists

  /** An update or delete was attempted on an id that does not exist (or was deleted). */
  case TaskNotFound
