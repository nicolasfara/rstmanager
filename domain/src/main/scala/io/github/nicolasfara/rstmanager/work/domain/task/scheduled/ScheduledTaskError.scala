package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

/** Errors raised while changing the lifecycle of a scheduled task. */
enum ScheduledTaskError derives CanEqual:
  case TaskMustBeInProgress
  case TaskAlreadyCompleted
  case TaskAlreadyInProgress
  case TaskWithNegativeProgress
