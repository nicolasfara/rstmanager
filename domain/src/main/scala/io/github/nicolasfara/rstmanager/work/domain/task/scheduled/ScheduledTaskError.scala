package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

enum ScheduledTaskError:
  case TaskMustBeInProgress
  case TaskAlreadyCompleted
  case TaskAlreadyInProgress
  case TaskWithNegativeProgress
