package io.github.nicolasfara.rstmanager.work.domain.task.schedule

enum ScheduledTaskError:
  case TaskMustBeInProgress
  case TaskAlreadyCompleted
  case TaskAlreadyInProgress
