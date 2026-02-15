package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ScheduledTaskError, ScheduledTaskId}

enum ScheduledManufacturingError derives CanEqual:
  case TaskError(error: ScheduledTaskError)
  case TaskIdNotFound(id: ScheduledTaskId)
  case ManufacturingWithNoTasks
