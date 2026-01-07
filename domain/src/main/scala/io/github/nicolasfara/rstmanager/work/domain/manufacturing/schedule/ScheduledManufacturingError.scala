package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import io.github.nicolasfara.rstmanager.work.domain.task.schedule.{ScheduledTaskError, ScheduledTaskId}

enum ScheduledManufacturingError derives CanEqual:
  case TaskError(error: ScheduledTaskError)
  case TaskIdNotFound(id: ScheduledTaskId)
  case TaskAddedToCompletedManufacturing
  case TaskRemovedFromCompletedManufacturing
  case ManufacturingWithNoTasks
  case ManufacturingCompleted
  case ManufacturingNotStarted
