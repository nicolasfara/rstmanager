package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import io.github.nicolasfara.rstmanager.work.domain.task.TaskId

enum ScheduledManufacturingError derives CanEqual:
  case ManufacturingWithNoTasks
  case TaskIdNotFound(taskId: TaskId)
