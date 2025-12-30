package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import io.github.nicolasfara.rstmanager.work.domain.task.TaskId

enum ScheduledManufacturingError:
  case ManufacturingWithNoTasks
  case TaskIdNotFound(taskId: TaskId)
