package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.work.domain.task.schedule.SchedulableTaskId

enum OrderError:
  case OrderWithNoManufacturing
  case TaskNotFound(id: SchedulableTaskId)
