package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.task.CompletableTaskId

enum ManufacturingError:
  case TaskNotFound(taskId: CompletableTaskId)
