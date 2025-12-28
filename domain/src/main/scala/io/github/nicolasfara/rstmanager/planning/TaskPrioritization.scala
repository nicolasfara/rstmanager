package io.github.nicolasfara.rstmanager.planning

import cats.implicits.*
import io.github.nicolasfara.rstmanager.work.domain.task.PlannedTask

object TaskPrioritization:
  def prioritizeTasks(tasks: List[PlannedTask]): List[PlannedTask] = tasks.sorted
