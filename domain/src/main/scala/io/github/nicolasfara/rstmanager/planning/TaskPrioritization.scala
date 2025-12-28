package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.work.domain.task.PlannedTask

import cats.implicits.*

object TaskPrioritization:
  def prioritizeTasks(tasks: List[PlannedTask]): List[PlannedTask] = tasks.sorted
