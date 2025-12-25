package io.github.nicolasfara.rstmanager.work.domain.task

final case class Task(id: TaskId, name: TaskName, taskDescription: Option[TaskDescription], requiredHours: Hours)
