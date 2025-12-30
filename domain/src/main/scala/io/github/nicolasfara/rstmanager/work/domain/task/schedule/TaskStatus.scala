package io.github.nicolasfara.rstmanager.work.domain.task.schedule

enum TaskStatus derives CanEqual:
  case NotStarted
  case InProgress
  case Done
