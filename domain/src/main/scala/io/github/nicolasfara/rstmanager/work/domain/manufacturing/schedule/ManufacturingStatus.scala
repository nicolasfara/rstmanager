package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

enum ManufacturingStatus derives CanEqual:
  case NotStarted
  case InProgress
  case Done
