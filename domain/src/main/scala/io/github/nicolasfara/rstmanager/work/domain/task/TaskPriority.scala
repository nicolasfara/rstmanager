package io.github.nicolasfara.rstmanager.work.domain.task

import cats.{Order, Show}
import cats.derived.*

enum TaskPriority derives Show, Order:
  case Low
  case Medium
  case High
