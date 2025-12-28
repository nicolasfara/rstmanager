package io.github.nicolasfara.rstmanager.work.domain.task

import cats.derived.*
import cats.{Order, Show}

enum TaskPriority derives Show, Order:
  case Low
  case Medium
  case High
