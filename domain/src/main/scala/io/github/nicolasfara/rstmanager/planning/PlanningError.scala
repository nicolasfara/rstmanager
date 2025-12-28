package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingCode

import com.github.nscala_time.time.Imports.DateTime

enum PlanningError:
  case InsufficientCapacity
  case ConstraintViolation(manufacturingCode: ManufacturingCode, firstAvailableDate: DateTime)
