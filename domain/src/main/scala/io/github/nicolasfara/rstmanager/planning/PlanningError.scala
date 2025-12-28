package io.github.nicolasfara.rstmanager.planning

import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingCode

enum PlanningError:
  case InsufficientCapacity
  case ConstraintViolation(manufacturingCode: ManufacturingCode, firstAvailableDate: DateTime)
