package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingCode

import com.github.nscala_time.time.Imports.DateTime

/** Errors produced while computing a production plan or schedule. */
enum PlanningError:
  /** Returned when no feasible capacity is available for the requested planning window. */
  case InsufficientCapacity

  /** Returned when a manufacturing constraint delays the earliest feasible start date. */
  case ConstraintViolation(manufacturingCode: ManufacturingCode, firstAvailableDate: DateTime)
