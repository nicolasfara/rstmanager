package io.github.nicolasfara.rstmanager.hr.domain

import com.github.nscala_time.time.Imports.*

enum Contract:
  case FullTime(startDate: DateTime)
  case FixedTerm(startDate: DateTime, endDate: DateTime)
  case PartTime(startDate: DateTime, weeklyHours: WeeklyHours)
