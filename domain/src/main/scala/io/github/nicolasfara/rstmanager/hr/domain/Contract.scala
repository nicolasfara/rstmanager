package io.github.nicolasfara.rstmanager.hr.domain

import java.time.LocalDate

enum Contract:
  case FullTime(startDate: LocalDate)
  case FixedTerm(startDate: LocalDate, endDate: LocalDate)
  case PartTime(startDate: LocalDate, weeklyHours: WeeklyHours)
