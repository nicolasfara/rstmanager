package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent
import io.github.nicolasfara.rstmanager.service.codecs.HrCodecs.given

class EmployeeEventSchemaTest
    extends JournalGoldenSuite[EmployeeEvent]("employee-events", EmployeeEventSchema, JournalGoldenSuite.caseNames[EmployeeEvent]):
  def current: List[String] = List("v1-employee-created", "v1-employee-updated", "v1-employee-deleted")

  /** A pre-version row written before calendar days became `LocalDate`: date-times and a joda vacation interval. */
  def legacy: List[(String, String)] = List("legacy-v1-employee-created-datetimes" -> "v1-employee-created")
