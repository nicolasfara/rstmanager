package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.calendar.events.CompanyCalendarEvent
import io.github.nicolasfara.rstmanager.service.codecs.HrCodecs.given

class CompanyCalendarEventSchemaTest
    extends JournalGoldenSuite[CompanyCalendarEvent](
      "company-calendar-events",
      CompanyCalendarEventSchema,
      JournalGoldenSuite.caseNames[CompanyCalendarEvent],
    ):
  def current: List[String] = List("v1-closure-added", "v1-closure-added-annual", "v1-closure-updated", "v1-closure-removed")

  /** The company calendar journal was versioned from its first release: there are no legacy rows. */
  def legacy: List[(String, String)] = Nil
