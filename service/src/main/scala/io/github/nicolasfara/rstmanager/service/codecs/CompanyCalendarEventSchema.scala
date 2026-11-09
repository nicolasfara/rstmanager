package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.calendar.events.CompanyCalendarEvent

import io.circe.{ Decoder, JsonObject }

/** Persisted schema of the [[CompanyCalendarEvent]] journal (`company_calendar`). It was versioned from its first release: v1 is the only version. */
object CompanyCalendarEventSchema extends DerivedEventSchema[CompanyCalendarEvent](using HrCodecs.companyCalendarEventData):
  val currentVersion: Int = 1
  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = EventSchema.noUpcasters
  def legacyVersion(tag: String, data: JsonObject): Int = 1
