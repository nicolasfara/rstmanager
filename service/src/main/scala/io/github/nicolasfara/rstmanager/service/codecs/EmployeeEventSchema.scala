package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent

import io.circe.{ Decoder, JsonObject }

/**
 * Persisted schema of the [[EmployeeEvent]] journal (`employees_journal`). v1 is the only version: rows written before calendar days became
 * `LocalDate` hold date-times and joda vacation intervals, which [[HrCodecs]] reads back as days.
 */
object EmployeeEventSchema extends DerivedEventSchema[EmployeeEvent](using HrCodecs.employeeEventData):
  val currentVersion: Int = 1
  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = EventSchema.noUpcasters
  def legacyVersion(tag: String, data: JsonObject): Int = 1
