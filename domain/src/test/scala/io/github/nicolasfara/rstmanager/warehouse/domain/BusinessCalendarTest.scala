package io.github.nicolasfara.rstmanager.warehouse.domain

import java.time.Instant

import com.github.nscala_time.time.Imports.{ DateTime, DateTimeZone, LocalDate }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class BusinessCalendarTest extends AnyFlatSpecLike:
  "BusinessCalendar.day" should "take the calendar day in Europe/Rome, not in UTC" in:
    // 23:30 UTC in summer is 01:30 of the next day in Rome (UTC+2).
    BusinessCalendar.day(Instant.parse("2026-06-14T23:30:00Z").nn) shouldEqual LocalDate.parse("2026-06-15")
    // 23:30 UTC in winter is 00:30 of the next day in Rome (UTC+1).
    BusinessCalendar.day(Instant.parse("2026-01-14T23:30:00Z").nn) shouldEqual LocalDate.parse("2026-01-15")
    BusinessCalendar.day(Instant.parse("2026-01-14T22:59:59Z").nn) shouldEqual LocalDate.parse("2026-01-14")

  it should "give the same day for a date-time whatever zone it is expressed in" in:
    val instant = Instant.parse("2026-06-14T23:30:00Z").nn
    val inNewYork = new DateTime(instant.toEpochMilli, DateTimeZone.forID("America/New_York"))
    BusinessCalendar.day(inNewYork) shouldEqual BusinessCalendar.day(instant)
