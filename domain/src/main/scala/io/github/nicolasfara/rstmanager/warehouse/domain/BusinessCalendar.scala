package io.github.nicolasfara.rstmanager.warehouse.domain

import java.time.Instant

import com.github.nscala_time.time.Imports.{ DateTime, DateTimeZone, LocalDate }

/**
 * Business days of the warehouse: the calendar day an instant falls on in the company's fixed time zone (`Europe/Rome`), whatever the zone of the
 * server or of the browser.
 *
 * Warehouse decisions take their day from the command time (`context.command.time`), never from the clock, so replaying a journal is deterministic.
 */
object BusinessCalendar:
  /** The company's time zone. */
  val zone: DateTimeZone = DateTimeZone.forID("Europe/Rome")

  /** The business day `instant` falls on. */
  def day(instant: Instant): LocalDate = new LocalDate(instant.toEpochMilli, zone)

  /** The business day `dateTime` falls on, whatever the zone it is expressed in. */
  def day(dateTime: DateTime): LocalDate = new LocalDate(dateTime.getMillis, zone)

  /** Chronological order of business days. */
  given dayOrdering: Ordering[LocalDate] = Ordering.fromLessThan(_.isBefore(_))
