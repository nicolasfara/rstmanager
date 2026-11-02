package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** The global `Date`, with the `toLocaleDateString(locales, options)` overload the `js.Date` facade does not expose. */
@js.native
@JSGlobal("Date")
private class IntlDate(@annotation.unused epochMillis: Double) extends js.Object:
  def toLocaleDateString(locales: String, options: js.Object): String = js.native

/**
 * The business day of the warehouse in the browser: the calendar day in `Europe/Rome`, the company's time zone, whatever the zone of the browser. It
 * is the day the server takes as "today" for movements and reports (`BusinessCalendar`), so the defaults and upper bounds of the date inputs match
 * the server's checks also around midnight and for users abroad.
 */
object BusinessDay:
  /** The company's time zone. */
  val timeZone: String = "Europe/Rome"

  /**
   * The business day of the instant `epochMillis` as `YYYY-MM-DD`, through `Intl` (the `en-CA` locale writes dates in that shape); `None` if the
   * runtime writes something else.
   */
  def of(epochMillis: Double): Option[String] =
    val options = js.Dynamic.literal(timeZone = timeZone, year = "numeric", month = "2-digit", day = "2-digit")
    val text = new IntlDate(epochMillis).toLocaleDateString("en-CA", options)
    IsoDay.parse(text).map(IsoDay.format)

  /** Today in the business calendar; the browser's own calendar day only if the runtime cannot convert time zones. */
  def today(): String =
    val now = js.Date.now()
    of(now).getOrElse(localDay(new js.Date(now)))

  private def localDay(date: js.Date): String =
    IsoDay.format(IsoDay.Ymd(date.getFullYear().toInt, date.getMonth().toInt + 1, date.getDate().toInt))
end BusinessDay
