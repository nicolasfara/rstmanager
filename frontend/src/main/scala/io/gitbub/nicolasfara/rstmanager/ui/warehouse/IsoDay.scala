package io.gitbub.nicolasfara.rstmanager.ui.warehouse

/**
 * Pure (DOM-free) calendar arithmetic on the `YYYY-MM-DD` days the warehouse API exchanges: proleptic Gregorian, no time of day, no time zone.
 *
 * The frontend has no `java.time`, and `js.Date` arithmetic drifts across daylight-saving changes, so days are converted to a count of days since
 * 1970-01-01 ("epoch day") and back with the civil-calendar algorithm by H. Hinnant.
 */
object IsoDay:

  /** A valid calendar day. */
  final case class Ymd(year: Int, month: Int, day: Int) derives CanEqual

  private val pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$".r

  def isLeap(year: Int): Boolean = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0

  def monthLength(year: Int, month: Int): Int = month match
    case 2 => if isLeap(year) then 29 else 28
    case 4 | 6 | 9 | 11 => 30
    case _ => 31

  /** Reads `YYYY-MM-DD` (surrounding blanks allowed); `None` for any other text or an impossible day such as `2026-02-30`. */
  def parse(text: String): Option[Ymd] =
    val trimmed = text.trim.nn
    if !pattern.matches(trimmed) then None
    else
      val (year, month, day) = (trimmed.take(4).toInt, trimmed.slice(5, 7).toInt, trimmed.drop(8).toInt)
      Option.when(month >= 1 && month <= 12 && day >= 1 && day <= monthLength(year, month))(Ymd(year, month, day))

  def format(day: Ymd): String = f"${day.year}%04d-${day.month}%02d-${day.day}%02d"

  def isValid(text: String): Boolean = parse(text).isDefined

  def toEpochDay(day: Ymd): Int =
    val y = if day.month <= 2 then day.year - 1 else day.year
    val era = (if y >= 0 then y else y - 399) / 400
    val yearOfEra = y - era * 400
    val shiftedMonth = (day.month + 9) % 12 // March = 0
    val dayOfYear = (153 * shiftedMonth + 2) / 5 + day.day - 1
    val dayOfEra = yearOfEra * 365 + yearOfEra / 4 - yearOfEra / 100 + dayOfYear
    era * 146097 + dayOfEra - 719468

  def fromEpochDay(epochDay: Int): Ymd =
    val z = epochDay + 719468
    val era = (if z >= 0 then z else z - 146096) / 146097
    val dayOfEra = z - era * 146097
    val yearOfEra = (dayOfEra - dayOfEra / 1460 + dayOfEra / 36524 - dayOfEra / 146096) / 365
    val dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra / 4 - yearOfEra / 100)
    val shiftedMonth = (5 * dayOfYear + 2) / 153
    val day = dayOfYear - (153 * shiftedMonth + 2) / 5 + 1
    val month = if shiftedMonth < 10 then shiftedMonth + 3 else shiftedMonth - 9
    Ymd(yearOfEra + era * 400 + (if month <= 2 then 1 else 0), month, day)

  /** ISO day of the week: 1 = Monday … 7 = Sunday (1970-01-01 was a Thursday). */
  def dayOfWeek(day: Ymd): Int = Math.floorMod(toEpochDay(day) + 3, 7) + 1

  def plusDays(day: Ymd, days: Int): Ymd = fromEpochDay(toEpochDay(day) + days)

  /** Adds calendar months, clamping the day to the length of the target month (`2026-03-31` − 1 month = `2026-02-28`). */
  def plusMonths(day: Ymd, months: Int): Ymd =
    val index = day.year * 12 + (day.month - 1) + months
    val year = Math.floorDiv(index, 12)
    val month = Math.floorMod(index, 12) + 1
    Ymd(year, month, day.day.min(monthLength(year, month)))

  /** The Monday of the ISO week holding `day`. */
  def mondayOf(day: Ymd): Ymd = plusDays(day, 1 - dayOfWeek(day))

  def firstOfMonth(day: Ymd): Ymd = day.copy(day = 1)

  /** Negative, zero, or positive as `a` is before, on, or after `b`. */
  def compare(a: Ymd, b: Ymd): Int = toEpochDay(a).compare(toEpochDay(b))

  /** `YYYY-MM-DD` shifted by `days`; `None` when `text` is not a valid day. */
  def shift(text: String, days: Int): Option[String] = parse(text).map(day => format(plusDays(day, days)))

  /** A day as the UI writes it: `DD/MM/YYYY`; text that is not a valid day is returned unchanged. */
  def display(text: String): String = parse(text).fold(text)(day => f"${day.day}%02d/${day.month}%02d/${day.year}%04d")
end IsoDay
