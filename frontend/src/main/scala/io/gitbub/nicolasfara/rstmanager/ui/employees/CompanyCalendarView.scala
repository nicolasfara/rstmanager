package io.gitbub.nicolasfara.rstmanager.ui.employees

import io.gitbub.nicolasfara.rstmanager.api.Dtos.ClosureResponse
import io.gitbub.nicolasfara.rstmanager.ui.Formats
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.IsoDay
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.IsoDay.Ymd

/**
 * Pure (DOM-free) reading of the company calendar: which closure covers a day, which ones a vacation overlaps, and the order the calendar tab lists
 * them in. Shared by the calendar tab, the hours-override editor, and the planning board. Same rule as the server: a period covers its first and last
 * day inclusive; an annual recurrence covers its day and month every year, so 29/02 only in leap years.
 */
object CompanyCalendarView:

  /**
   * Days scanned for annual recurrences when matching a range: four years hold every day and month, 29/02 included, so a longer range is not scanned
   * further.
   */
  private val maxScannedDays = 1461

  /** First and last day of a `period` closure; `None` for an annual one or unreadable dates. */
  def periodBounds(closure: ClosureResponse): Option[(Ymd, Ymd)] =
    for
      start <- closure.startDate.flatMap(d => IsoDay.parse(Formats.calendarDay(d)))
      end <- closure.endDate.flatMap(d => IsoDay.parse(Formats.calendarDay(d)))
    yield (start, end)

  def covers(closure: ClosureResponse, day: Ymd): Boolean = closure.kind match
    case "period" => periodBounds(closure).exists((start, end) => IsoDay.compare(start, day) <= 0 && IsoDay.compare(day, end) <= 0)
    case "annual" => closure.month.contains(day.month) && closure.day.contains(day.day)
    case _ => false

  /** The closure covering `day` (`YYYY-MM-DD`, or an ISO instant whose date prefix is read); `None` on a working day or unreadable text. */
  def closureOn(day: String, closures: List[ClosureResponse]): Option[ClosureResponse] =
    IsoDay.parse(Formats.calendarDay(day)).flatMap(d => closures.find(covers(_, d)))

  /**
   * The closures with at least one day from `start` to `end` (`YYYY-MM-DD`, inclusive), in the given order; `end` blank means the single day `start`.
   * Empty when the range is unreadable or reversed.
   */
  def closuresDuring(start: String, end: String, closures: List[ClosureResponse]): List[ClosureResponse] =
    val last = if end.trim.nn.isEmpty then start else end
    (IsoDay.parse(start), IsoDay.parse(last)) match
      case (Some(from), Some(to)) if IsoDay.compare(from, to) <= 0 =>
        lazy val days = (IsoDay.toEpochDay(from) to IsoDay.toEpochDay(to)).iterator.take(maxScannedDays).map(IsoDay.fromEpochDay).toList
        closures.filter { closure =>
          closure.kind match
            case "period" => periodBounds(closure).exists((s, e) => IsoDay.compare(s, to) <= 0 && IsoDay.compare(from, e) <= 0)
            case _ => days.exists(covers(closure, _))
        }
      case _ => Nil

  /** A period ends before `today` (`YYYY-MM-DD`). */
  def isPast(closure: ClosureResponse, today: String): Boolean =
    closure.endDate.map(Formats.calendarDay).exists(_.compareTo(today) < 0)

  /** The `period` closures to list: current and upcoming ones first by start day, then past ones from the most recent. */
  def periods(closures: List[ClosureResponse], today: String): List[ClosureResponse] =
    val (past, upcoming) = closures.filter(_.kind == "period").partition(isPast(_, today))
    upcoming.sortBy(c => c.startDate.map(Formats.calendarDay).getOrElse("")) ++
      past.sortBy(c => c.endDate.map(Formats.calendarDay).getOrElse("")).reverse

  /** The `annual` closures in calendar order (month, then day). */
  def annuals(closures: List[ClosureResponse]): List[ClosureResponse] =
    closures.filter(_.kind == "annual").sortBy(c => (c.month.getOrElse(0), c.day.getOrElse(0)))

  /** When the closure applies, as the UI writes it: `gg/mm/aaaa → gg/mm/aaaa` (a single day once) or `gg/mm` for a recurrence. */
  def describe(closure: ClosureResponse): String = closure.kind match
    case "annual" => f"${closure.day.getOrElse(0)}%02d/${closure.month.getOrElse(0)}%02d"
    case _ =>
      val start = closure.startDate.map(d => IsoDay.display(Formats.calendarDay(d))).getOrElse("?")
      val end = closure.endDate.map(d => IsoDay.display(Formats.calendarDay(d))).getOrElse("?")
      if start == end then start else s"$start → $end"
end CompanyCalendarView
