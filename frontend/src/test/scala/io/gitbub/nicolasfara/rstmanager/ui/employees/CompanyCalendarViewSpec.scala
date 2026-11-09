package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.ClosureResponse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure company-calendar reading: `closureOn`, the vacation overlap, and the listing order. */
final class CompanyCalendarViewSpec extends AnyFunSuite with Matchers:

  private given CanEqual[ClosureResponse, ClosureResponse] = CanEqual.derived

  private def id(n: Int): UUID = UUID.fromString(f"00000000-0000-0000-0000-$n%012d").nn

  private def period(n: Int, start: String, end: String, reason: String): ClosureResponse =
    ClosureResponse(id(n), "period", reason, Some(s"${start}T00:00:00.000Z"), Some(s"${end}T00:00:00.000Z"), None, None)

  private def annual(n: Int, day: Int, month: Int, reason: String): ClosureResponse =
    ClosureResponse(id(n), "annual", reason, None, None, Some(month), Some(day))

  private val summer = period(1, "2026-08-10", "2026-08-21", "Chiusura estiva")
  private val ferragosto = annual(2, 15, 8, "Ferragosto")
  private val leapDay = annual(3, 29, 2, "29 febbraio")
  private val christmas = annual(4, 25, 12, "Natale")

  test("a period covers its first and last day, inclusive, and nothing outside"):
    val closures = List(summer)
    CompanyCalendarView.closureOn("2026-08-09", closures) shouldBe None
    CompanyCalendarView.closureOn("2026-08-10", closures) shouldBe Some(summer)
    CompanyCalendarView.closureOn("2026-08-21", closures) shouldBe Some(summer)
    CompanyCalendarView.closureOn("2026-08-22", closures) shouldBe None

  test("closureOn also reads the day of an ISO instant"):
    CompanyCalendarView.closureOn("2026-08-21T00:00:00.000Z", List(summer)) shouldBe Some(summer)

  test("an annual recurrence matches its day and month every year"):
    val closures = List(christmas)
    CompanyCalendarView.closureOn("2026-12-25", closures) shouldBe Some(christmas)
    CompanyCalendarView.closureOn("2031-12-25", closures) shouldBe Some(christmas)
    CompanyCalendarView.closureOn("2026-12-26", closures) shouldBe None
    CompanyCalendarView.closureOn("2026-11-25", closures) shouldBe None

  test("29/02 is closed only in leap years"):
    val closures = List(leapDay)
    CompanyCalendarView.closureOn("2028-02-29", closures) shouldBe Some(leapDay)
    CompanyCalendarView.closureOn("2027-02-28", closures) shouldBe None
    CompanyCalendarView.closureOn("2027-03-01", closures) shouldBe None

  test("unreadable days and working days have no closure"):
    CompanyCalendarView.closureOn("not-a-day", List(summer, christmas)) shouldBe None
    CompanyCalendarView.closureOn("2026-03-02", Nil) shouldBe None

  test("closuresDuring lists the closures a vacation touches"):
    val closures = List(summer, ferragosto, christmas)
    CompanyCalendarView.closuresDuring("2026-08-01", "2026-08-10", closures) shouldBe List(summer)
    CompanyCalendarView.closuresDuring("2026-08-14", "2026-08-16", closures) shouldBe List(summer, ferragosto)
    CompanyCalendarView.closuresDuring("2026-12-24", "", closures) shouldBe Nil
    CompanyCalendarView.closuresDuring("2026-12-25", "", closures) shouldBe List(christmas)
    CompanyCalendarView.closuresDuring("2026-09-01", "2026-09-30", closures) shouldBe Nil
    CompanyCalendarView.closuresDuring("2026-08-21", "2026-08-10", closures) shouldBe Nil

  test("periods list current and upcoming ones by start, then past ones from the most recent"):
    val past1 = period(5, "2026-01-02", "2026-01-03", "Ponte")
    val past2 = period(6, "2026-04-03", "2026-04-06", "Pasqua")
    val next = period(7, "2026-12-28", "2026-12-31", "Inventario")
    val periods = CompanyCalendarView.periods(List(past1, next, summer, past2, christmas), "2026-08-15")
    periods shouldBe List(summer, next, past2, past1)
    CompanyCalendarView.isPast(past2, "2026-08-15") shouldBe true
    CompanyCalendarView.isPast(summer, "2026-08-21") shouldBe false

  test("annual recurrences are listed by month, then day, and written as gg/mm"):
    CompanyCalendarView.annuals(List(christmas, summer, ferragosto, leapDay)) shouldBe List(leapDay, ferragosto, christmas)
    CompanyCalendarView.describe(ferragosto) shouldBe "15/08"
    CompanyCalendarView.describe(summer) shouldBe "10/08/2026 → 21/08/2026"
    CompanyCalendarView.describe(period(8, "2026-06-02", "2026-06-02", "Festa")) shouldBe "02/06/2026"
end CompanyCalendarViewSpec
