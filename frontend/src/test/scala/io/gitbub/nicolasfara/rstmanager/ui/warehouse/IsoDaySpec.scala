package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import IsoDay.*

/** Unit tests for the pure `YYYY-MM-DD` calendar arithmetic. */
final class IsoDaySpec extends AnyFunSuite with Matchers:

  private def day(text: String): Ymd = parse(text).getOrElse(fail(s"not a day: $text"))

  test("parse accepts real days only, in the strict YYYY-MM-DD shape"):
    parse("2026-10-19") shouldBe Some(Ymd(2026, 10, 19))
    parse(" 2024-02-29 ") shouldBe Some(Ymd(2024, 2, 29))
    parse("2026-02-29") shouldBe None
    parse("2026-13-01") shouldBe None
    parse("2026-04-31") shouldBe None
    parse("2026-1-5") shouldBe None
    parse("19/10/2026") shouldBe None
    parse("") shouldBe None

  test("epoch days round-trip and match known dates"):
    toEpochDay(Ymd(1970, 1, 1)) shouldBe 0
    toEpochDay(Ymd(2000, 3, 1)) shouldBe 11017
    toEpochDay(Ymd(1969, 12, 31)) shouldBe -1
    (-1000 to 30000 by 37).foreach(n => toEpochDay(fromEpochDay(n)) shouldBe n)

  test("dayOfWeek is ISO: Monday 1 … Sunday 7"):
    dayOfWeek(day("1970-01-01")) shouldBe 4
    dayOfWeek(day("2026-10-19")) shouldBe 1
    dayOfWeek(day("2026-11-01")) shouldBe 7

  test("plusDays crosses month, year, and leap-day boundaries"):
    format(plusDays(day("2026-12-31"), 1)) shouldBe "2027-01-01"
    format(plusDays(day("2024-03-01"), -1)) shouldBe "2024-02-29"
    format(plusDays(day("2026-03-29"), 1)) shouldBe "2026-03-30" // DST change in Europe: no drift
    shift("2026-01-01", -1) shouldBe Some("2025-12-31")
    shift("nope", 1) shouldBe None

  test("plusMonths clamps the day to the target month"):
    format(plusMonths(day("2026-03-31"), -1)) shouldBe "2026-02-28"
    format(plusMonths(day("2026-01-15"), -5)) shouldBe "2025-08-15"
    format(plusMonths(day("2024-02-29"), 60)) shouldBe "2029-02-28"

  test("mondayOf and firstOfMonth"):
    format(mondayOf(day("2026-11-01"))) shouldBe "2026-10-26"
    format(mondayOf(day("2026-10-26"))) shouldBe "2026-10-26"
    format(mondayOf(day("2027-01-01"))) shouldBe "2026-12-28"
    format(firstOfMonth(day("2026-11-17"))) shouldBe "2026-11-01"

  test("display writes DD/MM/YYYY and leaves other text alone"):
    display("2026-10-05") shouldBe "05/10/2026"
    display("boh") shouldBe "boh"
end IsoDaySpec
