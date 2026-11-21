package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

final class DayTextSpec extends AnyFunSuite with Matchers:

  test("dayOf keeps the day part of an instant"):
    DayText.dayOf("2026-03-12T00:00:00.000Z") shouldBe "2026-03-12"

  test("short and long display a yyyy-MM-dd day"):
    DayText.short("2026-03-07") shouldBe "07/03"
    DayText.long("2026-03-07") shouldBe "07/03/2026"

  test("unreadable days display as empty"):
    DayText.short("") shouldBe ""
    DayText.long("2026-13-01") shouldBe ""

  test("today is a yyyy-MM-dd day"):
    DayText.long(DayText.today()) should not be empty
end DayTextSpec
