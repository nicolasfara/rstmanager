package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.scalajs.js

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the Europe/Rome business day of the browser. */
final class BusinessDaySpec extends AnyFunSuite with Matchers:

  test("the business day is the calendar day in Europe/Rome, across midnight and daylight saving"):
    // 2026-10-31 23:30 UTC is already 1 November in Rome (CET, UTC+1, after the end of daylight saving on 25 October).
    BusinessDay.of(js.Date.UTC(2026, 9, 31, 23, 30)) shouldBe Some("2026-11-01")
    // Summer time (CEST, UTC+2): 22:30 UTC is past midnight in Rome, 21:59 UTC is not.
    BusinessDay.of(js.Date.UTC(2026, 5, 30, 22, 30)) shouldBe Some("2026-07-01")
    BusinessDay.of(js.Date.UTC(2026, 5, 30, 21, 59)) shouldBe Some("2026-06-30")
    BusinessDay.of(js.Date.UTC(2026, 0, 1, 0, 0)) shouldBe Some("2026-01-01")

  test("today is a valid YYYY-MM-DD day"):
    IsoDay.isValid(BusinessDay.today()) shouldBe true
