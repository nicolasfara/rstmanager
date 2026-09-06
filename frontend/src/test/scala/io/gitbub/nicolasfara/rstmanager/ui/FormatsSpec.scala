package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the `H:MM` duration formatting/parsing helpers used across the task-duration UI. */
final class FormatsSpec extends AnyFunSuite with Matchers:

  test("duration formats total minutes as H:MM with a zero-padded minute part"):
    Formats.duration(0) shouldBe "0:00"
    Formats.duration(20) shouldBe "0:20"
    Formats.duration(60) shouldBe "1:00"
    Formats.duration(95) shouldBe "1:35"
    Formats.duration(600) shouldBe "10:00"

  test("parseDuration reads an H:MM value into total minutes"):
    Formats.parseDuration("0:20") shouldBe Some(20)
    Formats.parseDuration("1:35") shouldBe Some(95)
    Formats.parseDuration(" 2:05 ") shouldBe Some(125)

  test("parseDuration reads a plain integer as whole hours"):
    Formats.parseDuration("8") shouldBe Some(480)
    Formats.parseDuration("0") shouldBe Some(0)

  test("parseDuration rejects malformed or out-of-range input"):
    Formats.parseDuration("") shouldBe None
    Formats.parseDuration("abc") shouldBe None
    Formats.parseDuration("1:60") shouldBe None // minutes must be < 60
    Formats.parseDuration("1:-5") shouldBe None
    Formats.parseDuration("1:2:3") shouldBe None
    Formats.parseDuration("-3") shouldBe None

  test("duration and parseDuration round-trip for representative values"):
    for minutes <- List(0, 20, 60, 95, 125, 480, 3000) do Formats.parseDuration(Formats.duration(minutes)) shouldBe Some(minutes)

end FormatsSpec
