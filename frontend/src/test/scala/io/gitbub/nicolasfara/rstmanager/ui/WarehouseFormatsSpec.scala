package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import WarehouseFormats.*

/** Unit tests for the Italian decimal parsing/formatting of warehouse quantities, prices, and amounts. */
final class WarehouseFormatsSpec extends AnyFunSuite with Matchers:

  // ---- parseDecimal ------------------------------------------------------------------------------

  test("parseDecimal accepts both decimal separators and pads to the requested scale"):
    parseDecimal("1,5", 4) shouldBe Some("1.5000")
    parseDecimal("1.5", 4) shouldBe Some("1.5000")
    parseDecimal("18,50", 4) shouldBe Some("18.5000")
    parseDecimal("1234,5", 2) shouldBe Some("1234.50")
    parseDecimal("7", 3) shouldBe Some("7.000")
    parseDecimal("7", 0) shouldBe Some("7")

  test("parseDecimal reads grouping with the other separator"):
    parseDecimal("1.234,56", 2) shouldBe Some("1234.56")
    parseDecimal("1,234.56", 2) shouldBe Some("1234.56")
    parseDecimal("1.234.567", 2) shouldBe Some("1234567.00")
    parseDecimal("12.345.678,9", 4) shouldBe Some("12345678.9000")

  test("parseDecimal trims, drops leading zeros, and reads an omitted integer part as zero"):
    parseDecimal("  2,5  ", 3) shouldBe Some("2.500")
    parseDecimal("007,10", 2) shouldBe Some("7.10")
    parseDecimal(",5", 2) shouldBe Some("0.50")
    parseDecimal("0", 2) shouldBe Some("0.00")

  test("parseDecimal treats a single dot as the decimal separator, never as grouping"):
    parseDecimal("1.234", 4) shouldBe Some("1.2340")
    parseDecimal("1.234", 2) shouldBe None // three decimals > scale 2

  test("parseDecimal rejects more decimals than the scale"):
    parseDecimal("1,23456", 4) shouldBe None
    parseDecimal("0,001", 2) shouldBe None
    parseDecimal("1,5", 0) shouldBe None

  test("parseDecimal rejects negatives unless allowed, and never yields a negative zero"):
    parseDecimal("-1,5", 3) shouldBe None
    parseDecimal("-1,5", 3, true) shouldBe Some("-1.500")
    parseDecimal("-0,0", 3, true) shouldBe Some("0.000")

  test("parseDecimal rejects blanks, garbage, and malformed numbers"):
    List("", "   ", "abc", "1,5€", "1e3", "1,", ".", "-", "1,2,3", "1.23.4", "12.34,5.6", "1.2345,6", ",", "1..2", "+1", "1 234", "1,5,").foreach {
      text => withClue(s"'$text'")(parseDecimal(text, 4) shouldBe None)
    }

  test("parseDecimal rejects inputs that are too long"):
    parseDecimal("1" * 40, 0) shouldBe None

  // ---- formatting --------------------------------------------------------------------------------

  test("formatDecimal writes Italian grouping and decimals, trimming zeros down to the minimum scale"):
    formatDecimal("1234.5600", 2, 4) shouldBe "1.234,56"
    formatDecimal("1234567.0000", 0, 4) shouldBe "1.234.567"
    formatDecimal("0.1235", 2, 4) shouldBe "0,1235"
    formatDecimal("-1234.5", 2, 2) shouldBe "-1.234,50"
    formatDecimal("999", 0, 0) shouldBe "999"

  test("formatDecimal rounds HALF_UP past the maximum scale"):
    formatDecimal("0.125", 2, 2) shouldBe "0,13"
    formatDecimal("0.124", 2, 2) shouldBe "0,12"
    formatDecimal("-0.125", 2, 2) shouldBe "-0,13"
    formatDecimal("999.995", 2, 2) shouldBe "1.000,00"

  test("formatDecimal returns a non-canonical value unchanged"):
    formatDecimal("n/a", 2, 2) shouldBe "n/a"
    formatDecimal("1,5", 2, 2) shouldBe "1,5"

  test("formatEuro and formatUnitPrice show euros with 2 and 2-to-4 decimals"):
    formatEuro("1234.56") shouldBe "1.234,56 €"
    formatEuro("18.5000") shouldBe "18,50 €"
    formatEuro("0") shouldBe "0,00 €"
    formatUnitPrice("3.7000") shouldBe "3,70 €"
    formatUnitPrice("0.1235") shouldBe "0,1235 €"
    formatUnitPrice("2.1230") shouldBe "2,123 €"

  test("formatQty shows up to 3 decimals and the unit short label"):
    formatQty("2.500", "linear_meters") shouldBe "2,5 m"
    formatQty("3.000", "pieces") shouldBe "3 pz"
    formatQty("1250.125", "square_meters") shouldBe "1.250,125 m²"
    formatQty("0.750", "kilograms") shouldBe "0,75 kg"

  test("decimalInputText prefills inputs without grouping"):
    decimalInputText("1234.5000", 2, 4) shouldBe "1234,50"
    decimalInputText("18.1234", 2, 4) shouldBe "18,1234"

  test("unit labels cover every wire name and fall back to the raw name"):
    MeasureUnit.values.toList.map(u => unitLabel(u.wireName)) shouldBe List("Pezzi", "Metri lineari", "Metri quadri", "Chilogrammi")
    MeasureUnit.values.toList.map(u => unitShortLabel(u.wireName)) shouldBe List("pz", "m", "m²", "kg")
    unitLabel("barrels") shouldBe "barrels"
    MeasureUnit.options.map(_._1) shouldBe List("pieces", "linear_meters", "square_meters", "kilograms")

  // ---- round trips -------------------------------------------------------------------------------

  test("canonical values survive format -> parse at the same scale"):
    for value <- List("0.0000", "18.5000", "1234.5678", "1000000.0000", "0.0001") do
      parseDecimal(formatDecimal(value, 4, 4), 4) shouldBe Some(value)
      parseDecimal(decimalInputText(value, 2, 4), 4) shouldBe Some(value)

  test("typed values survive parse -> input text -> parse"):
    for text <- List("18,5", "1.234,56", "0,1", "42", "1.234.567,8") do
      val canonical = parseDecimal(text, 4)
      canonical.flatMap(c => parseDecimal(decimalInputText(c, 2, 4), 4)) shouldBe canonical
end WarehouseFormatsSpec
