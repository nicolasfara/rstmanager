package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import SearchTerms.*

/** Unit tests for the multi-term search of the searchable selects and the client-side filters. */
final class SearchTermsSpec extends AnyFunSuite with Matchers:

  test("terms splits at any run of blanks and lowers the case"):
    terms("  Tes \t BLU  navy ") shouldBe List("tes", "blu", "navy")
    terms("   ") shouldBe Nil

  test("every term must appear, in any order and case"):
    val text = "TES-001 Tessuto blu navy AC-152 X9"
    matches("navy tes", text) shouldBe true
    matches("ac-152", text) shouldBe true
    matches("x9 BLU", text) shouldBe true
    matches("blu rosso", text) shouldBe false

  test("a blank query matches everything"):
    matches("", "anything") shouldBe true
    matches("  ", "") shouldBe true

  test("matchesAny needs each term in at least one field, not all terms in the same one"):
    matchesAny("tes blu", List("TES-001", "Tessuto blu")) shouldBe true
    matchesAny("tes rosso", List("TES-001", "Tessuto blu")) shouldBe false
    matchesAny("tes", Nil) shouldBe false
end SearchTermsSpec
