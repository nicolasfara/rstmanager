package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import Sorting.*

/** Unit tests for the column sorting of the list tables. */
final class SortingSpec extends AnyFunSuite with Matchers:

  private final case class Row(code: String, note: Option[String], amount: String) derives CanEqual

  private val rows = List(
    Row("ORD-10", Some("beta"), "12.5"),
    Row("ord-9", None, "-3.00"),
    Row("ORD-100", Some("Alfa"), "12.45"),
    Row("ORD-9b", Some(" "), "x"),
  )

  private val columns = Map(
    "code" -> Column.text[Row](_.code),
    "note" -> Column.optionalText[Row](_.note),
    "amount" -> Column.decimal[Row](_.amount),
  )

  private def asc(column: String) = Some(SortBy(column, Direction.Ascending))
  private def desc(column: String) = Some(SortBy(column, Direction.Descending))

  test("a new column starts ascending, then goes descending, then back to unsorted"):
    toggle(None, "code") shouldBe asc("code")
    toggle(asc("code"), "code") shouldBe desc("code")
    toggle(desc("code"), "code") shouldBe None
    toggle(desc("code"), "note") shouldBe asc("note")

  test("unsorted, or sorted by an unknown column, keeps the original order"):
    sort(rows, None, columns) shouldBe rows
    sort(rows, asc("missing"), columns) shouldBe rows

  test("text sorts case-insensitively, with digit runs compared as numbers"):
    sort(rows, asc("code"), columns).map(_.code) shouldBe List("ord-9", "ORD-9b", "ORD-10", "ORD-100")
    sort(rows, desc("code"), columns).map(_.code) shouldBe List("ORD-100", "ORD-10", "ORD-9b", "ord-9")

  test("missing or blank values go last in both directions"):
    sort(rows, asc("note"), columns).map(_.code) shouldBe List("ORD-100", "ORD-10", "ord-9", "ORD-9b")
    sort(rows, desc("note"), columns).map(_.code) shouldBe List("ORD-10", "ORD-100", "ord-9", "ORD-9b")

  test("decimals sort by value, unparsable ones last"):
    sort(rows, asc("amount"), columns).map(_.amount) shouldBe List("-3.00", "12.45", "12.5", "x")
    sort(rows, desc("amount"), columns).map(_.amount) shouldBe List("12.5", "12.45", "-3.00", "x")

  test("ties keep their original order"):
    val tied = List(Row("A", None, "1"), Row("a", None, "1"), Row("A", None, "2"))
    sort(tied, asc("amount"), columns) shouldBe tied

  test("ISO days and instants sort chronologically"):
    List("2026-11-02", "2025-12-31", "2026-01-15").sorted(using naturalText) shouldBe List("2025-12-31", "2026-01-15", "2026-11-02")
end SortingSpec
