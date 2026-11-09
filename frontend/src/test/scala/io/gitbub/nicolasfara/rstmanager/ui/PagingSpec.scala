package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import Paging.*

/** Unit tests for the client-side pagination of the list pages. */
final class PagingSpec extends AnyFunSuite with Matchers:

  private val rows = (1 to 23).toList

  test("an empty list has a single empty page"):
    slice(List.empty[Int], 0, 10) shouldBe PageSlice(List.empty[Int], 0, 1, 0, 0, 0)

  test("the first page holds the first `size` rows"):
    slice(rows, 0, 10) shouldBe PageSlice((1 to 10).toList, 0, 3, 23, 1, 10)

  test("the last page may be partial"):
    slice(rows, 2, 10) shouldBe PageSlice(List(21, 22, 23), 2, 3, 23, 21, 23)

  test("a page past the last one is clamped to the last one"):
    slice(rows, 7, 10).page shouldBe 2
    slice(rows, 7, 10).items shouldBe List(21, 22, 23)

  test("a negative page is clamped to the first one"):
    slice(rows, -1, 10).page shouldBe 0

  test("a size below 1 is treated as 1"):
    slice(rows, 0, 0) shouldBe PageSlice(List(1), 0, 23, 23, 1, 1)

  test("a list that fits in one page has neither a previous nor a next page"):
    val single = slice(rows, 0, 25)
    single.pageCount shouldBe 1
    single.hasPrevious shouldBe false
    single.hasNext shouldBe false

  test("previous and next follow the page position"):
    slice(rows, 1, 10).hasPrevious shouldBe true
    slice(rows, 1, 10).hasNext shouldBe true
    slice(rows, 2, 10).hasNext shouldBe false
end PagingSpec
