package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the texts of the delay check of a new order. */
final class OrderFeasibilitySpec extends AnyFunSuite with Matchers:

  private val otherOrder = UUID.fromString("00000000-0000-0000-0000-000000000007").nn

  private val onTime = OrderFeasibilityResponse(
    Some("2026-06-16T00:00:00.000+02:00"),
    "2026-06-19T00:00:00.000Z",
    late = false,
    hasDelay = false,
    Nil,
    Nil,
    None,
    Nil,
  )

  test("an order on time shows its planned completion and no suggestion"):
    OrderFeasibility.headline(onTime) shouldBe "Nessun ritardo previsto"
    OrderFeasibility.details(onTime) shouldBe List("Fine lavori prevista il 16/06/2026 (deadline 19/06/2026).")
    OrderFeasibility.suggestedDay(onTime) shouldBe None
    OrderFeasibility.hint(onTime) shouldBe None

  test("a late order lists its delays and the next feasible date as a date-input day"):
    val late = onTime.copy(
      plannedCompletion = Some("2026-06-24T00:00:00.000+02:00"),
      late = true,
      hasDelay = true,
      inducedDelays = List(InducedDelayDto(otherOrder, "ORD-7", "2026-06-22T00:00:00.000Z", None)),
      suggestedDeadline = Some("2026-06-24T00:00:00.000+02:00"),
    )
    OrderFeasibility.headline(late) shouldBe "L'ordine genererebbe ritardi"
    OrderFeasibility.details(late) shouldBe List(
      "Fine lavori prevista il 24/06/2026, oltre la deadline del 19/06/2026.",
      "Ordine ORD-7: non sarebbe più pianificabile.",
    )
    OrderFeasibility.suggestedDay(late) shouldBe Some("2026-06-24")
    OrderFeasibility.suggestionText("2026-06-24") shouldBe "Prossima data utile senza ritardi: 24/06/2026"
    OrderFeasibility.hint(late) shouldBe None

  test("delays no deadline can avoid come with a hint instead of a suggestion"):
    val urgent = onTime.copy(
      hasDelay = true,
      inducedDelays = List(InducedDelayDto(otherOrder, "ORD-7", "2026-06-15T00:00:00.000Z", Some("2026-06-16T00:00:00.000+02:00"))),
    )
    OrderFeasibility.details(urgent).last shouldBe "Ordine ORD-7: finirebbe il 16/06/2026, oltre la sua deadline del 15/06/2026."
    OrderFeasibility.hint(urgent).isDefined shouldBe true
    OrderFeasibility.headline(onTime.copy(plannedCompletion = None, hasDelay = true)) shouldBe "Ordine non pianificabile con il personale attuale"
end OrderFeasibilitySpec
