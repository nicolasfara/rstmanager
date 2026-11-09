package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure company-closure form: validation messages and request building for both kinds. */
final class ClosureFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[ClosureRequest, ClosureRequest] = CanEqual.derived

  private val closureId = UUID.fromString("00000000-0000-0000-0000-000000000501").nn

  private val period = ClosureForm.empty.copy(startDate = "2026-08-10", endDate = "2026-08-21", reason = " Chiusura estiva ")
  private val annual = ClosureForm.empty.copy(kind = "annual", day = "15", month = "8", reason = "Ferragosto")

  test("a period is sent with its days as ISO instants and no recurrence fields"):
    period.errors shouldBe Nil
    period.request shouldBe
      ClosureRequest("period", "Chiusura estiva", Some("2026-08-10T00:00:00.000Z"), Some("2026-08-21T00:00:00.000Z"), None, None)

  test("an annual recurrence is sent with day and month only"):
    annual.errors shouldBe Nil
    annual.request shouldBe ClosureRequest("annual", "Ferragosto", None, None, Some(8), Some(15))

  test("the reason is required for both kinds"):
    period.copy(reason = "  ").errors shouldBe List("Motivo obbligatorio")
    annual.copy(reason = "").errors shouldBe List("Motivo obbligatorio")

  test("a period needs two valid days, the last not before the first"):
    ClosureForm.empty.copy(reason = "X").errors shouldBe List("Data inizio obbligatoria", "Data fine obbligatoria")
    period.copy(startDate = "2026-02-30").errors shouldBe List("Data inizio non valida")
    period.copy(endDate = "2026-08-09").errors shouldBe List("La data fine precede la data inizio")
    period.copy(endDate = "2026-08-10").errors shouldBe Nil

  test("an annual recurrence needs a day that exists in its month, 29/02 included"):
    annual.copy(day = "31", month = "4").errors shouldBe List("Giorno non valido")
    annual.copy(day = "29", month = "2").errors shouldBe Nil
    annual.copy(day = "30", month = "2").errors shouldBe List("Giorno non valido")
    annual.copy(day = "0").errors shouldBe List("Giorno non valido")
    annual.copy(month = "13").errors shouldBe List("Mese non valido")

  test("a blank form holds back its messages; a loaded one does not"):
    ClosureForm.empty.isBlank shouldBe true
    period.isBlank shouldBe false
    ClosureForm.fromResponse(ClosureResponse(closureId, "annual", "Natale", None, None, Some(12), Some(25))).isBlank shouldBe false

  test("fromResponse -> request round-trips both kinds"):
    val storedPeriod =
      ClosureResponse(closureId, "period", "Inventario", Some("2026-12-28T00:00:00.000Z"), Some("2026-12-31T00:00:00.000Z"), None, None)
    val loaded = ClosureForm.fromResponse(storedPeriod)
    loaded.editingId shouldBe Some(closureId)
    (loaded.startDate, loaded.endDate) shouldBe ("2026-12-28", "2026-12-31")
    loaded.request shouldBe ClosureRequest("period", "Inventario", storedPeriod.startDate, storedPeriod.endDate, None, None)

    val storedAnnual = ClosureResponse(closureId, "annual", "Natale", None, None, Some(12), Some(25))
    ClosureForm.fromResponse(storedAnnual).request shouldBe ClosureRequest("annual", "Natale", None, None, Some(12), Some(25))
end ClosureFormSpec
