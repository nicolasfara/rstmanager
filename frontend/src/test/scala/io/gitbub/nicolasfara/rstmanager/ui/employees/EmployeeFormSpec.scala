package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure employee form: loading a stored employee, request building, and the overrides it must not drop. */
final class EmployeeFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[EmployeeRequest, EmployeeRequest] = CanEqual.derived
  private given CanEqual[EmployeeContractDto, EmployeeContractDto] = CanEqual.derived
  private given CanEqual[List[HoursOverrideDto], List[HoursOverrideDto]] = CanEqual.derived

  private val employeeId = UUID.fromString("00000000-0000-0000-0000-000000000401").nn

  private val overrides = List(
    HoursOverrideDto("working_day", Some(4), Some("Inventario"), Some("2026-03-02T00:00:00.000Z"), None, None),
    HoursOverrideDto("vacation", None, None, None, Some("2026-08-10T00:00:00.000Z"), Some("2026-08-21T00:00:00.000Z")),
  )

  private val stored = EmployeeResponse(
    employeeId,
    "Mario",
    "Rossi",
    EmployeeContractDto("fixed_term", "2026-01-05T00:00:00.000Z", Some("2026-12-31T00:00:00.000Z"), None),
    38,
    overrides,
  )

  test("fromResponse loads the registry data with the contract days as YYYY-MM-DD"):
    val state = EmployeeForm.fromResponse(stored)
    state.editingId shouldBe Some(employeeId)
    (state.name, state.surname, state.contractKind) shouldBe ("Mario", "Rossi", "fixed_term")
    (state.startDate, state.endDate, state.budget) shouldBe ("2026-01-05", "2026-12-31", "38")
    state.errors shouldBe Nil

  test("an untouched stored employee round-trips to the same request, overrides included"):
    EmployeeForm.fromResponse(stored).request shouldBe
      EmployeeRequest(stored.name, stored.surname, stored.contract, stored.budgetWeeklyHours, stored.overrides)

  test("editing the registry data keeps the existing overrides in the update request"):
    val edited = EmployeeForm.fromResponse(stored).copy(name = " Maria ", contractKind = "part_time", weeklyHours = "24", budget = "24")
    val request = edited.request
    request.name shouldBe "Maria"
    request.contract shouldBe EmployeeContractDto("part_time", "2026-01-05T00:00:00.000Z", None, Some(24))
    request.budgetWeeklyHours shouldBe 24
    request.overrides shouldBe overrides

  test("a new employee is sent without overrides"):
    val state = EmployeeForm.empty.copy(name = "Anna", surname = "Bianchi", startDate = "2026-02-01")
    state.editingId shouldBe None
    state.request shouldBe EmployeeRequest("Anna", "Bianchi", EmployeeContractDto("full_time", "2026-02-01T00:00:00.000Z", None, None), 40, Nil)

  test("the contract fields of the chosen kind are required"):
    EmployeeForm.empty.errors shouldBe List("Nome obbligatorio", "Cognome obbligatorio", "Data inizio obbligatoria")
    val base = EmployeeForm.empty.copy(name = "A", surname = "B", startDate = "2026-02-01")
    base.copy(contractKind = "fixed_term").errors shouldBe List("Data fine obbligatoria")
    base.copy(contractKind = "part_time", weeklyHours = "0").errors shouldBe List("Ore settimanali non valide")
    base.copy(budget = "x").errors shouldBe List("Budget non valido")
end EmployeeFormSpec
