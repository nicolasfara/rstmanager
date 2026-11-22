package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure employee form: loading a stored employee, request building, the overrides it must not drop, and the override editor. */
final class EmployeeFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[EmployeeRequest, EmployeeRequest] = CanEqual.derived
  private given CanEqual[EmployeeContractDto, EmployeeContractDto] = CanEqual.derived
  private given CanEqual[List[HoursOverrideDto], List[HoursOverrideDto]] = CanEqual.derived
  private given CanEqual[HoursOverrideDto, HoursOverrideDto] = CanEqual.derived

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

  test("issues are addressed to their field and the focus goes to the first one on screen"):
    import EmployeeForm.Field
    val base = EmployeeForm.empty.copy(name = "A", surname = "B", startDate = "2026-02-01")
    EmployeeForm.empty.issues.map(_.field) shouldBe List(Field.Name, Field.Surname, Field.StartDate)
    // The budget is listed before the contract fields, but sits after them on screen.
    val invalid = base.copy(contractKind = "fixed_term", budget = "x")
    invalid.issues.map(_.field) shouldBe List(Field.Budget, Field.EndDate)
    invalid.firstIssueOnScreen.map(_.field) shouldBe Some(Field.EndDate)
    base.copy(contractKind = "part_time", weeklyHours = "").firstIssueOnScreen.map(_.field) shouldBe Some(Field.WeeklyHours)
    base.firstIssueOnScreen shouldBe None
    EmployeeForm.fieldId(Field.EndDate) shouldBe "emp-end"

  // ---- Override section ------------------------------------------------------------------------

  import EmployeeForm.{ OverrideDraft, OverrideField }

  private val loaded = EmployeeForm.fromResponse(stored)
  private val workingDraft = OverrideDraft.empty.copy(hours = "6", reason = " Fiera ", day = "2026-04-07")
  private val vacationDraft = OverrideDraft.empty.copy(kind = "vacation", start = "2026-12-24", end = "2026-12-31")

  test("+ Aggiungi appends the typed override, clears the draft and keeps its kind; the save request carries it"):
    val added = loaded.copy(overrideDraft = vacationDraft).addOverride.getOrElse(fail("the vacation should be accepted"))
    added.overrides shouldBe overrides :+
      HoursOverrideDto("vacation", None, None, None, Some("2026-12-24T00:00:00.000Z"), Some("2026-12-31T00:00:00.000Z"))
    added.overrideDraft shouldBe OverrideDraft.empty.copy(kind = "vacation")
    added.request.overrides shouldBe added.overrides

    val working = loaded.copy(overrideDraft = workingDraft).addOverride.map(_.overrides.last)
    working shouldBe Some(HoursOverrideDto("working_day", Some(6), Some("Fiera"), Some("2026-04-07T00:00:00.000Z"), None, None))

  test("an override with invalid fields is refused field by field"):
    OverrideDraft.empty.issues(Nil).map(_.field) shouldBe List(OverrideField.Day)
    workingDraft.copy(hours = "25").issues(Nil).map(_.message) shouldBe List("Ore non valide (da 0 a 24)")
    workingDraft.copy(hours = "").issues(Nil).map(_.field) shouldBe List(OverrideField.Hours)
    workingDraft.copy(day = "2026-02-30").issues(Nil).map(_.message) shouldBe List("Giorno non valido")
    OverrideDraft.empty.copy(kind = "vacation").issues(Nil).map(_.field) shouldBe List(OverrideField.Start, OverrideField.End)
    vacationDraft.copy(end = "2026-12-23").issues(Nil).map(_.message) shouldBe List("La data fine precede la data inizio")
    vacationDraft.copy(end = "2026-12-24").issues(Nil) shouldBe Nil
    loaded.copy(overrideDraft = OverrideDraft.empty).addOverride shouldBe None

  test("an override that clashes with the ones in the list is refused, like the server does"):
    // `overrides`: 4h on 2026-03-02, vacation 2026-08-10 → 2026-08-21.
    workingDraft.copy(day = "2026-03-02").issues(overrides).map(_.message) shouldBe List("Esiste già un override per questo giorno")
    workingDraft.copy(day = "2026-08-21").issues(overrides).map(_.message) shouldBe List("Il giorno cade in un periodo di ferie già inserito")
    vacationDraft.copy(start = "2026-08-21", end = "2026-08-25").issues(overrides).map(_.message) shouldBe List("Si sovrappone a ferie già inserite")
    vacationDraft.copy(start = "2026-03-01", end = "2026-03-03").issues(overrides).map(_.message) shouldBe
      List("Comprende il giorno lavorativo del 02/03/2026")
    vacationDraft.copy(start = "2026-08-22", end = "2026-08-25").issues(overrides) shouldBe Nil

  test("removing an override drops only that one; the chips list them by first day"):
    loaded.removeOverride(0).overrides shouldBe overrides.drop(1)
    loaded.removeOverride(5).overrides shouldBe overrides
    val unordered = loaded.copy(overrides = overrides.reverse)
    unordered.overridesByDay.map(_._2) shouldBe List(1, 0)
    unordered.removeOverride(unordered.overridesByDay.head._2).overrides shouldBe List(overrides(1))

  test("a half-typed override blocks the save and is a change; a blank one with another kind is not"):
    import EmployeeForm.Field
    loaded.copy(overrideDraft = workingDraft).firstIssueOnScreen.map(_.field) shouldBe Some(Field.PendingOverride)
    EmployeeForm.fieldId(Field.PendingOverride) shouldBe EmployeeForm.overrideAddId
    val switchedKind = loaded.copy(overrideDraft = OverrideDraft.empty.copy(kind = "vacation", hours = ""))
    switchedKind.issues shouldBe Nil
    switchedKind.comparable == loaded shouldBe true
    loaded.copy(overrideDraft = workingDraft).comparable == loaded shouldBe false

  test("overrides are described with their days as dd/mm/yyyy, without a time-zone shift"):
    EmployeeForm.describeOverride(overrides.head) shouldBe "4h il 02/03/2026 · Inventario"
    EmployeeForm.describeOverride(overrides(1)) shouldBe "Ferie 10/08/2026 → 21/08/2026"
    EmployeeForm.describeOverride(
      HoursOverrideDto("vacation", None, None, None, Some("2026-05-04T00:00:00.000Z"), Some("2026-05-04T00:00:00.000Z")),
    ) shouldBe
      "Ferie il 04/05/2026"
end EmployeeFormSpec
