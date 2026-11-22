package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.FieldIssue
import io.gitbub.nicolasfara.rstmanager.ui.ManufacturingCatalogForm.{ Field, TaskRow }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure manufacturing-catalog form: validation, total duration, request building and loading a stored template. */
final class ManufacturingCatalogFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[ManufacturingCatalogRequest, ManufacturingCatalogRequest] = CanEqual.derived

  private val cut = "00000000-0000-0000-0000-000000000601"
  private val sew = "00000000-0000-0000-0000-000000000602"
  private val employee = "00000000-0000-0000-0000-000000000701"
  private def uuid(value: String): UUID = UUID.fromString(value).nn

  private def row(key: Int, taskId: String, hours: String): TaskRow = TaskRow(key, taskId, Set.empty, "", hours)
  private val base = ManufacturingCatalogForm.fresh("MFG-1", 1).copy(name = "Tendalino")

  test("a fresh form needs a name and at least one task"):
    ManufacturingCatalogForm.fresh("MFG-1", 1).errors shouldBe List("Nome obbligatorio", "Seleziona almeno un task")
    ManufacturingCatalogForm.fresh(" ", 1).issues.map(_.field) shouldBe List(Field.Code, Field.Name, Field.Tasks)

  test("a row without a task is an issue only when other rows exist"):
    base.copy(taskRows = List(row(1, cut, "1:00"))).errors shouldBe Nil
    base.copy(taskRows = List(row(1, cut, "1:00"), row(2, "", ""))).issues shouldBe
      List(FieldIssue(Field.RowTask(2), "Task 2: seleziona un task o rimuovi la riga"))

  test("a repeated task is flagged on the later row"):
    base.copy(taskRows = List(row(1, cut, "1:00"), row(2, sew, "0:30"), row(3, cut, "2:00"))).issues shouldBe
      List(FieldIssue(Field.RowTask(3), "Task 3: task già presente in questa lavorazione"))

  test("the duration of a picked task is required and must parse (0 is allowed)"):
    base.copy(taskRows = List(row(1, cut, " "))).issues.map(_.field) shouldBe List(Field.RowDuration(1))
    base.copy(taskRows = List(row(1, cut, "1:75"))).errors shouldBe List("Task 1: durata non valida (usa h:mm, es. 1:30, o ore intere)")
    base.copy(taskRows = List(row(1, cut, "0"))).errors shouldBe Nil

  test("the total duration sums the valid durations of the picked tasks"):
    base.copy(taskRows = List(row(1, cut, "1:30"), row(2, sew, "2"), row(3, "", "5:00"), row(4, sew, "x"))).totalMinutes shouldBe 210

  test("request sends a duration override only when it differs from the catalog, and drops dangling dependencies"):
    val state = base.copy(
      description = "  ",
      taskRows = List(
        TaskRow(1, cut, Set.empty, employee, "1:00"),
        TaskRow(2, sew, Set(cut, "00000000-0000-0000-0000-000000000999"), "", "0:45"),
      ),
    )
    state.request(Map(cut -> 60, sew -> 30)) shouldBe ManufacturingCatalogRequest(
      "MFG-1",
      "Tendalino",
      None,
      List(uuid(cut), uuid(sew)),
      List(ManufacturingCatalogDependencyDto(uuid(cut), Nil), ManufacturingCatalogDependencyDto(uuid(sew), List(uuid(cut)))),
      Some(List(TaskDefaultEmployeeDto(uuid(cut), uuid(employee)))),
      Some(List(TaskDurationOverrideDto(uuid(sew), 45))),
    )

  test("fromResponse loads one row per task with the override or catalog duration"):
    val response = ManufacturingCatalogResponse(
      uuid("00000000-0000-0000-0000-000000000801"),
      "MFG-7",
      "Capottina",
      Some("Su misura"),
      List(uuid(cut), uuid(sew)),
      List(TaskResponse(uuid(cut), "Taglio", None, 90, None), TaskResponse(uuid(sew), "Cucitura", None, 120, None)),
      List(ManufacturingCatalogDependencyDto(uuid(sew), List(uuid(cut)))),
      0,
      List(TaskDefaultEmployeeDto(uuid(cut), uuid(employee))),
      List(TaskDurationOverrideDto(uuid(sew), 150)),
    )
    var key = 10
    val state = ManufacturingCatalogForm.fromResponse(
      response,
      () =>
        key += 1; key,
    )
    state.editingId shouldBe Some(response.id)
    (state.code, state.name, state.description) shouldBe ("MFG-7", "Capottina", "Su misura")
    state.taskRows shouldBe List(TaskRow(11, cut, Set.empty, employee, "1:30"), TaskRow(12, sew, Set(cut), "", "2:30"))
    state.errors shouldBe Nil
end ManufacturingCatalogFormSpec
