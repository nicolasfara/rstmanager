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
  private given CanEqual[CatalogMaterialDto, CatalogMaterialDto] = CanEqual.derived

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
    state.request(Map(cut -> 60, sew -> 30), MaterialDrafts.Context.empty) shouldBe Right(
      ManufacturingCatalogRequest(
        "MFG-1",
        "Tendalino",
        None,
        List(uuid(cut), uuid(sew)),
        List(ManufacturingCatalogDependencyDto(uuid(cut), Nil), ManufacturingCatalogDependencyDto(uuid(sew), List(uuid(cut)))),
        Some(List(TaskDefaultEmployeeDto(uuid(cut), uuid(employee)))),
        Some(List(TaskDurationOverrideDto(uuid(sew), 45))),
        Some(Nil),
      ),
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
    state.materials shouldBe Nil

  // ---- per-unit materials ------------------------------------------------------------------------

  private val fabric = uuid("00000000-0000-0000-0000-000000000901")
  private val oldScrew = uuid("00000000-0000-0000-0000-000000000902")

  private val fabricMaterial: MaterialResponse =
    MaterialResponse(
      fabric,
      "TES-001",
      "Tessuto",
      None,
      None,
      "linear_meters",
      "18.5000",
      Nil,
      Nil,
      "2026-10-01",
      false,
      None,
      StockDto("10.000", None, None, "0.00", false),
    )

  private val storedLines = List(
    CatalogMaterialResponse(fabric, Some("TES-001"), Some("Tessuto"), Some("linear_meters"), false, "2.500"),
    CatalogMaterialResponse(oldScrew, Some("VIT-9"), Some("Vite"), Some("pieces"), true, "4.000"),
  )

  test("fromResponse loads the per-unit materials as price-less rows, and an archived stored line stays valid"):
    val response = ManufacturingCatalogResponse(uuid("00000000-0000-0000-0000-000000000801"), "MFG-7", "Capottina", None, Nil, Nil, Nil, 0)
      .copy(materials = storedLines)
    var key = 0
    val state = ManufacturingCatalogForm.fromResponse(
      response,
      () =>
        key += 1; key,
    )
    state.materials shouldBe List(MaterialDrafts.Row(1, fabric.toString, "2,5", ""), MaterialDrafts.Row(2, oldScrew.toString, "4", ""))
    val context = MaterialDrafts.Context.ofCatalog(List(fabricMaterial), storedLines)
    context.material(oldScrew).map(_.archived) shouldBe Some(true)
    state.materialIssues(context, true) shouldBe Nil
    state.request(Map.empty, context).map(_.materials) shouldBe
      Right(Some(List(CatalogMaterialDto(fabric, "2.500"), CatalogMaterialDto(oldScrew, "4.000"))))
    // The warehouse failed to load: the stored lines are still known to the context, so the template can be saved unchanged.
    val stored = MaterialDrafts.Context.ofCatalog(Nil, storedLines)
    state.materialIssues(stored, false) shouldBe Nil
    state.request(Map.empty, stored).map(_.materials.map(_.size)) shouldBe Right(Some(2))

  test("materials are validated without a price, blank rows are not sent, and no line sends an explicit empty list (clears on update)"):
    val context = MaterialDrafts.Context.ofCatalog(List(fabricMaterial), Nil)
    val withRows = base.copy(materials = List(MaterialDrafts.Row(1, fabric.toString, "0", "")))
    withRows.materialIssues(context, true) shouldBe List(FieldIssue(Field.Materials, "Materiali — Riga 1: la quantità deve essere maggiore di zero"))
    base.copy(materials = List(MaterialDrafts.Row.empty(1))).request(Map.empty, context).map(_.materials) shouldBe Right(Some(Nil))
    base.request(Map.empty, context).map(_.materials) shouldBe Right(Some(Nil))
    base.copy(materials = List(MaterialDrafts.Row(1, fabric.toString, "1,25", "99"))).request(Map.empty, context).map(_.materials) shouldBe
      Right(Some(List(CatalogMaterialDto(fabric, "1.250"))))

  test("while the warehouse is not loaded a line that cannot be checked blocks the submit, and no request drops it"):
    val pending = base.copy(materials = List(MaterialDrafts.Row(1, fabric.toString, "2", "")))
    pending.materialIssues(MaterialDrafts.Context.empty, false) shouldBe List(
      FieldIssue(Field.Materials, "Materiali — elenco del magazzino non disponibile: attendi il caricamento o ricarica la pagina prima di salvare"),
      FieldIssue(Field.Materials, "Materiali — Riga 1: il materiale selezionato non è più disponibile"),
    )
    pending.request(Map.empty, MaterialDrafts.Context.empty).isLeft shouldBe true
end ManufacturingCatalogFormSpec
