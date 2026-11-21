package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import OrderDrafts.*

/** Unit tests for the pure draft->DTO transforms. UUID minting is injected so results are deterministic. */
final class OrderDraftsSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares these DTOs structurally, so it needs CanEqual instances.
  private given CanEqual[TaskDependencyDto, TaskDependencyDto] = CanEqual.derived
  private given CanEqual[ManufacturingDependencyByIndexDto, ManufacturingDependencyByIndexDto] = CanEqual.derived
  private given CanEqual[MaterialLineDto, MaterialLineDto] = CanEqual.derived

  private def uuid(s: String): UUID = UUID.fromString(s).nn

  // Fixed ids so assertions can reference them by name.
  private val taskA = uuid("00000000-0000-0000-0000-0000000000a1")
  private val taskB = uuid("00000000-0000-0000-0000-0000000000b2")
  private val taskUnknown = uuid("00000000-0000-0000-0000-0000000000cc")
  private val empA = uuid("00000000-0000-0000-0000-0000000000e1")
  private val empB = uuid("00000000-0000-0000-0000-0000000000e2")

  private def newId(n: Int): UUID = uuid(f"00000000-0000-0000-0000-$n%012d")

  /** Deterministic, sequential id generator: id 0, 1, 2, ... so minted task ids are predictable. */
  private def seqIds(): () => UUID =
    val counter = Iterator.from(0)
    () => newId(counter.next())

  private def taskResponse(id: UUID, hours: Int): TaskResponse =
    TaskResponse(id, s"task-$id", None, hours, None)

  // Base fixtures; individual tests derive the variant they need via `.copy` (project style forbids method default args).
  private val baseCatalog: ManufacturingCatalogResponse =
    ManufacturingCatalogResponse(
      id = uuid("00000000-0000-0000-0000-0000000000c1"),
      code = "MFG-1",
      name = "Catalog",
      description = Some("catalog desc"),
      taskIds = Nil,
      tasks = Nil,
      dependencies = Nil,
      totalRequiredMinutes = 0,
      defaultEmployees = Nil,
    )

  /** A catalog carrying `tasks`, with the derived `taskIds`/`totalRequiredMinutes` kept consistent. */
  private def catalogWith(tasks: List[TaskResponse]): ManufacturingCatalogResponse =
    baseCatalog.copy(tasks = tasks, taskIds = tasks.map(_.id), totalRequiredMinutes = tasks.map(_.requiredMinutes).sum)

  private val baseCore: MfgCoreState =
    MfgCoreState("custom", "", "C1", "2026-08-01", "", "", Set.empty, Map.empty)

  // ---- fromCatalog -------------------------------------------------------------------------------

  test("fromCatalog copies template code/description/dependencies and converts the completion date to ISO"):
    val template = catalogWith(List(taskResponse(taskA, 4), taskResponse(taskB, 6)))
      .copy(code = "MFG-9", dependencies = List(ManufacturingCatalogDependencyDto(taskB, List(taskA))))
    val dto = OrderDrafts.fromCatalog(template, "2026-08-01", "", Map.empty, seqIds())

    dto.code shouldBe "MFG-9"
    dto.status shouldBe "not_started"
    dto.completionDate shouldBe "2026-08-01T00:00:00.000Z"
    dto.description shouldBe Some("catalog desc")
    dto.tasks.map(_.taskId) shouldBe List(taskA, taskB)
    dto.tasks.map(_.expectedMinutes) shouldBe List(4, 6)
    dto.tasks.map(_.status).distinct shouldBe List("pending")
    dto.dependencies shouldBe List(TaskDependencyDto(taskB, List(taskA)))

  test("fromCatalog mints a fresh id per scheduled task via the injected generator"):
    val template = catalogWith(List(taskResponse(taskA, 4), taskResponse(taskB, 6)))
    val dto = OrderDrafts.fromCatalog(template, "2026-08-01", "", Map.empty, seqIds())
    dto.tasks.map(_.id) shouldBe List(newId(0), newId(1))

  test("fromCatalog assigns per-task preferred employees from the taskEmployees overrides, ignoring unmatched keys"):
    val template = catalogWith(List(taskResponse(taskA, 4), taskResponse(taskB, 6)))
    val overrides = Map(taskA.toString -> empA.toString, "not-a-uuid" -> empB.toString)
    val dto = OrderDrafts.fromCatalog(template, "2026-08-01", "", overrides, seqIds())

    dto.tasks.find(_.taskId == taskA).flatMap(_.preferredEmployeeId) shouldBe Some(empA)
    dto.tasks.find(_.taskId == taskB).flatMap(_.preferredEmployeeId) shouldBe None

  test("fromCatalog applies the catalog duration override per task, falling back to the task-catalog duration"):
    val template = catalogWith(List(taskResponse(taskA, 4), taskResponse(taskB, 6)))
      .copy(taskDurations = List(TaskDurationOverrideDto(taskA, 10)))
    val dto = OrderDrafts.fromCatalog(template, "2026-08-01", "", Map.empty, seqIds())

    dto.tasks.find(_.taskId == taskA).map(_.expectedMinutes) shouldBe Some(10) // overridden
    dto.tasks.find(_.taskId == taskB).map(_.expectedMinutes) shouldBe Some(6) // falls back to catalog duration

  test("fromCatalog parses the manufacturing-level preferred employee, dropping an invalid one"):
    val template = catalogWith(List(taskResponse(taskA, 4)))
    OrderDrafts.fromCatalog(template, "2026-08-01", empB.toString, Map.empty, seqIds()).preferredEmployeeId shouldBe Some(empB)
    OrderDrafts.fromCatalog(template, "2026-08-01", "garbage", Map.empty, seqIds()).preferredEmployeeId shouldBe None

  // ---- fromCustom --------------------------------------------------------------------------------

  test("fromCustom keeps only tasks with a valid id and parses the h:mm duration, defaulting unparseable input to 0"):
    val tasks = List(
      TaskState(taskA.toString, "8", Set.empty, ""), // plain integer -> whole hours -> 480 minutes
      TaskState("", "5", Set.empty, ""), // no task id -> dropped
      TaskState(taskB.toString, "abc", Set.empty, ""), // bad duration -> 0
    )
    val dto = OrderDrafts.fromCustom(baseCore.copy(code = " C7 "), tasks, "2026-08-01", seqIds())

    dto.code shouldBe "C7" // trimmed
    dto.tasks.map(_.taskId) shouldBe List(taskA, taskB)
    dto.tasks.map(_.expectedMinutes) shouldBe List(480, 0)

  test("fromCustom keeps dependencies only between selected tasks and drops self-edges"):
    val tasks = List(
      TaskState(taskA.toString, "8", Set.empty, ""),
      TaskState(taskB.toString, "8", Set(taskA.toString, taskB.toString, taskUnknown.toString), ""),
    )
    val dto = OrderDrafts.fromCustom(baseCore, tasks, "2026-08-01", seqIds())

    // taskB depends on taskA (selected); its self-edge and the unknown/unselected id are dropped.
    dto.dependencies shouldBe List(TaskDependencyDto(taskB, List(taskA)))

  test("fromCustom collapses a blank description to None and parses the preferred employee"):
    val withDesc =
      OrderDrafts.fromCustom(baseCore.copy(description = "  built to order  ", employeeId = empA.toString), Nil, "2026-08-01", seqIds())
    withDesc.description shouldBe Some("built to order")
    withDesc.preferredEmployeeId shouldBe Some(empA)

    val blank = OrderDrafts.fromCustom(baseCore.copy(description = "   "), Nil, "2026-08-01", seqIds())
    blank.description shouldBe None

  // ---- manufacturingFromDraft --------------------------------------------------------------------

  test("manufacturingFromDraft falls back to the order deadline when the draft completion date is blank"):
    val snapshot = MfgSnapshot(1, baseCore.copy(completionDate = "  "), Nil)
    val dto = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", _ => None, seqIds())
    dto.map(_.completionDate) shouldBe Right("2026-09-15T00:00:00.000Z")

  test("manufacturingFromDraft returns an invalid-form error when a catalog draft references an unknown catalog"):
    val snapshot = MfgSnapshot(1, baseCore.copy(mode = "catalog", catalogId = "missing"), Nil)
    val result = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", _ => None, seqIds())
    result.left.map(_.code) shouldBe Left("invalid-form")

  test("manufacturingFromDraft resolves a catalog draft through the lookup function"):
    val template = catalogWith(List(taskResponse(taskA, 3))).copy(code = "FROM-CAT")
    val snapshot = MfgSnapshot(1, baseCore.copy(mode = "catalog", catalogId = "known"), Nil)
    val result = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", id => Option.when(id == "known")(template), seqIds())
    result.map(_.code) shouldBe Right("FROM-CAT")

  // ---- collectManufacturings ---------------------------------------------------------------------

  test("collectManufacturings preserves order and returns all DTOs when every draft is valid"):
    val snapshots = List(
      MfgSnapshot(1, baseCore.copy(code = "A"), List(TaskState(taskA.toString, "8", Set.empty, ""))),
      MfgSnapshot(2, baseCore.copy(code = "B"), List(TaskState(taskB.toString, "8", Set.empty, ""))),
    )
    val result = OrderDrafts.collectManufacturings(snapshots, "2026-09-15", _ => None, seqIds())
    result.map(_.map(_.code)) shouldBe Right(List("A", "B"))

  test("collectManufacturings short-circuits on the first invalid draft"):
    val snapshots = List(
      MfgSnapshot(1, baseCore.copy(code = "A"), Nil),
      MfgSnapshot(2, baseCore.copy(mode = "catalog", catalogId = "missing"), Nil),
    )
    val result = OrderDrafts.collectManufacturings(snapshots, "2026-09-15", _ => None, seqIds())
    result.isLeft shouldBe true

  // ---- materials ---------------------------------------------------------------------------------

  private val materialId = uuid("00000000-0000-0000-0000-000000000f01")

  private val materialsContext: MaterialDrafts.Context =
    MaterialDrafts.Context(List(MaterialDrafts.Material(materialId, "TES-001", "Tessuto", "linear_meters", Some("18.5000"), false)), Map.empty)

  test("manufacturingFromDraft omits materials when the draft has no non-blank line"):
    val snapshot = MfgSnapshot(1, baseCore, Nil, List(MaterialDrafts.Row.empty(1)))
    OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", _ => None, materialsContext, seqIds()).map(_.materials) shouldBe Right(None)

  test("manufacturingFromDraft attaches the material lines, without a price unless one was typed"):
    val snapshot = MfgSnapshot(1, baseCore, Nil, List(MaterialDrafts.Row(1, materialId.toString, "2,5", "")))
    val result = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", _ => None, materialsContext, seqIds())
    result.map(_.materials) shouldBe Right(Some(List(MaterialLineDto(materialId, "2.500", None))))

  test("manufacturingFromDraft reports invalid material lines as an invalid-form error naming the manufacturing"):
    val snapshot = MfgSnapshot(1, baseCore, Nil, List(MaterialDrafts.Row(1, materialId.toString, "0", "")))
    val result = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", _ => None, materialsContext, seqIds())
    result.left.map(err => (err.code, err.message, err.details)) shouldBe
      Left(("invalid-form", "Materiali della lavorazione C1 non validi.", List("Riga 1: la quantità deve essere maggiore di zero")))

  test("the overloads without materials context reject any material line"):
    val snapshot = MfgSnapshot(1, baseCore, Nil, List(MaterialDrafts.Row(1, materialId.toString, "1", "")))
    OrderDrafts.collectManufacturings(List(snapshot), "2026-09-15", _ => None, seqIds()).isLeft shouldBe true

  // ---- collectDependencies -----------------------------------------------------------------------

  test("collectDependencies maps draft-key selections to positional indexes"):
    val snapshots = List(
      MfgSnapshot(10, baseCore, Nil),
      MfgSnapshot(20, baseCore.copy(dependsOn = Set("10")), Nil), // index 1 depends on index 0
      MfgSnapshot(30, baseCore.copy(dependsOn = Set("10", "20")), Nil), // index 2 depends on 0 and 1
    )
    OrderDrafts.collectDependencies(snapshots) shouldBe Some(
      List(
        ManufacturingDependencyByIndexDto(1, List(0)),
        ManufacturingDependencyByIndexDto(2, List(0, 1)),
      ),
    )

  test("collectDependencies drops self-references and unknown keys, and returns None when nothing remains"):
    val snapshots = List(
      MfgSnapshot(10, baseCore.copy(dependsOn = Set("10", "999")), Nil), // self + unknown only
      MfgSnapshot(20, baseCore, Nil),
    )
    OrderDrafts.collectDependencies(snapshots) shouldBe None

  // ---- header ------------------------------------------------------------------------------------

  private val customerId = uuid("00000000-0000-0000-0000-0000000000d1")

  private val validHeader: HeaderState =
    HeaderState.initial("ORD-1", "2026-07-01").copy(customerId = customerId.toString).withDelivery("2026-09-10")

  test("withDelivery proposes the work deadline 5 days earlier, across month and year boundaries"):
    val header = HeaderState.initial("ORD-1", "2026-07-01")
    header.withDelivery("2026-09-10").workDeadline shouldBe "2026-09-05"
    header.withDelivery("2026-03-03").workDeadline shouldBe "2026-02-26"
    header.withDelivery("2024-03-03").workDeadline shouldBe "2024-02-27" // leap year
    header.withDelivery("2027-01-02").workDeadline shouldBe "2026-12-28"
    header.withDelivery("").workDeadline shouldBe ""

  test("withDelivery stops following once the deadline is edited by hand, and withProposedDeadline resumes it"):
    val edited = HeaderState.initial("ORD-1", "2026-07-01").withDelivery("2026-09-10").withDeadline("2026-08-01")
    edited.workDeadlineManuallyEdited shouldBe true
    edited.withDelivery("2026-10-20") shouldBe edited.copy(deliveryDate = "2026-10-20")
    val restored = edited.withDelivery("2026-10-20").withProposedDeadline
    restored.workDeadline shouldBe "2026-10-15"
    restored.workDeadlineManuallyEdited shouldBe false
    restored.withDelivery("2026-11-10").workDeadline shouldBe "2026-11-05"

  test("withGeneratedNumber refreshes the number only until it is typed"):
    val header = HeaderState.initial("ORD-1", "2026-07-01")
    header.withGeneratedNumber("ORD-2").number shouldBe "ORD-2"
    header.withNumber("MIO-1").withGeneratedNumber("ORD-2").number shouldBe "MIO-1"

  test("deadlineAfterDelivery flags a work deadline later than the delivery date"):
    OrderDrafts.deadlineAfterDelivery(validHeader) shouldBe false
    OrderDrafts.deadlineAfterDelivery(validHeader.withDeadline("2026-09-11")) shouldBe true
    OrderDrafts.deadlineAfterDelivery(validHeader.copy(deliveryDate = "")) shouldBe false

  test("isPristine holds for the initial header without manufacturings only"):
    val initial = HeaderState.initial("ORD-1", "2026-07-01")
    OrderDrafts.isPristine(initial, "2026-07-01", Nil) shouldBe true
    OrderDrafts.isPristine(initial.withGeneratedNumber("ORD-2"), "2026-07-01", Nil) shouldBe true // a regenerated number is not an edit
    OrderDrafts.isPristine(initial.withNumber("ORD-2"), "2026-07-01", Nil) shouldBe false
    OrderDrafts.isPristine(initial.copy(description = "x"), "2026-07-01", Nil) shouldBe false
    OrderDrafts.isPristine(initial.copy(priority = "urgent"), "2026-07-01", Nil) shouldBe false
    OrderDrafts.isPristine(initial.copy(creationDate = "2026-06-30"), "2026-07-01", Nil) shouldBe false
    OrderDrafts.isPristine(initial, "2026-07-01", List(MfgSnapshot(1, baseCore, Nil))) shouldBe false

  // ---- validation --------------------------------------------------------------------------------

  private def targets(issues: List[Issue]): List[IssueTarget] = issues.map(_.target)

  test("headerIssues reports every missing header field, in form order"):
    val issues = OrderDrafts.headerIssues(HeaderState.initial(" ", "").copy(priority = "normal"))
    targets(issues) shouldBe List(
      IssueTarget.Header(HeaderField.Number),
      IssueTarget.Header(HeaderField.Customer),
      IssueTarget.Header(HeaderField.CreationDate),
      IssueTarget.Header(HeaderField.DeliveryDate),
      IssueTarget.Header(HeaderField.WorkDeadline),
    )
    issues.map(_.message) shouldBe List(
      "Numero ordine obbligatorio",
      "Cliente obbligatorio",
      "Data di creazione obbligatoria",
      "Data di consegna cliente obbligatoria",
      "Deadline fine lavorazione obbligatoria",
    )

  test("headerIssues rejects a customer that is not a valid id, and accepts a complete header"):
    OrderDrafts.headerIssues(validHeader.copy(customerId = "nope")).map(_.message) shouldBe List("Seleziona un cliente valido dall'elenco")
    OrderDrafts.headerIssues(validHeader) shouldBe Nil

  private val validCustom: MfgSnapshot = MfgSnapshot(7, baseCore, List(TaskState(taskA.toString, "1:30", Set.empty, "")))

  private def mfgIssues(snapshot: MfgSnapshot): List[Issue] =
    OrderDrafts.manufacturingIssues(snapshot, 0, _ => None, MaterialDrafts.Context.empty)

  test("a custom manufacturing needs a code and at least one task"):
    val issues = mfgIssues(MfgSnapshot(7, baseCore.copy(code = "  "), Nil))
    targets(issues) shouldBe List(IssueTarget.Manufacturing(7, MfgField.Code), IssueTarget.Manufacturing(7, MfgField.Tasks))
    issues.map(_.message) shouldBe List("Lavorazione 1: codice obbligatorio", "Lavorazione 1: aggiungi almeno un task")

  test("every custom task row needs a selected task and a readable duration, 0 allowed"):
    val rows = List(
      TaskState("", "1:00", Set.empty, ""), // no task
      TaskState(taskA.toString, "1:75", Set.empty, ""), // bad minutes
      TaskState(taskB.toString, "", Set.empty, ""), // blank duration
      TaskState(taskUnknown.toString, "0", Set.empty, ""), // zero is fine
    )
    val issues = OrderDrafts.manufacturingIssues(MfgSnapshot(7, baseCore, rows), 1, _ => None, MaterialDrafts.Context.empty)
    targets(issues) shouldBe List(
      IssueTarget.Task(7, 0, TaskField.Task),
      IssueTarget.Task(7, 1, TaskField.Duration),
      IssueTarget.Task(7, 2, TaskField.Duration),
    )
    issues.map(_.message) shouldBe List(
      "Lavorazione 2, task 1: seleziona un task",
      "Lavorazione 2, task 2: durata non valida (usa h:mm, es. 1:30, o ore intere)",
      "Lavorazione 2, task 3: durata obbligatoria",
    )

  test("a task repeated in the same custom manufacturing is an error on the later rows"):
    val rows = List.fill(3)(TaskState(taskA.toString, "1", Set.empty, ""))
    targets(mfgIssues(MfgSnapshot(7, baseCore, rows))) shouldBe List(IssueTarget.Task(7, 1, TaskField.Task), IssueTarget.Task(7, 2, TaskField.Task))
    mfgIssues(MfgSnapshot(7, baseCore, rows)).map(_.message).head shouldBe "Lavorazione 1, task 2: task già presente in questa lavorazione"

  test("a catalog manufacturing needs a resolvable template and ignores the custom fields"):
    val catalogCore = baseCore.copy(mode = "catalog", code = "")
    targets(mfgIssues(MfgSnapshot(7, catalogCore, Nil))) shouldBe List(IssueTarget.Manufacturing(7, MfgField.Catalog))
    mfgIssues(MfgSnapshot(7, catalogCore, Nil)).map(_.message) shouldBe List("Lavorazione 1: seleziona la lavorazione a catalogo")
    mfgIssues(MfgSnapshot(7, catalogCore.copy(catalogId = "gone"), Nil)).map(_.message) shouldBe
      List("Lavorazione 1: la lavorazione a catalogo selezionata non è disponibile")
    val template = catalogWith(List(taskResponse(taskA, 60)))
    OrderDrafts.manufacturingIssues(
      MfgSnapshot(7, catalogCore.copy(catalogId = "known"), Nil),
      0,
      _ => Some(template),
      MaterialDrafts.Context.empty,
    ) shouldBe
      Nil

  test("material issues are reported per line, but not while the materials are still loading"):
    val snapshot = validCustom.copy(materials = List(MaterialDrafts.Row(1, materialId.toString, "0", "")))
    OrderDrafts.manufacturingIssues(snapshot, 0, _ => None, materialsContext) shouldBe List(
      Issue(IssueTarget.Manufacturing(7, MfgField.Materials), "Lavorazione 1, materiali — Riga 1: la quantità deve essere maggiore di zero"),
    )
    mfgIssues(snapshot) shouldBe Nil // empty context = not loaded yet
    mfgIssues(validCustom.copy(materials = List(MaterialDrafts.Row.empty(1)))) shouldBe Nil // blank lines are ignored

  test("validate orders header issues, then the missing manufacturings, then each manufacturing in sequence"):
    val header = validHeader.copy(number = "")
    targets(OrderDrafts.validate(header, Nil, _ => None, MaterialDrafts.Context.empty)) shouldBe
      List(IssueTarget.Header(HeaderField.Number), IssueTarget.NoManufacturings)
    val snapshots = List(validCustom.copy(key = 1, core = baseCore.copy(code = "")), validCustom.copy(key = 2, tasks = Nil))
    targets(OrderDrafts.validate(header, snapshots, _ => None, MaterialDrafts.Context.empty)) shouldBe List(
      IssueTarget.Header(HeaderField.Number),
      IssueTarget.Manufacturing(1, MfgField.Code),
      IssueTarget.Manufacturing(2, MfgField.Tasks),
    )

  test("a valid draft has no issues and collects into DTOs"):
    val snapshots = List(validCustom.copy(key = 1), validCustom.copy(key = 2, core = baseCore.copy(code = "C2")))
    OrderDrafts.validate(validHeader, snapshots, _ => None, materialsContext) shouldBe Nil
    OrderDrafts.collectManufacturings(snapshots, validHeader.workDeadline, _ => None, materialsContext, seqIds()).map(_.map(_.code)) shouldBe
      Right(List("C1", "C2"))

  // ---- totals ------------------------------------------------------------------------------------

  test("mfgTotals of a catalog manufacturing uses the overridden durations, and counts nothing while unresolved"):
    val template = catalogWith(List(taskResponse(taskA, 60), taskResponse(taskB, 30))).copy(taskDurations = List(TaskDurationOverrideDto(taskA, 90)))
    val snapshot = MfgSnapshot(1, baseCore.copy(mode = "catalog", catalogId = "known"), Nil)
    OrderDrafts.mfgTotals(snapshot, _ => Some(template), MaterialDrafts.Context.empty) shouldBe MfgTotals(2, 120, "0.00")
    OrderDrafts.mfgTotals(snapshot, _ => None, MaterialDrafts.Context.empty) shouldBe MfgTotals(0, 0, "0.00")

  test("mfgTotals of a custom manufacturing counts the selected tasks and their readable durations, plus the materials cost"):
    val rows = List(
      TaskState(taskA.toString, "1:30", Set.empty, ""),
      TaskState(taskB.toString, "bad", Set.empty, ""), // counted, no minutes
      TaskState("", "8", Set.empty, ""), // not selected: ignored
    )
    val snapshot = MfgSnapshot(1, baseCore, rows, List(MaterialDrafts.Row(1, materialId.toString, "2", "")))
    OrderDrafts.mfgTotals(snapshot, _ => None, materialsContext) shouldBe MfgTotals(2, 90, "37.00")

  test("orderTotals sums every manufacturing"):
    val template = catalogWith(List(taskResponse(taskA, 60)))
    val snapshots = List(
      MfgSnapshot(1, baseCore.copy(mode = "catalog", catalogId = "known"), Nil, List(MaterialDrafts.Row(1, materialId.toString, "1", ""))),
      MfgSnapshot(2, baseCore, List(TaskState(taskB.toString, "2", Set.empty, "")), List(MaterialDrafts.Row(2, materialId.toString, "0,5", ""))),
    )
    OrderDrafts.orderTotals(snapshots, _ => Some(template), materialsContext) shouldBe MfgTotals(2, 180, "27.75")

end OrderDraftsSpec
