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
    TaskResponse(id, s"task-$id", None, hours)

  private def catalog(
      tasks: List[TaskResponse],
      code: String = "MFG-1",
      dependencies: List[ManufacturingCatalogDependencyDto] = Nil,
      defaultEmployees: List[TaskDefaultEmployeeDto] = Nil,
      description: Option[String] = Some("catalog desc"),
  ): ManufacturingCatalogResponse =
    ManufacturingCatalogResponse(
      id = uuid("00000000-0000-0000-0000-0000000000c1"),
      code = code,
      name = "Catalog",
      description = description,
      taskIds = tasks.map(_.id),
      tasks = tasks,
      dependencies = dependencies,
      totalRequiredHours = tasks.map(_.requiredHours).sum,
      defaultEmployees = defaultEmployees,
    )

  private def core(
      mode: String = "custom",
      catalogId: String = "",
      code: String = "C1",
      completionDate: String = "2026-08-01",
      description: String = "",
      employeeId: String = "",
      dependsOn: Set[String] = Set.empty,
      taskEmployees: Map[String, String] = Map.empty,
  ): MfgCoreState =
    MfgCoreState(mode, catalogId, code, completionDate, description, employeeId, dependsOn, taskEmployees)

  // ---- fromCatalog -------------------------------------------------------------------------------

  test("fromCatalog copies template code/description/dependencies and converts the completion date to ISO"):
    val template = catalog(
      code = "MFG-9",
      tasks = List(taskResponse(taskA, 4), taskResponse(taskB, 6)),
      dependencies = List(ManufacturingCatalogDependencyDto(taskB, List(taskA))),
    )
    val dto = OrderDrafts.fromCatalog(template, "2026-08-01", "", Map.empty, seqIds())

    dto.code shouldBe "MFG-9"
    dto.status shouldBe "not_started"
    dto.completionDate shouldBe "2026-08-01T00:00:00.000Z"
    dto.description shouldBe Some("catalog desc")
    dto.tasks.map(_.taskId) shouldBe List(taskA, taskB)
    dto.tasks.map(_.expectedHours) shouldBe List(4, 6)
    dto.tasks.map(_.status).distinct shouldBe List("pending")
    dto.dependencies shouldBe List(TaskDependencyDto(taskB, List(taskA)))

  test("fromCatalog mints a fresh id per scheduled task via the injected generator"):
    val template = catalog(tasks = List(taskResponse(taskA, 4), taskResponse(taskB, 6)))
    val dto = OrderDrafts.fromCatalog(template, "2026-08-01", "", Map.empty, seqIds())
    dto.tasks.map(_.id) shouldBe List(newId(0), newId(1))

  test("fromCatalog assigns per-task preferred employees from the taskEmployees overrides, ignoring unmatched keys"):
    val template = catalog(tasks = List(taskResponse(taskA, 4), taskResponse(taskB, 6)))
    val overrides = Map(taskA.toString -> empA.toString, "not-a-uuid" -> empB.toString)
    val dto = OrderDrafts.fromCatalog(template, "2026-08-01", "", overrides, seqIds())

    dto.tasks.find(_.taskId == taskA).flatMap(_.preferredEmployeeId) shouldBe Some(empA)
    dto.tasks.find(_.taskId == taskB).flatMap(_.preferredEmployeeId) shouldBe None

  test("fromCatalog parses the manufacturing-level preferred employee, dropping an invalid one"):
    val template = catalog(tasks = List(taskResponse(taskA, 4)))
    OrderDrafts.fromCatalog(template, "2026-08-01", empB.toString, Map.empty, seqIds()).preferredEmployeeId shouldBe Some(empB)
    OrderDrafts.fromCatalog(template, "2026-08-01", "garbage", Map.empty, seqIds()).preferredEmployeeId shouldBe None

  // ---- fromCustom --------------------------------------------------------------------------------

  test("fromCustom keeps only tasks with a valid id and parses hours, defaulting unparseable hours to 0"):
    val tasks = List(
      TaskState(taskA.toString, "8", Set.empty, ""),
      TaskState("", "5", Set.empty, ""), // no task id -> dropped
      TaskState(taskB.toString, "abc", Set.empty, ""), // bad hours -> 0
    )
    val dto = OrderDrafts.fromCustom(core(code = " C7 "), tasks, "2026-08-01", seqIds())

    dto.code shouldBe "C7" // trimmed
    dto.tasks.map(_.taskId) shouldBe List(taskA, taskB)
    dto.tasks.map(_.expectedHours) shouldBe List(8, 0)

  test("fromCustom keeps dependencies only between selected tasks and drops self-edges"):
    val tasks = List(
      TaskState(taskA.toString, "8", Set.empty, ""),
      TaskState(taskB.toString, "8", Set(taskA.toString, taskB.toString, taskUnknown.toString), ""),
    )
    val dto = OrderDrafts.fromCustom(core(), tasks, "2026-08-01", seqIds())

    // taskB depends on taskA (selected); its self-edge and the unknown/unselected id are dropped.
    dto.dependencies shouldBe List(TaskDependencyDto(taskB, List(taskA)))

  test("fromCustom collapses a blank description to None and parses the preferred employee"):
    val withDesc = OrderDrafts.fromCustom(core(description = "  built to order  ", employeeId = empA.toString), Nil, "2026-08-01", seqIds())
    withDesc.description shouldBe Some("built to order")
    withDesc.preferredEmployeeId shouldBe Some(empA)

    val blank = OrderDrafts.fromCustom(core(description = "   "), Nil, "2026-08-01", seqIds())
    blank.description shouldBe None

  // ---- manufacturingFromDraft --------------------------------------------------------------------

  test("manufacturingFromDraft falls back to the order deadline when the draft completion date is blank"):
    val snapshot = MfgSnapshot(1, core(mode = "custom", completionDate = "  "), Nil)
    val dto = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", _ => None, seqIds())
    dto.map(_.completionDate) shouldBe Right("2026-09-15T00:00:00.000Z")

  test("manufacturingFromDraft returns an invalid-form error when a catalog draft references an unknown catalog"):
    val snapshot = MfgSnapshot(1, core(mode = "catalog", catalogId = "missing"), Nil)
    val result = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", _ => None, seqIds())
    result.left.map(_.code) shouldBe Left("invalid-form")

  test("manufacturingFromDraft resolves a catalog draft through the lookup function"):
    val template = catalog(code = "FROM-CAT", tasks = List(taskResponse(taskA, 3)))
    val snapshot = MfgSnapshot(1, core(mode = "catalog", catalogId = "known"), Nil)
    val result = OrderDrafts.manufacturingFromDraft(snapshot, "2026-09-15", id => Option.when(id == "known")(template), seqIds())
    result.map(_.code) shouldBe Right("FROM-CAT")

  // ---- collectManufacturings ---------------------------------------------------------------------

  test("collectManufacturings preserves order and returns all DTOs when every draft is valid"):
    val snapshots = List(
      MfgSnapshot(1, core(code = "A"), List(TaskState(taskA.toString, "8", Set.empty, ""))),
      MfgSnapshot(2, core(code = "B"), List(TaskState(taskB.toString, "8", Set.empty, ""))),
    )
    val result = OrderDrafts.collectManufacturings(snapshots, "2026-09-15", _ => None, seqIds())
    result.map(_.map(_.code)) shouldBe Right(List("A", "B"))

  test("collectManufacturings short-circuits on the first invalid draft"):
    val snapshots = List(
      MfgSnapshot(1, core(code = "A"), Nil),
      MfgSnapshot(2, core(mode = "catalog", catalogId = "missing"), Nil),
    )
    val result = OrderDrafts.collectManufacturings(snapshots, "2026-09-15", _ => None, seqIds())
    result.isLeft shouldBe true

  // ---- collectDependencies -----------------------------------------------------------------------

  test("collectDependencies maps draft-key selections to positional indexes"):
    val snapshots = List(
      MfgSnapshot(10, core(dependsOn = Set.empty), Nil),
      MfgSnapshot(20, core(dependsOn = Set("10")), Nil), // index 1 depends on index 0
      MfgSnapshot(30, core(dependsOn = Set("10", "20")), Nil), // index 2 depends on 0 and 1
    )
    OrderDrafts.collectDependencies(snapshots) shouldBe Some(
      List(
        ManufacturingDependencyByIndexDto(1, List(0)),
        ManufacturingDependencyByIndexDto(2, List(0, 1)),
      ),
    )

  test("collectDependencies drops self-references and unknown keys, and returns None when nothing remains"):
    val snapshots = List(
      MfgSnapshot(10, core(dependsOn = Set("10", "999")), Nil), // self + unknown only
      MfgSnapshot(20, core(dependsOn = Set.empty), Nil),
    )
    OrderDrafts.collectDependencies(snapshots) shouldBe None

end OrderDraftsSpec
