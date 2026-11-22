package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import OrderEditDraft.*

/** Unit tests for the pure draft of the "Modifica ordine" page: rebase (B1), changes, issues (B2) and effects. */
final class OrderEditDraftSpec extends AnyFunSuite with Matchers:

  private given CanEqual[AddTaskRequest, AddTaskRequest] = CanEqual.derived

  private def uuid(n: Int): UUID = UUID.fromString(f"00000000-0000-0000-0000-$n%012d").nn

  private val orderId = uuid(1)
  private val mfgA = uuid(10)
  private val mfgB = uuid(11)
  private val taskA1 = uuid(20)
  private val taskA2 = uuid(21)
  private val taskB1 = uuid(22)
  private val cutting = uuid(30)
  private val sewing = uuid(31)
  private val material = uuid(40)

  private def keys(): () => Int =
    val counter = Iterator.from(1)
    () => counter.next()

  private val names: UUID => String = id => if id == cutting then "Taglio" else if id == sewing then "Cucitura" else "?"

  private def task(id: UUID, template: UUID, expected: Int, completed: Int): ScheduledTaskDto =
    ScheduledTaskDto(id, template, "pending", expected, Some(completed), None)

  private val line = MaterialLineResponse(material, Some("TES-001"), Some("Tessuto"), Some("linear_meters"), false, "2.000", "10.0000", "20.00")

  private val mA = ManufacturingResponse(
    mfgA,
    "MFG-01",
    "2026-08-01T00:00:00.000Z",
    "not_started",
    None,
    None,
    None,
    List(task(taskA1, cutting, 60, 0), task(taskA2, sewing, 120, 30)),
    List(TaskDependencyDto(sewing, List(cutting))),
    None,
    None,
    List(line),
    "20.00",
  )

  private val mB = ManufacturingResponse(
    mfgB,
    "MFG-02",
    "2026-08-02T00:00:00.000Z",
    "in_progress",
    None,
    None,
    None,
    List(task(taskB1, cutting, 90, 0)),
    Nil,
    Some("Seconda"),
  )

  private val order = OrderResponse(
    orderId,
    "ORD-2026-014",
    uuid(2),
    "in_progress",
    "normal",
    "2026-07-01T00:00:00.000Z",
    "2026-08-10T00:00:00.000Z",
    Some("2026-08-05T00:00:00.000Z"),
    List(mA, mB),
    Some("Descrizione"),
    List(ManufacturingDependencyDto(mfgB, List(mfgA))),
  )

  private val contexts: UUID => MaterialDrafts.Context = _ => MaterialDrafts.Context.of(Nil, List(line))

  private def withPriority(draft: Draft, priority: String): Draft = updateHeader(draft)(_.copy(priority = priority))

  test("a fresh draft has no changes, no issues and no effects"):
    val draft = fromOrder(order, keys())
    changes(draft, names) shouldBe Nil
    isDirty(draft) shouldBe false
    issues(draft, names, contexts) shouldBe Nil
    effects(draft, contexts) shouldBe Nil
    draft.header shouldBe HeaderValues("normal", "2026-08-05", "Descrizione")
    draft.mfgs.find(_.id == mfgB).map(_.current.dependsOn) shouldBe Some(Set(mfgA.toString))

  test("changes are labelled by section, manufacturing code and task name"):
    val edited = updateTask(updateMfg(withPriority(fromOrder(order, keys()), "urgent"), mfgB)(_.copy(status = "paused")), mfgA, taskA2)(
      _.copy(completed = "1:00"),
    )
    changes(edited, names).map(_.label) shouldBe List("Testata: priorità", "MFG-01 · Cucitura: ore fatte", "MFG-02: stato")
    isMfgDirty(edited.mfgs.head) shouldBe true

  test("B1: a structural response keeps the unsaved edits and adds the new task"):
    val draft = withPriority(fromOrder(order, keys()), "urgent")
    val added = order.copy(manufacturings = List(mA.copy(tasks = mA.tasks :+ task(uuid(23), uuid(32), 30, 0)), mB))
    val rebased = rebase(draft, added, keys())
    rebased.header.priority shouldBe "urgent"
    changes(rebased, names).map(_.label) shouldBe List("Testata: priorità")
    rebased.mfgs.head.tasks.map(_.id) shouldBe List(taskA1, taskA2, uuid(23))

  test("rebase takes the server values of the untouched fields and drops vanished rows and dependencies"):
    val draft = updateTask(fromOrder(order, keys()), mfgA, taskA1)(_.copy(expected = "2:00"))
    // Meanwhile the workshop recorded hours on another task, and MFG-02 was removed.
    val server = order.copy(
      manufacturings = List(mA.copy(tasks = List(task(taskA1, cutting, 60, 0), task(taskA2, sewing, 120, 90)))),
      dependencies = Nil,
      description = Some("Nuova"),
    )
    val rebased = rebase(draft, server, keys())
    rebased.mfgs.map(_.id) shouldBe List(mfgA)
    rebased.header.description shouldBe "Nuova"
    rebased.mfgs.head.tasks.map(_.current.completed) shouldBe List("0:00", "1:30")
    rebased.mfgs.head.tasks.head.current.expected shouldBe "2:00"
    changes(rebased, names).map(_.label) shouldBe List("MFG-01 · Taglio: ore previste")

  test("rebase drops a dependency on a manufacturing that no longer exists"):
    val draft = updateMfg(fromOrder(order, keys()), mfgA)(_.copy(dependsOn = Set(mfgB.toString)))
    val rebased = rebase(draft, order.copy(manufacturings = List(mA), dependencies = Nil), keys())
    rebased.mfgs.head.current.dependsOn shouldBe Set.empty[String]

  test("partial failure: the saved changes leave the list, the failed ones stay"):
    val draft = updateMfg(withPriority(fromOrder(order, keys()), "urgent"), mfgB)(_.copy(description = "Cambiata"))
    // The order update succeeded, the manufacturing update did not.
    val rebased = rebase(draft, order.copy(priority = "urgent"), keys())
    changes(rebased, names).map(_.label) shouldBe List("MFG-02: descrizione")
    rebased.header.priority shouldBe "urgent"

  test("B2: an unreadable duration is an inline issue and is never sent silently"):
    val draft = updateTask(fromOrder(order, keys()), mfgA, taskA2)(_.copy(completed = "1.30"))
    val found = issues(draft, names, contexts)
    found.map(i => (i.field, i.message)) shouldBe List(EditField.Task(mfgA, taskA2, TaskPart.Completed) -> "Usa il formato h:mm (es. 1:35)")
    found.head.text shouldBe "MFG-01 · Cucitura, ore fatte: usa il formato h:mm (es. 1:35)"
    changes(draft, names).map(_.field) shouldBe List(EditField.Task(mfgA, taskA2, TaskPart.Completed))

  test("planned hours need at least one minute; blank hours and a cleared deadline are issues"):
    val draft = updateMfg(updateTask(fromOrder(order, keys()), mfgA, taskA1)(_.copy(expected = "0:00", completed = " ")), mfgB)(
      _.copy(deadline = ""),
    )
    issues(draft, names, contexts).map(_.message) shouldBe List(expectedTooLow, durationRequired, "Deadline obbligatoria")
    issues(updateHeader(fromOrder(order, keys()))(_.copy(deadline = "")), names, contexts).map(_.field) shouldBe
      List(EditField.Header(HeaderPart.Deadline))

  test("effects send only what changed, in order, materials last"):
    val draft0 = fromOrder(order, keys())
    val edited = updateMaterials(
      updateTask(updateMfg(withPriority(draft0, "urgent"), mfgA)(_.copy(deadline = "2026-08-03")), mfgA, taskA2)(_.copy(completed = "1:00")),
      mfgA,
    )(_.map(_.copy(quantity = "3")))
    val ops = effects(edited, contexts)
    ops.take(3) shouldBe List(
      EditOp.UpdateOrder(OrderUpdateRequest(Some("urgent"), None, None)),
      EditOp.UpdateMfg(mfgA, ManufacturingUpdateRequest(None, Some("2026-08-03T00:00:00.000Z"), None, None)),
      EditOp.UpdateTask(mfgA, taskA2, TaskProgressUpdateRequest(Some(60), None)),
    )
    ops.drop(3) match
      case List(EditOp.Materials(id, List(MaterialLineDto(materialId, _, None)))) =>
        id shouldBe mfgA
        materialId shouldBe material
      case other => fail(s"unexpected $other")

  test("a blank extra material row is not a change"):
    val draft = updateMaterials(fromOrder(order, keys()), mfgA)(_ :+ MaterialDrafts.Row.empty(99))
    isDirty(draft) shouldBe false

  test("dependency edits replace the whole graph"):
    val draft = updateTask(updateMfg(fromOrder(order, keys()), mfgA)(_.copy(dependsOn = Set(mfgB.toString))), mfgA, taskA2)(
      _.copy(dependsOn = Set.empty),
    )
    effects(draft, contexts) shouldBe List(
      EditOp.OrderDependencies(
        OrderDependenciesUpdateRequest(List(ManufacturingDependencyDto(mfgA, List(mfgB)), ManufacturingDependencyDto(mfgB, List(mfgA)))),
      ),
      EditOp.TaskDependencies(mfgA, TaskDependenciesUpdateRequest(Nil)),
    )

  test("the inline + Task row needs a new task and a duration of at least one minute"):
    val present = Set(cutting)
    newTaskIssues(OrderDrafts.TaskState("", "1:00", Set.empty, ""), present).map(_._2) shouldBe List("Seleziona un task")
    newTaskIssues(OrderDrafts.TaskState(cutting.toString, "1.30", Set.empty, ""), present).map(_._2) shouldBe
      List("Task già presente in questa lavorazione", durationFormat)
    newTaskIssues(OrderDrafts.TaskState(sewing.toString, "0:45", Set.empty, ""), present) shouldBe Nil
    addTaskRequest(OrderDrafts.TaskState(sewing.toString, "0:45", Set.empty, "")) shouldBe Some(AddTaskRequest(sewing, 45, Nil, None))

  test("the ×N of a saved manufacturing comes from the server, follows a rebase and is never a change or an effect"):
    val tripled = order.copy(manufacturings = List(mA.copy(quantity = 3), mB))
    val draft = fromOrder(tripled, keys())
    draft.mfgs.map(_.quantity) shouldBe List(3, 1)
    val rebased = rebase(withPriority(draft, "urgent"), order.copy(manufacturings = List(mA.copy(quantity = 2), mB)), keys())
    rebased.mfgs.map(_.quantity) shouldBe List(2, 1)
    changes(rebased, names).map(_.label) shouldBe List("Testata: priorità")
    effects(fromOrder(tripled, keys()), contexts) shouldBe Nil

  test("stock warnings count the edited lines of the open manufacturings and the new ones, never a completed one"):
    val completedB = mB.copy(status = "completed", materials = List(line.copy(quantity = "5.000")))
    val draft = updateMaterials(fromOrder(order.copy(manufacturings = List(mA, completedB)), keys()), mfgA)(_.map(_.copy(quantity = "3")))
    demandRows(draft).map(_.quantity) shouldBe List("3")
    val newRows = List(MaterialDrafts.Row(99, material.toString, "1,5", ""))
    val available = MaterialRequirementResponse(material, "TES-001", "Tessuto", "linear_meters", "4.000", "0.000", "4.000")
    stockWarnings(draft, newRows, List(available)).get(material).map(w => (w.required, w.missing)) shouldBe Some(("4.500", "0.500"))
    stockWarnings(draft, Nil, List(available)).isEmpty shouldBe true

  test("addedManufacturings finds the manufacturing a response added"):
    addedManufacturings(order.copy(manufacturings = List(mA)), order).map(_.id) shouldBe List(mfgB)
end OrderEditDraftSpec
