package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import OrderDrafts.*

/** Unit tests for the pure helpers of the "Nuovo ordine" page: issue ids, inline messages, blank drafts and card labels. */
final class OrderCreateModelSpec extends AnyFunSuite with Matchers:

  private val templateId = UUID.fromString("00000000-0000-0000-0000-0000000000f1").nn

  private val template = ManufacturingCatalogResponse(templateId, "CAT-01", "Tagliando", None, Nil, Nil, Nil, 0)

  private val catalog: String => Option[ManufacturingCatalogResponse] = raw => Option.when(raw == templateId.toString)(template)

  private val emptyCore = MfgCoreState("custom", "", "", "", "", "", Set.empty, Map.empty)

  private def snapshot(core: MfgCoreState): MfgSnapshot = MfgSnapshot(1, core, Nil)

  private def snapshot(core: MfgCoreState, tasks: List[TaskState]): MfgSnapshot = MfgSnapshot(1, core, tasks)

  test("domId maps every issue target to its stable DOM id"):
    val noTask: (Int, Int) => Option[Int] = (_, _) => None
    OrderCreateModel.domId(IssueTarget.Header(HeaderField.Customer), noTask) shouldBe "oc-customer"
    OrderCreateModel.domId(IssueTarget.Header(HeaderField.WorkDeadline), noTask) shouldBe "oc-deadline"
    OrderCreateModel.domId(IssueTarget.NoManufacturings, noTask) shouldBe "oc-add-mfg"
    OrderCreateModel.domId(IssueTarget.Manufacturing(3, MfgField.Code), noTask) shouldBe "oc-mfg-3-code"
    OrderCreateModel.domId(IssueTarget.Manufacturing(3, MfgField.Materials), noTask) shouldBe "oc-mfg-3-materials"

  test("domId resolves a task row index to the row's draft key, falling back to the task list"):
    val taskKeyAt: (Int, Int) => Option[Int] = (mfg, index) => Option.when(mfg == 3 && index == 1)(42)
    OrderCreateModel.domId(IssueTarget.Task(3, 1, TaskField.Duration), taskKeyAt) shouldBe "oc-mfg-3-task-42-duration"
    OrderCreateModel.domId(IssueTarget.Task(3, 7, TaskField.Task), taskKeyAt) shouldBe "oc-mfg-3-tasks"

  test("mfgKeyOf and isHeader classify the targets"):
    OrderCreateModel.mfgKeyOf(IssueTarget.Task(5, 0, TaskField.Task)) shouldBe Some(5)
    OrderCreateModel.mfgKeyOf(IssueTarget.Manufacturing(4, MfgField.Catalog)) shouldBe Some(4)
    OrderCreateModel.mfgKeyOf(IssueTarget.NoManufacturings) shouldBe None
    OrderCreateModel.isHeader(IssueTarget.Header(HeaderField.Number)) shouldBe true
    OrderCreateModel.isHeader(IssueTarget.NoManufacturings) shouldBe false

  test("inlineMessage drops the manufacturing/task context and capitalizes"):
    OrderCreateModel.inlineMessage("Lavorazione 1: codice obbligatorio") shouldBe "Codice obbligatorio"
    OrderCreateModel.inlineMessage("Lavorazione 2, task 1: durata non valida (usa h:mm, es. 1:30, o ore intere)") shouldBe
      "Durata non valida (usa h:mm, es. 1:30, o ore intere)"
    OrderCreateModel.inlineMessage("Cliente obbligatorio") shouldBe "Cliente obbligatorio"

  test("every validation issue has an inline message without context"):
    val header = HeaderState.initial("", "2026-01-10")
    val custom = MfgSnapshot(1, emptyCore, List(TaskState("", "1:75", Set.empty, "")))
    OrderDrafts.validate(header, List(custom), catalog, MaterialDrafts.Context.empty).foreach { issue =>
      OrderCreateModel.inlineMessage(issue.message) should not include "Lavorazione"
    }

  test("isBlankDraft ignores the chosen type but not entered data"):
    OrderCreateModel.isBlankDraft(snapshot(emptyCore)) shouldBe true
    OrderCreateModel.isBlankDraft(snapshot(emptyCore.copy(mode = "catalog"))) shouldBe true
    OrderCreateModel.isBlankDraft(snapshot(emptyCore, List(TaskState("", "", Set.empty, "")))) shouldBe true
    OrderCreateModel.isBlankDraft(snapshot(emptyCore.copy(code = "M1"))) shouldBe false
    OrderCreateModel.isBlankDraft(snapshot(emptyCore, List(TaskState("", "2:00", Set.empty, "")))) shouldBe false
    OrderCreateModel.isBlankDraft(MfgSnapshot(1, emptyCore, Nil, List(MaterialDrafts.Row(1, "", "3", "")))) shouldBe false

  test("card labels: template code/name, typed code, or empty"):
    val catalogCore = emptyCore.copy(mode = "catalog", catalogId = templateId.toString)
    OrderCreateModel.mfgLabel(catalogCore, catalog) shouldBe "CAT-01 · Tagliando"
    OrderCreateModel.mfgCode(catalogCore, catalog) shouldBe "CAT-01"
    OrderCreateModel.mfgLabel(emptyCore.copy(code = "  M-7 "), catalog) shouldBe "M-7"
    OrderCreateModel.mfgLabel(emptyCore.copy(mode = "catalog"), catalog) shouldBe ""

  test("totalsText formats tasks, hours and cost"):
    OrderCreateModel.totalsText(MfgTotals(3, 95, "12.5")) shouldBe "3 task · 1:35 h · 12,50 €"
end OrderCreateModelSpec
