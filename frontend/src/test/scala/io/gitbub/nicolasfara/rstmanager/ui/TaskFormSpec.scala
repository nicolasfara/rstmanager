package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.FieldIssue
import io.gitbub.nicolasfara.rstmanager.ui.TaskForm.Field
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure catalog-task form: validation, request building and loading a stored task. */
final class TaskFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[Option[TaskRequest], Option[TaskRequest]] = CanEqual.derived
  private given CanEqual[List[FieldIssue[Field]], List[FieldIssue[Field]]] = CanEqual.derived

  private val taskId = UUID.fromString("00000000-0000-0000-0000-000000000701").nn
  private val employeeId = UUID.fromString("00000000-0000-0000-0000-000000000702").nn
  private val valid = TaskForm.empty.copy(name = "Taglio")

  test("a new task proposes 8:00 and needs a name"):
    TaskForm.empty.hours shouldBe "8:00"
    TaskForm.empty.issues shouldBe List(FieldIssue(Field.Name, "Nome obbligatorio"))
    TaskForm.empty.copy(name = "   ").errors shouldBe List("Nome obbligatorio")
    valid.errors shouldBe Nil

  test("the duration is required and must be h:mm or whole hours"):
    valid.copy(hours = " ").issues shouldBe List(FieldIssue(Field.Duration, "Durata obbligatoria"))
    valid.copy(hours = "1.30").issues.map(_.field) shouldBe List(Field.Duration)
    valid.copy(hours = "1:75").issues.map(_.field) shouldBe List(Field.Duration)
    valid.copy(hours = "2").errors shouldBe Nil
    valid.copy(hours = "0:00").errors shouldBe Nil

  test("the request trims the text, drops an empty description and parses the employee"):
    valid.copy(name = " Taglio ", description = "  ", hours = "1:35", defaultEmployee = employeeId.toString).request shouldBe
      Some(TaskRequest("Taglio", None, 95, Some(employeeId)))
    valid.copy(description = " Taglio tessuto ", defaultEmployee = "").request shouldBe
      Some(TaskRequest("Taglio", Some("Taglio tessuto"), 480, None))
    valid.copy(hours = "abc").request shouldBe None

  test("a stored task loads as an edit of the same content"):
    val loaded = TaskForm.fromResponse(TaskResponse(taskId, "Cucitura", Some("Doppia"), 150, Some(employeeId)))
    loaded shouldBe TaskForm.State(Some(taskId), "Cucitura", "Doppia", "2:30", employeeId.toString)
    loaded.displayName shouldBe "Cucitura"
    loaded.request shouldBe Some(TaskRequest("Cucitura", Some("Doppia"), 150, Some(employeeId)))
    TaskForm.fromResponse(TaskResponse(taskId, "Cucitura", None, 60, None)).copy(editingId = None) shouldBe
      TaskForm.empty.copy(name = "Cucitura", hours = "1:00")
end TaskFormSpec
