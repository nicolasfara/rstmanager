package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ ScheduledTaskDto, TaskProgressUpdateRequest }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.FieldIssue
import io.gitbub.nicolasfara.rstmanager.ui.TaskProgressForm.{ Field, Mode, Outcome, State, Submission }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure planning task form: baseline, inline validation (B3), quick chips, "Segna completato" and the requests sent. */
final class TaskProgressFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[List[FieldIssue[Field]], List[FieldIssue[Field]]] = CanEqual.derived
  private given CanEqual[Submission, Submission] = CanEqual.derived
  private given CanEqual[Option[String], Option[String]] = CanEqual.derived

  private val employee = UUID.fromString("00000000-0000-0000-0000-000000000701").nn
  private val other = UUID.fromString("00000000-0000-0000-0000-000000000702").nn

  private def task(done: Option[Int], total: Int, status: String): ScheduledTaskDto =
    ScheduledTaskDto(UUID.fromString("00000000-0000-0000-0000-000000000801").nn, other, status, total, done, None)

  // 1:00 done of 3:00, preferred employee set.
  private val planned = TaskProgressForm.baseline(task(Some(60), 180, "in_progress"), Some(employee))
  // Completed: 3:00 of 3:00.
  private val completed = TaskProgressForm.baseline(task(Some(180), 180, "completed"), None)

  private def fields(mode: Mode, state: State, baseline: State): List[Field] = TaskProgressForm.issues(mode, state, baseline).map(_.field)

  test("the baseline formats the stored hours and the preferred employee"):
    planned shouldBe State("1:00", "3:00", employee.toString)
    TaskProgressForm.baseline(task(None, 90, "pending"), None) shouldBe State("0:00", "1:30", "")

  test("progress starts from the stored values, reactivation with empty hours done"):
    TaskProgressForm.initial(Mode.Progress, planned) shouldBe planned
    TaskProgressForm.initial(Mode.Reactivate, completed) shouldBe completed.copy(completed = "")

  test("a valid form has no issues"):
    TaskProgressForm.issues(Mode.Progress, planned, planned) shouldBe Nil
    TaskProgressForm.issues(Mode.Progress, planned.copy(completed = "2", expected = "4:30"), planned) shouldBe Nil

  test("malformed hours are reported on their own field, with the h:mm hint"):
    TaskProgressForm.issues(Mode.Progress, planned.copy(completed = "1.30"), planned) shouldBe List(
      FieldIssue(Field.Completed, "Le ore svolte non sono valide. Usa il formato h:mm (es. 1:35)."),
    )
    TaskProgressForm.issues(Mode.Progress, planned.copy(expected = "abc"), planned) shouldBe List(
      FieldIssue(Field.Expected, "Le ore totali non sono valide. Usa il formato h:mm (es. 1:35)."),
    )
    fields(Mode.Progress, planned.copy(completed = "x", expected = "1:75"), planned) shouldBe List(Field.Completed, Field.Expected)

  test("the total must be at least one minute"):
    TaskProgressForm.issues(Mode.Progress, planned.copy(expected = "0:00"), planned) shouldBe List(
      FieldIssue(Field.Expected, "Le ore totali devono essere almeno 1 minuto."),
    )

  test("blank hours are not an error while updating: they keep the stored value"):
    TaskProgressForm.issues(Mode.Progress, planned.copy(completed = " ", expected = ""), planned) shouldBe Nil

  test("reactivating needs hours done below the total"):
    val message = "Per riportare il task in lavorazione le ore svolte devono essere inferiori alle ore totali."
    val start = TaskProgressForm.initial(Mode.Reactivate, completed)
    TaskProgressForm.issues(Mode.Reactivate, start, completed) shouldBe List(FieldIssue(Field.Completed, message))
    fields(Mode.Reactivate, start.copy(completed = "3:00"), completed) shouldBe List(Field.Completed)
    fields(Mode.Reactivate, start.copy(completed = "2:30"), completed) shouldBe Nil
    // A blank total means the stored one (3:00); a lowered total is compared as typed.
    fields(Mode.Reactivate, start.copy(completed = "2:30", expected = ""), completed) shouldBe Nil
    fields(Mode.Reactivate, start.copy(completed = "2:30", expected = "2:00"), completed) shouldBe List(Field.Completed)

  test("a format error wins over the reactivation rule on the same form"):
    fields(Mode.Reactivate, completed.copy(completed = "abc"), completed) shouldBe List(Field.Completed)
    fields(Mode.Reactivate, completed.copy(completed = "1:00", expected = "abc"), completed) shouldBe List(Field.Expected)

  test("the quick chips add to the typed hours and never overwrite a malformed value"):
    TaskProgressForm.quickIncrements.map(_._2) shouldBe List("+0:30", "+1:00", "+2:00")
    TaskProgressForm.addMinutes("1:00", 60) shouldBe Some("2:00")
    TaskProgressForm.addMinutes("1:45", 30) shouldBe Some("2:15")
    TaskProgressForm.addMinutes("2", 120) shouldBe Some("4:00")
    TaskProgressForm.addMinutes("", 30) shouldBe Some("0:30")
    TaskProgressForm.addMinutes("1.30", 30) shouldBe None

  test("'Segna completato' aligns the hours done to the total in effect"):
    TaskProgressForm.markCompleted(planned, planned).completed shouldBe "3:00"
    TaskProgressForm.markCompleted(planned.copy(expected = "4"), planned).completed shouldBe "4:00"
    TaskProgressForm.markCompleted(planned.copy(expected = ""), planned).completed shouldBe "3:00"
    TaskProgressForm.markCompleted(planned.copy(expected = "abc"), planned) shouldBe planned.copy(expected = "abc")

  test("a pristine or equivalent form sends nothing"):
    TaskProgressForm.submission(Mode.Progress, planned, planned).isEmpty shouldBe true
    TaskProgressForm.submission(Mode.Progress, planned.copy(completed = "1", expected = "3"), planned).isEmpty shouldBe true

  test("only the changed hours are sent"):
    TaskProgressForm.submission(Mode.Progress, planned.copy(completed = "2:00"), planned) shouldBe
      Submission(Some(TaskProgressUpdateRequest(Some(120), None)), None)
    TaskProgressForm.submission(Mode.Progress, planned.copy(expected = "4:00"), planned) shouldBe
      Submission(Some(TaskProgressUpdateRequest(None, Some(240))), None)
    TaskProgressForm.submission(Mode.Progress, planned.copy(expected = ""), planned).isEmpty shouldBe true

  test("the preferred employee is sent when changed, an empty choice clears it"):
    TaskProgressForm.submission(Mode.Progress, planned.copy(employeeId = other.toString), planned) shouldBe Submission(None, Some(Some(other)))
    TaskProgressForm.submission(Mode.Progress, planned.copy(employeeId = ""), planned) shouldBe Submission(None, Some(None))

  test("reactivation sends the hours done and never the employee"):
    val start = TaskProgressForm.initial(Mode.Reactivate, completed)
    TaskProgressForm.submission(Mode.Reactivate, start.copy(completed = "2:00", employeeId = other.toString), completed) shouldBe
      Submission(Some(TaskProgressUpdateRequest(Some(120), None)), None)

  test("the outcome names what the save did"):
    given CanEqual[Outcome, Outcome] = CanEqual.derived
    TaskProgressForm.outcome(Mode.Progress, planned.copy(completed = "2:00"), planned) shouldBe Outcome.Updated
    TaskProgressForm.outcome(Mode.Progress, planned.copy(completed = "3:00"), planned) shouldBe Outcome.Completed
    TaskProgressForm.outcome(Mode.Progress, planned.copy(expected = "1:00"), planned) shouldBe Outcome.Completed
    TaskProgressForm.outcome(Mode.Progress, planned.copy(employeeId = ""), planned) shouldBe Outcome.Updated
    TaskProgressForm.outcome(Mode.Reactivate, completed.copy(completed = "1:00"), completed) shouldBe Outcome.Reactivated

  test("the success messages"):
    TaskProgressForm.successMessage(Outcome.Updated, "Cucitura") shouldBe "Task «Cucitura» aggiornato"
    TaskProgressForm.successMessage(Outcome.Completed, "Cucitura") shouldBe "Task «Cucitura» completato"
    TaskProgressForm.successMessage(Outcome.Reactivated, "Cucitura") shouldBe "Task «Cucitura» riattivato"
end TaskProgressFormSpec
