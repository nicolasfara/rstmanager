package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ ScheduledTaskDto, TaskProgressUpdateRequest }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.FieldIssue

/**
 * Pure (DOM-free) state of the planning task page: updating the progress of a planned task, or reactivating a completed one.
 *
 * The form keeps the hours as typed (`h:mm`, or whole hours) and compares them with the `baseline` loaded from the task when the page opens: only the
 * values that changed are sent. An empty "Ore totali" keeps the stored estimate (it is not sent), as before.
 */
object TaskProgressForm:

  /** Updating a planned task (hours and preferred employee) or bringing a completed one back into planning (hours only). */
  enum Mode derives CanEqual:
    case Progress, Reactivate

  /** The fields that can carry a validation issue. */
  enum Field derives CanEqual:
    case Completed, Expected

  /** DOM id of the control of `field` (label `for`, inline error anchor, focus target). */
  def fieldId(field: Field): String = field match
    case Field.Completed => "pt-completed"
    case Field.Expected => "pt-expected"

  /** The editable values: hours done, total hours (both as typed) and the preferred employee id (empty = automatic). */
  final case class State(completed: String, expected: String, employeeId: String) derives CanEqual

  /** The quick increments offered under "Ore svolte", as (minutes, label). */
  val quickIncrements: List[(Int, String)] = List(30 -> "+0:30", 60 -> "+1:00", 120 -> "+2:00")

  private val formatHint = "Usa il formato h:mm (es. 1:35)."

  /** The stored values of `task`, with the manufacturing's preferred employee: the baseline of the page. */
  def baseline(task: ScheduledTaskDto, preferred: Option[UUID]): State =
    State(Formats.duration(task.completedMinutes.getOrElse(0)), Formats.duration(task.expectedMinutes), preferred.fold("")(_.toString))

  /**
   * The form as the page opens. Reactivating leaves "Ore svolte" empty: the task is completed, so its recorded hours cover the estimate, and the user
   * must say how many were actually done.
   */
  def initial(mode: Mode, baseline: State): State = mode match
    case Mode.Progress => baseline
    case Mode.Reactivate => baseline.copy(completed = "")

  /** Parses a typed duration; blank is `None` (no value), like a malformed one: check [[issues]] first. */
  private def parse(raw: String): Option[Int] = Formats.parseDuration(raw)

  private def malformed(raw: String): Boolean = raw.trim.nn.nonEmpty && parse(raw).isEmpty

  /** The total hours that would be in effect after saving: the typed ones, or the stored ones when the field is blank. */
  def effectiveExpected(state: State, baseline: State): Option[Int] =
    parse(state.expected).orElse(Option.when(state.expected.trim.nn.isEmpty)(parse(baseline.expected)).flatten)

  /** The issues in form order: hours done, then total hours. Same rules and messages as the former task dialog. */
  def issues(mode: Mode, state: State, baseline: State): List[FieldIssue[Field]] =
    val completedFormat = Option.when(malformed(state.completed))(FieldIssue(Field.Completed, s"Le ore svolte non sono valide. $formatHint"))
    val expectedIssue =
      if malformed(state.expected) then Some(FieldIssue(Field.Expected, s"Le ore totali non sono valide. $formatHint"))
      else Option.when(parse(state.expected).exists(_ < 1))(FieldIssue(Field.Expected, "Le ore totali devono essere almeno 1 minuto."))
    val reactivation =
      Option.when(
        mode == Mode.Reactivate && completedFormat.isEmpty && expectedIssue.isEmpty &&
          !parse(state.completed).zip(effectiveExpected(state, baseline)).exists((done, total) => done < total),
      )(FieldIssue(Field.Completed, "Per riportare il task in lavorazione le ore svolte devono essere inferiori alle ore totali."))
    List(completedFormat, reactivation, expectedIssue).flatten
  end issues

  /**
   * `raw` plus `minutes` (a quick chip), formatted as `h:mm`; blank counts as zero. `None` when `raw` is malformed, so a chip never overwrites what
   * the user is still correcting.
   */
  def addMinutes(raw: String, minutes: Int): Option[String] =
    if raw.trim.nn.isEmpty then Some(Formats.duration(minutes))
    else parse(raw).map(current => Formats.duration(current + minutes))

  /** "Segna completato": the hours done aligned to the total in effect (the typed total, or the stored one when blank). */
  def markCompleted(state: State, baseline: State): State =
    effectiveExpected(state, baseline).fold(state)(total => state.copy(completed = Formats.duration(total)))

  /** What a valid form sends: the task progress request (when an hour value changed) and the new preferred employee (when it changed). */
  final case class Submission(task: Option[TaskProgressUpdateRequest], employee: Option[Option[UUID]]):
    def isEmpty: Boolean = task.isEmpty && employee.isEmpty

  /** The requests for a form without [[issues]]: only changed values are sent; the employee only in [[Mode.Progress]]. */
  def submission(mode: Mode, state: State, baseline: State): Submission =
    def changed(select: State => String): Option[Int] =
      val next = parse(select(state))
      next.filter(_ => next != parse(select(baseline)))
    val completed = changed(_.completed)
    val expected = changed(_.expected)
    val task = Option.when(completed.isDefined || expected.isDefined)(TaskProgressUpdateRequest(completed, expected))
    val employee = Option.when(mode == Mode.Progress && state.employeeId != baseline.employeeId)(
      Try(UUID.fromString(state.employeeId).nn).toOption,
    )
    Submission(task, employee)
  end submission

  /** How a successful save is announced. */
  enum Outcome derives CanEqual:
    case Updated, Completed, Reactivated

  /** [[Outcome.Reactivated]] when reactivating; otherwise [[Outcome.Completed]] when the hours done now cover the total, else [[Outcome.Updated]]. */
  def outcome(mode: Mode, state: State, baseline: State): Outcome = mode match
    case Mode.Reactivate => Outcome.Reactivated
    case Mode.Progress =>
      val done = parse(state.completed).orElse(parse(baseline.completed))
      if done.zip(effectiveExpected(state, baseline)).exists((d, total) => d >= total) then Outcome.Completed else Outcome.Updated

  /** The success toast, e.g. «Task «Cucitura» aggiornato». */
  def successMessage(outcome: Outcome, taskName: String): String = outcome match
    case Outcome.Updated => s"Task «$taskName» aggiornato"
    case Outcome.Completed => s"Task «$taskName» completato"
    case Outcome.Reactivated => s"Task «$taskName» riattivato"
end TaskProgressForm
