package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ ClosureRequest, ClosureResponse }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messages, FieldIssue }
import io.gitbub.nicolasfara.rstmanager.ui.Formats
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.IsoDay

/** Pure (DOM-free) state of the company-closure create/edit page: request building and field-addressed validation. */
object ClosureForm:

  val kindOptions: List[(String, String)] = List("period" -> "Periodo", "annual" -> "Ricorrenza annuale")

  val monthOptions: List[(String, String)] =
    List(
      "Gennaio",
      "Febbraio",
      "Marzo",
      "Aprile",
      "Maggio",
      "Giugno",
      "Luglio",
      "Agosto",
      "Settembre",
      "Ottobre",
      "Novembre",
      "Dicembre",
    ).zipWithIndex
      .map((name, index) => (index + 1).toString -> name)

  /** A leap year: an annual recurrence may fall on 29/02 (it then applies only in leap years). */
  private val leapYear = 2024

  final case class State(
      editingId: Option[UUID],
      kind: String,
      startDate: String,
      endDate: String,
      day: String,
      month: String,
      reason: String,
  ) derives CanEqual:
    def isAnnual: Boolean = kind == "annual"

    /** The payload to send: only the fields of the chosen kind, dates as the ISO instants the API expects, the reason trimmed. */
    def request: ClosureRequest =
      if isAnnual then ClosureRequest("annual", reason.trim.nn, None, None, month.trim.nn.toIntOption, day.trim.nn.toIntOption)
      else ClosureRequest("period", reason.trim.nn, Some(Formats.toIso(startDate.trim.nn)), Some(Formats.toIso(endDate.trim.nn)), None, None)

    /** The issues: the reason, then the fields of the chosen kind (the other kind's fields are ignored). */
    def issues: List[FieldIssue[Field]] =
      val kindIssues =
        if isAnnual then
          val validMonth = month.trim.nn.toIntOption.filter(m => m >= 1 && m <= 12)
          val maxDay = validMonth.fold(31)(IsoDay.monthLength(leapYear, _))
          List(
            Option.when(validMonth.isEmpty)(FieldIssue(Field.Month, "Mese non valido")),
            Option.when(!day.trim.nn.toIntOption.exists(d => d >= 1 && d <= maxDay))(FieldIssue(Field.Day, "Giorno non valido")),
          )
        else
          val (start, end) = (IsoDay.parse(startDate.trim.nn), IsoDay.parse(endDate.trim.nn))
          List(
            dayIssue(startDate, Field.StartDate, "Data inizio"),
            dayIssue(endDate, Field.EndDate, "Data fine"),
            start.zip(end).collect { case (s, e) if IsoDay.compare(e, s) < 0 => FieldIssue(Field.EndDate, "La data fine precede la data inizio") },
          )
      (Option.when(reason.trim.nn.isEmpty)(FieldIssue(Field.Reason, "Motivo obbligatorio")) :: kindIssues).flatten

    def errors: List[String] = issues.messages

    /** The first issue in on-screen order (the reason sits after the days), i.e. where an invalid submit moves the focus. */
    def firstIssueOnScreen: Option[FieldIssue[Field]] = issues.minByOption(issue => screenOrder.indexOf(issue.field))
  end State

  /** The fields that can carry a validation issue. */
  enum Field derives CanEqual:
    case StartDate, EndDate, Day, Month, Reason

  private val screenOrder: List[Field] = List(Field.StartDate, Field.EndDate, Field.Day, Field.Month, Field.Reason)

  /** DOM id of the control of `field` (label `for`, inline error anchor, focus target). */
  def fieldId(field: Field): String = field match
    case Field.StartDate => "closure-start"
    case Field.EndDate => "closure-end"
    case Field.Day => "closure-day"
    case Field.Month => "closure-month"
    case Field.Reason => "closure-reason"

  /** The banner of a failed save request. */
  val submitErrorId = "closure-submit-error"

  private def dayIssue(text: String, field: Field, label: String): Option[FieldIssue[Field]] =
    if text.trim.nn.isEmpty then Some(FieldIssue(field, s"$label obbligatoria"))
    else Option.when(!IsoDay.isValid(text.trim.nn))(FieldIssue(field, s"$label non valida"))

  val empty: State = State(None, "period", "", "", "", "1", "")

  def fromResponse(c: ClosureResponse): State =
    State(
      Some(c.id),
      if c.kind == "annual" then "annual" else "period",
      c.startDate.map(Formats.calendarDay).getOrElse(""),
      c.endDate.map(Formats.calendarDay).getOrElse(""),
      c.day.map(_.toString).getOrElse(""),
      c.month.map(_.toString).getOrElse("1"),
      c.reason,
    )
end ClosureForm
