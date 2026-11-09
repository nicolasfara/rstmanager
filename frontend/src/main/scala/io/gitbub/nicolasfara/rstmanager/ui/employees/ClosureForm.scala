package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ ClosureRequest, ClosureResponse }
import io.gitbub.nicolasfara.rstmanager.ui.Formats
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.IsoDay

/** Pure (DOM-free) state of the company-closure create/edit form: request building and validation messages. */
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
  ):
    def isAnnual: Boolean = kind == "annual"

    /** Nothing typed yet: the form holds back its validation messages. */
    def isBlank: Boolean = editingId.isEmpty && reason.trim.nn.isEmpty && startDate.isEmpty && endDate.isEmpty && day.trim.nn.isEmpty

    /** The payload to send: only the fields of the chosen kind, dates as the ISO instants the API expects, the reason trimmed. */
    def request: ClosureRequest =
      if isAnnual then ClosureRequest("annual", reason.trim.nn, None, None, month.trim.nn.toIntOption, day.trim.nn.toIntOption)
      else ClosureRequest("period", reason.trim.nn, Some(Formats.toIso(startDate.trim.nn)), Some(Formats.toIso(endDate.trim.nn)), None, None)

    def errors: List[String] =
      val kindErrors =
        if isAnnual then
          val validMonth = month.trim.nn.toIntOption.filter(m => m >= 1 && m <= 12)
          val maxDay = validMonth.fold(31)(IsoDay.monthLength(leapYear, _))
          List(
            Option.when(validMonth.isEmpty)("Mese non valido"),
            Option.when(!day.trim.nn.toIntOption.exists(d => d >= 1 && d <= maxDay))("Giorno non valido"),
          )
        else
          val (start, end) = (IsoDay.parse(startDate), IsoDay.parse(endDate))
          List(
            dayError(startDate, "Data inizio"),
            dayError(endDate, "Data fine"),
            start.zip(end).collect { case (s, e) if IsoDay.compare(e, s) < 0 => "La data fine precede la data inizio" },
          )
      (Option.when(reason.trim.nn.isEmpty)("Motivo obbligatorio") :: kindErrors).flatten
  end State

  private def dayError(text: String, label: String): Option[String] =
    if text.trim.nn.isEmpty then Some(s"$label obbligatoria")
    else Option.when(!IsoDay.isValid(text))(s"$label non valida")

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
