package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ EmployeeContractDto, EmployeeRequest, EmployeeResponse, HoursOverrideDto }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messages, FieldIssue }
import io.gitbub.nicolasfara.rstmanager.ui.Formats
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.IsoDay
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.IsoDay.Ymd

/**
 * Pure (DOM-free) state of the employee create/edit page: request building and field-addressed validation.
 *
 * `PUT /employees/{id}` replaces the whole record, overrides included. The page edits both the registry data and the hours overrides ("Override"
 * section), and one "Salva modifiche" sends them together. `overrideDraft` is the override being typed in that section, not yet added to the list
 * with "+ Aggiungi": it is never sent, and a submit with a half-typed one is refused (so it is not lost silently).
 */
object EmployeeForm:

  val contractOptions: List[(String, String)] =
    List("full_time" -> "Tempo pieno", "fixed_term" -> "Tempo determinato", "part_time" -> "Part-time")

  val overrideKinds: List[(String, String)] = List("working_day" -> "Giorno lavorativo", "vacation" -> "Ferie")

  /** Highest daily hours a working-day override accepts (the server's `DailyHours` is 0 to 24). */
  private val maxDailyHours = 24

  /**
   * The override being typed in the "Override" section: a working day (`hours` on `day`, optional `reason`) or a vacation (`start` to `end`, both
   * inclusive). The dates are the `YYYY-MM-DD` the date inputs take.
   */
  final case class OverrideDraft(
      kind: String,
      hours: String,
      reason: String,
      day: String,
      start: String,
      end: String,
  ) derives CanEqual:
    def isVacation: Boolean = kind == "vacation"

    /** Nothing typed yet (the default hours and the kind switch do not count): there is nothing to add or to lose. */
    def isBlank: Boolean =
      (hours.trim.nn.isEmpty || hours.trim.nn == OverrideDraft.empty.hours) &&
        reason.trim.nn.isEmpty && day.trim.nn.isEmpty && start.trim.nn.isEmpty && end.trim.nn.isEmpty

    /** The override to store: only the fields of the chosen kind, days as the ISO instants the API expects, the reason trimmed (blank = none). */
    def toOverride: HoursOverrideDto =
      if isVacation then HoursOverrideDto("vacation", None, None, None, Some(Formats.toIso(start.trim.nn)), Some(Formats.toIso(end.trim.nn)))
      else
        HoursOverrideDto(
          "working_day",
          hours.trim.nn.toIntOption,
          Some(reason.trim.nn).filter(_.nonEmpty),
          Some(Formats.toIso(day.trim.nn)),
          None,
          None,
        )

    /**
     * Why this override cannot be added next to `existing`, field by field, in on-screen order: the fields of its kind, then the clashes the server
     * refuses too (two overrides on one day, a working day inside a vacation, overlapping vacations).
     */
    def issues(existing: List[HoursOverrideDto]): List[FieldIssue[OverrideField]] =
      if isVacation then
        val (from, to) = (IsoDay.parse(start.trim.nn), IsoDay.parse(end.trim.nn))
        val ownIssues = List(
          dayIssue(start, OverrideField.Start, "Data inizio obbligatoria", "Data inizio non valida"),
          dayIssue(end, OverrideField.End, "Data fine obbligatoria", "Data fine non valida"),
          from.zip(to).collect { case (s, e) if IsoDay.compare(e, s) < 0 => FieldIssue(OverrideField.End, "La data fine precede la data inizio") },
        ).flatten
        if ownIssues.nonEmpty then ownIssues
        else
          from.zip(to).toList.flatMap { (s, e) =>
            val overlapsVacation = existing.flatMap(vacationBounds).exists((vs, ve) => IsoDay.compare(vs, e) <= 0 && IsoDay.compare(s, ve) <= 0)
            val workingInside = existing.flatMap(workingDay).find(d => IsoDay.compare(s, d) <= 0 && IsoDay.compare(d, e) <= 0)
            List(
              Option.when(overlapsVacation)(FieldIssue(OverrideField.Start, "Si sovrappone a ferie già inserite")),
              workingInside.map(d => FieldIssue(OverrideField.Start, s"Comprende il giorno lavorativo del ${IsoDay.display(IsoDay.format(d))}")),
            ).flatten
          }
      else
        val ownIssues = List(
          Option.when(!hours.trim.nn.toIntOption.exists(h => h >= 0 && h <= maxDailyHours))(
            FieldIssue(OverrideField.Hours, s"Ore non valide (da 0 a $maxDailyHours)"),
          ),
          dayIssue(day, OverrideField.Day, "Giorno obbligatorio", "Giorno non valido"),
        ).flatten
        if ownIssues.nonEmpty then ownIssues
        else
          IsoDay.parse(day.trim.nn).toList.flatMap { d =>
            if existing.flatMap(workingDay).contains(d) then List(FieldIssue(OverrideField.Day, "Esiste già un override per questo giorno"))
            else if existing.flatMap(vacationBounds).exists((s, e) => IsoDay.compare(s, d) <= 0 && IsoDay.compare(d, e) <= 0) then
              List(FieldIssue(OverrideField.Day, "Il giorno cade in un periodo di ferie già inserito"))
            else Nil
          }
  end OverrideDraft

  object OverrideDraft:
    val empty: OverrideDraft = OverrideDraft("working_day", "8", "", "", "", "")

  /** The fields of the override draft that can carry a validation issue. */
  enum OverrideField derives CanEqual:
    case Hours, Day, Start, End

  /** DOM id of the control of an override draft `field`. */
  def overrideFieldId(field: OverrideField): String = field match
    case OverrideField.Hours => "emp-ovr-hours"
    case OverrideField.Day => "emp-ovr-day"
    case OverrideField.Start => "emp-ovr-start"
    case OverrideField.End => "emp-ovr-end"

  /** `missing` when `text` is blank, `invalid` when it is not a real `YYYY-MM-DD` day. */
  private def dayIssue(text: String, field: OverrideField, missing: String, invalid: String): Option[FieldIssue[OverrideField]] =
    val trimmed = text.trim.nn
    if trimmed.isEmpty then Some(FieldIssue(field, missing))
    else Option.when(!IsoDay.isValid(trimmed))(FieldIssue(field, invalid))

  private def workingDay(o: HoursOverrideDto): Option[Ymd] =
    if o.kind == "working_day" then o.day.flatMap(d => IsoDay.parse(Formats.calendarDay(d))) else None

  private def vacationBounds(o: HoursOverrideDto): Option[(Ymd, Ymd)] =
    if o.kind == "vacation" then
      for
        s <- o.startDate.flatMap(d => IsoDay.parse(Formats.calendarDay(d)))
        e <- o.endDate.flatMap(d => IsoDay.parse(Formats.calendarDay(d)))
      yield (s, e)
    else None

  /** The first day an override applies, `YYYY-MM-DD` (empty when unreadable): the order the page lists the overrides in. */
  def overrideStart(o: HoursOverrideDto): String =
    (if o.kind == "vacation" then o.startDate else o.day).map(Formats.calendarDay).getOrElse("")

  /** The override as the page writes it: `4h il 02/03/2026 · Inventario` or `Ferie 10/08/2026 → 21/08/2026` (days read without time-zone shift). */
  def describeOverride(o: HoursOverrideDto): String =
    def shown(iso: Option[String]): String = iso.map(d => IsoDay.display(Formats.calendarDay(d))).getOrElse("?")
    o.kind match
      case "working_day" => s"${o.hours.getOrElse(0)}h il ${shown(o.day)}${o.reason.map(r => s" · $r").getOrElse("")}"
      case "vacation" =>
        val (start, end) = (shown(o.startDate), shown(o.endDate))
        if start == end then s"Ferie il $start" else s"Ferie $start → $end"
      case other => other

  final case class State(
      editingId: Option[UUID],
      name: String,
      surname: String,
      contractKind: String,
      startDate: String,
      endDate: String,
      weeklyHours: String,
      budget: String,
      overrides: List[HoursOverrideDto],
      overrideDraft: OverrideDraft,
  ) derives CanEqual:
    def contract: EmployeeContractDto = contractKind match
      case "fixed_term" => EmployeeContractDto("fixed_term", Formats.toIso(startDate), Some(Formats.toIso(endDate)), None)
      case "part_time" => EmployeeContractDto("part_time", Formats.toIso(startDate), None, weeklyHours.toIntOption)
      case _ => EmployeeContractDto("full_time", Formats.toIso(startDate), None, None)

    def request: EmployeeRequest =
      EmployeeRequest(name.trim.nn, surname.trim.nn, contract, budget.toIntOption.getOrElse(0), overrides)

    /** True when the contract kind needs an end date (fixed term). */
    def needsEndDate: Boolean = contractKind == "fixed_term"

    /** True when the contract kind needs the contractual weekly hours (part time). */
    def needsWeeklyHours: Boolean = contractKind == "part_time"

    /**
     * The issues: name, surname, start date, budget, then the fields required only by the chosen contract kind, and last an override typed but not
     * added yet.
     */
    def issues: List[FieldIssue[Field]] =
      List(
        Option.when(name.trim.nn.isEmpty)(FieldIssue(Field.Name, "Nome obbligatorio")),
        Option.when(surname.trim.nn.isEmpty)(FieldIssue(Field.Surname, "Cognome obbligatorio")),
        Option.when(startDate.trim.nn.isEmpty)(FieldIssue(Field.StartDate, "Data inizio obbligatoria")),
        Option.when(budget.toIntOption.isEmpty)(FieldIssue(Field.Budget, "Budget non valido")),
        Option.when(needsEndDate && endDate.trim.nn.isEmpty)(FieldIssue(Field.EndDate, "Data fine obbligatoria")),
        Option.when(needsWeeklyHours && weeklyHours.toIntOption.forall(_ <= 0))(FieldIssue(Field.WeeklyHours, "Ore settimanali non valide")),
        Option.when(!overrideDraft.isBlank)(
          FieldIssue(Field.PendingOverride, "Override non ancora aggiunto: premi «+ Aggiungi» oppure «Svuota»"),
        ),
      ).flatten

    def errors: List[String] = issues.messages

    /** The first issue in on-screen order (the budget sits after the contract fields), i.e. where an invalid submit moves the focus. */
    def firstIssueOnScreen: Option[FieldIssue[Field]] = issues.minByOption(issue => screenOrder.indexOf(issue.field))

    /** Why the override draft cannot be added (empty = it can), checked against the overrides already in the list. */
    def overrideIssues: List[FieldIssue[OverrideField]] = overrideDraft.issues(overrides)

    /** "+ Aggiungi": the override draft joins the list and the draft is cleared (its kind kept, for the next one); `None` while it has issues. */
    def addOverride: Option[State] =
      Option.when(overrideIssues.isEmpty)(
        copy(overrides = overrides :+ overrideDraft.toOverride, overrideDraft = OverrideDraft.empty.copy(kind = overrideDraft.kind)),
      )

    /**
     * What the dirty check compares: a blank override draft counts as the default one, so switching its kind (or clearing the default hours) does not
     * make the page dirty, while a half-typed override does.
     */
    def comparable: State = if overrideDraft.isBlank then copy(overrideDraft = OverrideDraft.empty) else this

    /** Removes the override at `index` of `overrides` (an index out of range changes nothing). */
    def removeOverride(index: Int): State = copy(overrides = overrides.patch(index, Nil, 1))

    /** The overrides in calendar order (by first day), each with its index in `overrides` (what [[removeOverride]] takes). */
    def overridesByDay: List[(HoursOverrideDto, Int)] = overrides.zipWithIndex.sortBy((o, _) => overrideStart(o))
  end State

  /** The fields that can carry a validation issue. */
  enum Field derives CanEqual:
    case Name, Surname, StartDate, EndDate, WeeklyHours, Budget, PendingOverride

  private val screenOrder: List[Field] =
    List(Field.Name, Field.Surname, Field.StartDate, Field.EndDate, Field.WeeklyHours, Field.Budget, Field.PendingOverride)

  /** DOM id of the control of `field` (label `for`, inline error anchor, focus target); a pending override points at "+ Aggiungi". */
  def fieldId(field: Field): String = field match
    case Field.Name => "emp-name"
    case Field.Surname => "emp-surname"
    case Field.StartDate => "emp-start"
    case Field.EndDate => "emp-end"
    case Field.WeeklyHours => "emp-weekly-hours"
    case Field.Budget => "emp-budget"
    case Field.PendingOverride => overrideAddId

  /** The "+ Aggiungi" button of the "Override" section. */
  val overrideAddId = "emp-ovr-add"

  /** The body of the "Override" section: where «Override» on the list moves the focus. */
  val overridesSectionId = "emp-overrides"

  /** The banner of a failed save request. */
  val submitErrorId = "emp-submit-error"

  val empty: State = State(None, "", "", "full_time", "", "", "20", "40", Nil, OverrideDraft.empty)

  /** Loads a stored employee into the form, contract days as the `YYYY-MM-DD` the date inputs take, overrides kept as they are. */
  def fromResponse(emp: EmployeeResponse): State =
    State(
      Some(emp.id),
      emp.name,
      emp.surname,
      emp.contract.kind,
      Formats.calendarDay(emp.contract.startDate),
      emp.contract.endDate.map(Formats.calendarDay).getOrElse(""),
      emp.contract.weeklyHours.fold(empty.weeklyHours)(_.toString),
      emp.budgetWeeklyHours.toString,
      emp.overrides,
      OverrideDraft.empty,
    )
end EmployeeForm
