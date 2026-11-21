package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ EmployeeContractDto, EmployeeRequest, EmployeeResponse, HoursOverrideDto }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messages, FieldIssue }
import io.gitbub.nicolasfara.rstmanager.ui.Formats

/**
 * Pure (DOM-free) state of the employee create/edit form: request building and field-addressed validation.
 *
 * `PUT /employees/{id}` replaces the whole record, overrides included, while this form edits only the registry data: the overrides of the employee
 * being edited ride along unchanged in `overrides` (they have their own editor), so saving the form never drops them.
 */
object EmployeeForm:

  val contractOptions: List[(String, String)] =
    List("full_time" -> "Tempo pieno", "fixed_term" -> "Tempo determinato", "part_time" -> "Part-time")

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
  ):
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

    /** The issues: name, surname, start date, budget, then the fields required only by the chosen contract kind. */
    def issues: List[FieldIssue[Field]] =
      List(
        Option.when(name.trim.nn.isEmpty)(FieldIssue(Field.Name, "Nome obbligatorio")),
        Option.when(surname.trim.nn.isEmpty)(FieldIssue(Field.Surname, "Cognome obbligatorio")),
        Option.when(startDate.trim.nn.isEmpty)(FieldIssue(Field.StartDate, "Data inizio obbligatoria")),
        Option.when(budget.toIntOption.isEmpty)(FieldIssue(Field.Budget, "Budget non valido")),
        Option.when(needsEndDate && endDate.trim.nn.isEmpty)(FieldIssue(Field.EndDate, "Data fine obbligatoria")),
        Option.when(needsWeeklyHours && weeklyHours.toIntOption.forall(_ <= 0))(FieldIssue(Field.WeeklyHours, "Ore settimanali non valide")),
      ).flatten

    def errors: List[String] = issues.messages

    /** The first issue in on-screen order (the budget sits after the contract fields), i.e. where an invalid submit moves the focus. */
    def firstIssueOnScreen: Option[FieldIssue[Field]] = issues.minByOption(issue => screenOrder.indexOf(issue.field))
  end State

  /** The fields that can carry a validation issue. */
  enum Field derives CanEqual:
    case Name, Surname, StartDate, EndDate, WeeklyHours, Budget

  private val screenOrder: List[Field] = List(Field.Name, Field.Surname, Field.StartDate, Field.EndDate, Field.WeeklyHours, Field.Budget)

  /** DOM id of the control of `field` (label `for`, inline error anchor, focus target). */
  def fieldId(field: Field): String = field match
    case Field.Name => "emp-name"
    case Field.Surname => "emp-surname"
    case Field.StartDate => "emp-start"
    case Field.EndDate => "emp-end"
    case Field.WeeklyHours => "emp-weekly-hours"
    case Field.Budget => "emp-budget"

  val empty: State = State(None, "", "", "full_time", "", "", "20", "40", Nil)

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
    )
end EmployeeForm
