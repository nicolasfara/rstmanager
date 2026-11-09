package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ EmployeeContractDto, EmployeeRequest, EmployeeResponse, HoursOverrideDto }
import io.gitbub.nicolasfara.rstmanager.ui.Formats

/**
 * Pure (DOM-free) state of the employee create/edit form: request building and validation messages.
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

    def errors: List[String] =
      List(
        Option.when(name.trim.nn.isEmpty)("Nome obbligatorio"),
        Option.when(surname.trim.nn.isEmpty)("Cognome obbligatorio"),
        Option.when(startDate.trim.nn.isEmpty)("Data inizio obbligatoria"),
        Option.when(budget.toIntOption.isEmpty)("Budget non valido"),
        Option.when(contractKind == "fixed_term" && endDate.trim.nn.isEmpty)("Data fine obbligatoria"),
        Option.when(contractKind == "part_time" && weeklyHours.toIntOption.forall(_ <= 0))("Ore settimanali non valide"),
      ).flatten
  end State

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
