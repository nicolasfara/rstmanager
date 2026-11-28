package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.Dtos.OrderFeasibilityResponse
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** State of the delay check of the "Nuovo ordine" page. */
enum FeasibilityState derives CanEqual:
  /** Nothing to check yet: the form still has issues. */
  case Idle

  case Checking

  case Ready(response: OrderFeasibilityResponse)

  /** The check could not run (network or server error): it never blocks the creation. */
  case Failed

/**
 * The delay check of a new order against the production plan: the pure texts (kept DOM-free so they can be unit-tested) and the box showing them, in
 * the summary, the sidebar and the confirmation dialog of the "Nuovo ordine" page.
 */
object OrderFeasibility:

  /** A server date as `dd/MM/yyyy`, read from its calendar-day prefix (no time-zone shift). */
  private def day(iso: String): String =
    val text = DayText.long(Formats.calendarDay(iso))
    if text.isEmpty then iso else text

  /** The suggested work deadline as `yyyy-MM-dd` date-input text, ready for [[OrderDrafts.HeaderState.withSuggestedDeadline]]. */
  def suggestedDay(response: OrderFeasibilityResponse): Option[String] = response.suggestedDeadline.map(Formats.calendarDay)

  def headline(response: OrderFeasibilityResponse): String =
    if response.plannedCompletion.isEmpty then "Ordine non pianificabile con il personale attuale"
    else if response.hasDelay then "L'ordine genererebbe ritardi"
    else "Nessun ritardo previsto"

  /** One line per problem found: the order itself, its manufacturings, the existing orders it delays, why it cannot be planned. */
  def details(response: OrderFeasibilityResponse): List[String] =
    val completion = response.plannedCompletion.toList.map { done =>
      if response.late then s"Fine lavori prevista il ${day(done)}, oltre la deadline del ${day(response.deadline)}."
      else s"Fine lavori prevista il ${day(done)} (deadline ${day(response.deadline)})."
    }
    val manufacturings = response.lateManufacturings.map { late =>
      s"Lavorazione ${late.code}: finirebbe il ${day(late.plannedCompletion)}, oltre il completamento richiesto del ${day(late.completionDate)}."
    }
    val others = response.inducedDelays.map { delay =>
      delay.plannedCompletion.fold(s"Ordine ${delay.number}: non sarebbe più pianificabile.") { done =>
        s"Ordine ${delay.number}: finirebbe il ${day(done)}, oltre la sua deadline del ${day(delay.deadline)}."
      }
    }
    completion ++ manufacturings ++ others ++ response.reasons.map(_.message)
  end details

  /** What is left to the user when no deadline removes the delays. */
  def hint(response: OrderFeasibilityResponse): Option[String] =
    if !response.hasDelay || response.suggestedDeadline.isDefined then None
    else if response.plannedCompletion.isEmpty then Some("Verifica personale e calendario, o riduci le lavorazioni.")
    else if response.inducedDelays.nonEmpty && !response.late then
      Some("Nessuna deadline evita i ritardi sugli altri ordini (es. ordine urgente): valuta la priorità.")
    else Some("Rivedi le date di completamento delle lavorazioni indicate.")

  def suggestionText(suggested: String): String = s"Prossima data utile senza ritardi: ${DayText.long(suggested)}"

  private val footnote = "Avviso non bloccante: l'ordine si può creare comunque."

  /**
   * The delay box of `state`: nothing while idle, a quiet note while checking or when the check failed, green without delays, amber with the details,
   * the next feasible date and its "Usa data proposta" button (calling `onApply` with the `yyyy-MM-dd` day) otherwise.
   */
  def box(state: FeasibilityState, onApply: String => Unit): Option[HtmlElement] = state match
    case FeasibilityState.Idle => None
    case FeasibilityState.Checking => Some(div(role := "status", cls := "text-xs text-slate-500", "Verifica della pianificazione…"))
    case FeasibilityState.Failed =>
      Some(div(role := "status", cls := "text-xs text-slate-500", s"Impossibile verificare i ritardi. $footnote"))
    case FeasibilityState.Ready(response) if !response.hasDelay =>
      Some(
        div(
          role := "status",
          cls := "rounded-md border border-emerald-200 bg-emerald-50 px-3 py-2 text-xs text-emerald-800",
          div(cls := "font-semibold", headline(response)),
          details(response).map(line => div(line)),
        ),
      )
    case FeasibilityState.Ready(response) =>
      Some(
        div(
          role := "status",
          cls := "space-y-1 rounded-md border border-amber-200 bg-amber-50 px-3 py-2 text-left text-xs text-amber-800",
          div(cls := "font-semibold", headline(response)),
          ul(cls := "list-disc space-y-0.5 pl-4", details(response).map(line => li(line))),
          suggestedDay(response).map { suggested =>
            div(
              cls := "flex flex-wrap items-center gap-x-2 gap-y-1 pt-1",
              span(cls := "font-semibold", suggestionText(suggested)),
              button(tpe := "button", cls := s"$btnLink text-xs", "Usa data proposta", onClick --> (_ => onApply(suggested))),
            )
          },
          hint(response).map(text => div(text)),
          div(cls := "text-amber-700", footnote),
        ),
      )
end OrderFeasibility
