package io.gitbub.nicolasfara.rstmanager.ui.employees

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, Pagination, Paging }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.BusinessDay

/**
 * Company calendar ("Calendario aziendale"): the closures in which nobody works, either a period (first and last day inclusive) or a day recurring
 * every year. A closure wins over any hours override, and every change triggers a planning recalculation.
 */
object CompanyCalendarTab:

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val form = Var(ClosureForm.empty)
    val pendingDelete = Var(Option.empty[ClosureResponse])

    def resetForm(): Unit =
      form.set(ClosureForm.empty)
      formError.set(None)

    def edit(c: ClosureResponse): Unit =
      form.set(ClosureForm.fromResponse(c))
      formError.set(None)

    def submit(): Unit =
      val current = form.now()
      val effect = current.editingId match
        case Some(id) => ApiClient.updateClosure(id, current.request)
        case None => ApiClient.createClosure(current.request)
      effect.foreach {
        case Right(_) => resetForm(); AppBus.mutatedCalendar()
        case Left(err) => showError(formError, "Salvataggio chiusura aziendale")(err)
      }

    def delete(id: UUID): Unit =
      ApiClient.deleteClosure(id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(id) then resetForm()
          AppBus.mutatedCalendar()
        case Left(err) => showError(formError, "Eliminazione chiusura aziendale")(err)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = Signal.fromValue("Elimina chiusura"),
        message = pendingDelete.signal.map(_.fold("")(c => s"Eliminare la chiusura “${c.reason}” (${CompanyCalendarView.describe(c)})?")),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { c => delete(c.id); pendingDelete.set(None) },
      )

    val formErrors: Signal[List[String]] = form.signal.map(_.errors)

    val data = loadable(AppBus.calendarTicks)(() => ApiClient.listClosures())
    val periodsPager = Pagination(Paging.defaultPageSize)
    val annualsPager = Pagination(Paging.defaultPageSize)

    def kindFields(annual: Boolean): HtmlElement =
      if annual then
        div(
          cls := "col-span-2 grid grid-cols-2 gap-3",
          field("Giorno", textInput(form.signal.map(_.day), Observer[String](v => form.update(_.copy(day = v))), "1–31", "number")),
          field("Mese", staticSelect(form.signal.map(_.month), Observer[String](v => form.update(_.copy(month = v))), ClosureForm.monthOptions)),
        )
      else
        div(
          cls := "col-span-2 grid grid-cols-2 gap-3",
          field("Dal", textInput(form.signal.map(_.startDate), Observer[String](v => form.update(_.copy(startDate = v))), "", "date")),
          field("Al", textInput(form.signal.map(_.endDate), Observer[String](v => form.update(_.copy(endDate = v))), "", "date")),
        )

    def row(c: ClosureResponse, dimmed: Boolean): HtmlElement =
      div(
        cls := s"flex items-center justify-between gap-3 px-4 py-2${if dimmed then " opacity-50" else ""}",
        div(
          cls := "min-w-0",
          div(cls := "font-medium text-slate-800", c.reason),
          div(cls := "text-xs tabular-nums text-slate-500", CompanyCalendarView.describe(c)),
        ),
        roleGated(Role.Admin)(
          div(
            cls := "flex shrink-0 gap-2",
            button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(c))),
            button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(c)))),
          ),
        ),
      )

    def section(title: String, emptyText: String, closures: List[ClosureResponse], pager: Pagination)(
        dimmed: ClosureResponse => Boolean,
    ): HtmlElement =
      div(
        div(cls := "border-b border-slate-100 bg-slate-50 px-4 py-2 text-xs font-semibold uppercase tracking-wide text-slate-500", title),
        if closures.isEmpty then div(cls := "px-4 py-3 text-xs text-slate-400", emptyText)
        else pager.view(closures)(shown => div(cls := "divide-y divide-slate-100", shown.map(c => row(c, dimmed(c))))),
      )

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[22rem_1fr]"),
      roleGated(Role.Admin)(
        card(
          cls := "self-start p-4",
          child <-- form.signal
            .map(_.editingId.isDefined)
            .distinct
            .map(editing => sectionTitle(if editing then "Modifica chiusura" else "Nuova chiusura")),
          div(
            cls := "mt-3 grid grid-cols-2 gap-3",
            div(
              cls := "col-span-2",
              field("Tipo", staticSelect(form.signal.map(_.kind), Observer[String](v => form.update(_.copy(kind = v))), ClosureForm.kindOptions)),
            ),
            // `distinct`: the date/day inputs are rebuilt only when the kind changes, not on every keystroke.
            child <-- form.signal.map(_.isAnnual).distinct.map(kindFields),
            div(
              cls := "col-span-2",
              field("Motivo", textInput(form.signal.map(_.reason), Observer[String](v => form.update(_.copy(reason = v))), "Es. Ferragosto")),
            ),
          ),
          child.maybe <-- form.signal.combineWith(formErrors).map { (state, errors) =>
            Option.when(!state.isBlank && errors.nonEmpty)(ul(cls := "mt-3 list-disc pl-5 text-xs text-rose-600", errors.map(e => li(e))))
          },
          child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
          div(
            cls := "mt-3 flex gap-2",
            button(
              tpe := "button",
              cls := btnPrimary,
              child.text <-- form.signal.map(_.editingId.fold("Crea")(_ => "Salva")),
              disabled <-- formErrors.map(_.nonEmpty),
              onClick --> (_ => submit()),
            ),
            child.maybe <-- form.signal.map(_.editingId.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
          ),
          div(
            cls := "mt-3 text-xs text-slate-400",
            "Nei giorni di chiusura nessun dipendente è disponibile, anche con un override «Giorno lavorativo». La pianificazione si ricalcola da sola.",
          ),
        ),
      ),
      card(
        cls := "overflow-hidden",
        renderResult(data) { closures =>
          if closures.isEmpty then div(cls := "p-4", emptyState("Nessuna chiusura aziendale."))
          else
            val today = BusinessDay.today()
            div(
              section("Periodi", "Nessun periodo di chiusura.", CompanyCalendarView.periods(closures, today), periodsPager)(
                CompanyCalendarView.isPast(_, today),
              ),
              section("Ricorrenze annuali", "Nessuna ricorrenza annuale.", CompanyCalendarView.annuals(closures), annualsPager)(_ => false),
            )
        },
      ),
      confirmDeleteModal,
    )
  end apply
end CompanyCalendarTab
