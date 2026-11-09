package io.gitbub.nicolasfara.rstmanager.ui.employees

import com.raquo.laminar.api.L.*

/** A section of the employees page. */
enum EmployeesPageTab(val label: String) derives CanEqual:
  case Employees extends EmployeesPageTab("Dipendenti")
  case Calendar extends EmployeesPageTab("Calendario aziendale")

/** Employees ("Dipendenti"): a pill tab bar over the employee registry (with the hours overrides) and the company calendar of closures. */
object EmployeesPage:

  /** Selected tab, kept at module level so it survives navigating away from the page and back within the session. */
  private val currentTab = Var(EmployeesPageTab.Employees)

  private val tabBase = "rounded-full px-3 py-1 text-xs font-medium border transition-colors"

  private def tabCls(active: Boolean): String =
    if active then s"$tabBase bg-slate-900 text-white border-slate-900"
    else s"$tabBase bg-white text-slate-600 border-slate-300 hover:bg-slate-100"

  private def render(tab: EmployeesPageTab): HtmlElement = tab match
    case EmployeesPageTab.Employees => EmployeesTab()
    case EmployeesPageTab.Calendar => CompanyCalendarTab()

  def apply(): HtmlElement =
    div(
      cls := "space-y-4",
      div(
        cls := "flex flex-wrap gap-2",
        EmployeesPageTab.values.toList.map { tab =>
          button(
            tpe := "button",
            cls <-- currentTab.signal.map(active => tabCls(active == tab)),
            tab.label,
            onClick --> (_ => currentTab.set(tab)),
          )
        },
      ),
      // `distinct`: re-clicking the open tab must not remount it (and lose the form being edited).
      child <-- currentTab.signal.distinct.map(render),
    )
end EmployeesPage
