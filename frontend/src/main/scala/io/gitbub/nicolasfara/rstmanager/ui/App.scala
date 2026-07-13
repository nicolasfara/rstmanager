package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*

/** Application shell: top navigation and the routed page content area. */
object App:

  enum Page derives CanEqual:
    case Planning, Orders, Employees, Customers, Tasks

  private def label(page: Page): String = page match
    case Page.Planning => "Pianificazione"
    case Page.Orders => "Ordini"
    case Page.Employees => "Dipendenti"
    case Page.Customers => "Clienti"
    case Page.Tasks => "Catalogo Task"

  private def render(page: Page): HtmlElement = page match
    case Page.Planning => PlanningPage()
    case Page.Orders => OrdersPage()
    case Page.Employees => EmployeesPage()
    case Page.Customers => CustomersPage()
    case Page.Tasks => TasksPage()

  private def navButton(page: Page, current: Var[Page]): HtmlElement =
    button(
      tpe := "button",
      cls := "rounded-md px-3 py-1.5 text-sm font-medium transition-colors",
      cls <-- current.signal.map(active => if active == page then "bg-slate-900 text-white" else "text-slate-600 hover:bg-slate-200"),
      label(page),
      onClick --> (_ => current.set(page)),
    )

  def apply(): HtmlElement =
    val current = Var(Page.Planning)
    div(
      cls := "min-h-screen bg-slate-50 text-slate-800",
      headerTag(
        cls := "sticky top-0 z-30 border-b border-slate-200 bg-white/90 backdrop-blur",
        div(
          cls := "mx-auto flex max-w-7xl items-center gap-6 px-4 py-3",
          div(
            cls := "flex items-center gap-2",
            div(cls := "h-6 w-6 rounded-md bg-slate-900"),
            span(cls := "text-sm font-semibold tracking-tight text-slate-900", "RST Manager"),
          ),
          navTag(cls := "flex flex-wrap gap-1", Page.values.toList.map(navButton(_, current))),
        ),
      ),
      mainTag(
        cls := "mx-auto max-w-7xl px-4 py-6",
        child <-- current.signal.map(render),
      ),
    )
  end apply
end App
