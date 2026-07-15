package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*

/** Application shell: top navigation and the routed page content area. */
object App:

  enum Page derives CanEqual:
    case Planning, Orders, Employees, Customers, Tasks, Manufacturings

  private def label(page: Page): String = page match
    case Page.Planning => "Pianificazione"
    case Page.Orders => "Ordini"
    case Page.Employees => "Dipendenti"
    case Page.Customers => "Clienti"
    case Page.Tasks => "Catalogo Task"
    case Page.Manufacturings => "Catalogo Lavorazioni"

  private def render(page: Page): HtmlElement = page match
    case Page.Planning => PlanningPage()
    case Page.Orders => OrdersPage()
    case Page.Employees => EmployeesPage()
    case Page.Customers => CustomersPage()
    case Page.Tasks => TasksPage()
    case Page.Manufacturings => ManufacturingsPage()

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
    val menuOpen = Var(false)
    div(
      cls := "min-h-screen bg-slate-50 text-slate-800",
      headerTag(
        cls := "sticky top-0 z-30 border-b border-slate-200 bg-white/90 backdrop-blur",
        div(
          cls := "mx-auto flex max-w-7xl items-center justify-between sm:justify-start gap-4 sm:gap-6 px-4 py-3",
          div(
            cls := "flex items-center gap-2",
            div(cls := "h-6 w-6 rounded-md bg-slate-900"),
            span(cls := "text-sm font-semibold tracking-tight text-slate-900", "RST Manager"),
          ),
          // Desktop nav
          navTag(cls := "hidden sm:flex flex-wrap gap-1", Page.values.toList.map(navButton(_, current))),
          // Hamburger toggle (mobile only)
          button(
            tpe := "button",
            cls := "sm:hidden rounded-md p-1.5 text-slate-600 hover:bg-slate-100 transition-colors",
            onClick --> (_ => menuOpen.update(!_)),
            child.text <-- menuOpen.signal.map(open => if open then "✕" else "☰"),
          ),
        ),
        // Mobile dropdown
        child.maybe <-- menuOpen.signal.map { open =>
          Option.when(open)(
            div(
              cls := "sm:hidden border-t border-slate-200 px-4 py-2",
              div(
                cls := "flex flex-col gap-1",
                Page.values.toList.map { page =>
                  button(
                    tpe := "button",
                    cls := "w-full rounded-md px-3 py-2 text-left text-sm font-medium transition-colors",
                    cls <-- current.signal.map(active => if active == page then "bg-slate-900 text-white" else "text-slate-600 hover:bg-slate-100"),
                    label(page),
                    onClick --> (_ =>
                      current.set(page); menuOpen.set(false)
                    ),
                  )
                },
              ),
            ),
          )
        },
      ),
      mainTag(
        cls := "mx-auto max-w-7xl px-4 sm:px-6 py-6",
        child <-- current.signal.map(render),
      ),
    )
  end apply
end App
