package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.auth.{ AuthService, AuthState, AuthUser }

/** Application shell: auth gate, top navigation, and the routed page content area. */
object App:

  enum Page derives CanEqual:
    case Planning, Orders, Employees, Customers, Tasks, Manufacturings

  private val catalogPages: List[Page] = List(Page.Tasks, Page.Manufacturings)
  private val topLevelPages: List[Page] = Page.values.toList.filterNot(catalogPages.contains)

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

  private def catalogDropdown(current: Var[Page]): HtmlElement =
    val open = Var(false)
    div(
      cls := "relative",
      button(
        tpe := "button",
        cls := "flex items-center gap-1 rounded-md px-3 py-1.5 text-sm font-medium transition-colors",
        cls <-- current.signal.map(active =>
          if catalogPages.contains(active) then "bg-slate-900 text-white" else "text-slate-600 hover:bg-slate-200",
        ),
        "Cataloghi",
        span(cls := "text-[10px]", child.text <-- open.signal.map(o => if o then "▲" else "▼")),
        onClick.stopPropagation --> (_ => open.update(!_)),
      ),
      child.maybe <-- open.signal.map { isOpen =>
        Option.when(isOpen)(
          div(
            cls := "absolute left-0 top-full z-40 mt-1 w-52 rounded-md border border-slate-200 bg-white py-1 shadow-lg",
            catalogPages.map { page =>
              button(
                tpe := "button",
                cls := "block w-full px-3 py-1.5 text-left text-sm font-medium transition-colors",
                cls <-- current.signal.map(active =>
                  if active == page then "bg-slate-100 text-slate-900" else "text-slate-600 hover:bg-slate-100",
                ),
                label(page),
                onClick --> (_ =>
                  current.set(page); open.set(false)
                ),
              )
            },
          ),
        )
      },
      documentEvents(_.onClick) --> (_ => open.set(false)),
    )
  end catalogDropdown

  private def errorDetails(details: List[String]): com.raquo.laminar.nodes.ChildNode.Base =
    if details.nonEmpty then ul(cls := "mt-1 list-disc pl-5 text-xs text-rose-700", details.map(detail => li(detail)))
    else emptyNode

  private def globalErrorBanner: HtmlElement =
    div(
      child.maybe <-- ErrorCenter.latestSignal.map(
        _.map(report =>
          div(
            cls := "fixed bottom-4 left-4 right-4 z-50 max-w-md rounded-md border border-rose-200 bg-white p-3 text-sm shadow-lg sm:left-auto sm:w-full",
            div(
              cls := "flex items-start justify-between gap-3",
              div(
                cls := "min-w-0",
                div(cls := "text-xs font-semibold uppercase tracking-wide text-rose-500", report.context),
                div(cls := "mt-0.5 break-words font-medium text-rose-800", report.error.message),
                errorDetails(report.error.details),
              ),
              button(
                tpe := "button",
                cls := "shrink-0 rounded-md px-2 py-1 text-xs font-medium text-slate-500 hover:bg-slate-100 hover:text-slate-700",
                "Chiudi",
                onClick --> (_ => ErrorCenter.dismiss(report.id)),
              ),
            ),
          ),
        ),
      ),
    )

  def apply(): HtmlElement =
    ErrorCenter.installRuntimeHandlers()
    div(
      cls := "min-h-screen bg-slate-50 text-slate-800",
      child <-- AuthService.stateSignal.map {
        case AuthState.Initializing => div(cls := "flex min-h-[70vh] items-center justify-center", Components.spinner)
        case AuthState.Anonymous => LandingPage()
        case AuthState.Authenticated(user) => shell(user)
      },
      globalErrorBanner,
    )
  end apply

  private def logoutButton(user: AuthUser): HtmlElement =
    div(
      cls := "flex items-center gap-2",
      span(cls := "hidden sm:inline text-xs font-medium text-slate-500", user.username),
      button(
        tpe := "button",
        cls := "rounded-md border border-slate-300 px-2.5 py-1 text-xs font-medium text-slate-600 hover:bg-slate-100 transition-colors",
        "Esci",
        onClick --> (_ => AuthService.logout()),
      ),
    )

  private def shell(user: AuthUser): HtmlElement =
    val current = Var(Page.Planning)
    val menuOpen = Var(false)
    div(
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
          navTag(
            cls := "hidden sm:flex flex-wrap gap-1 grow",
            topLevelPages.map(navButton(_, current)),
            catalogDropdown(current),
          ),
          div(
            cls := "flex items-center gap-2",
            logoutButton(user),
            // Hamburger toggle (mobile only)
            button(
              tpe := "button",
              cls := "sm:hidden rounded-md p-1.5 text-slate-600 hover:bg-slate-100 transition-colors",
              onClick --> (_ => menuOpen.update(!_)),
              child.text <-- menuOpen.signal.map(open => if open then "✕" else "☰"),
            ),
          ),
        ),
        // Mobile dropdown
        child.maybe <-- menuOpen.signal.map { open =>
          Option.when(open)(
            div(
              cls := "sm:hidden border-t border-slate-200 px-4 py-2",
              div(
                cls := "flex flex-col gap-1",
                topLevelPages.map { page =>
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
                div(cls := "mt-1 border-t border-slate-200 pt-2 px-3 text-xs font-semibold uppercase tracking-wide text-slate-400", "Cataloghi"),
                catalogPages.map { page =>
                  button(
                    tpe := "button",
                    cls := "w-full rounded-md px-3 py-2 pl-6 text-left text-sm font-medium transition-colors",
                    cls <-- current.signal.map(active => if active == page then "bg-slate-900 text-white" else "text-slate-600 hover:bg-slate-100"),
                    label(page),
                    onClick --> (_ =>
                      current.set(page); menuOpen.set(false)
                    ),
                  )
                },
                div(cls := "mt-1 border-t border-slate-200 pt-2 px-3 text-xs font-medium text-slate-500", user.username),
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
  end shell
end App
