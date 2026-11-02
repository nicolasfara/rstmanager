package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import com.raquo.laminar.api.L.*

/** A section of the warehouse page. */
enum WarehouseTab(val label: String) derives CanEqual:
  case Products extends WarehouseTab("Prodotti")
  case Suppliers extends WarehouseTab("Fornitori")
  case History extends WarehouseTab("Storico")
  case Flows extends WarehouseTab("Entrate/Uscite")

/** Warehouse ("Magazzino"): a pill tab bar over the product and supplier registries, the day-by-day history, and the flows report. */
object WarehousePage:

  /**
   * Today as `YYYY-MM-DD` in the business calendar (Europe/Rome, see [[BusinessDay]]): the default and upper bound of the movement, history, and
   * report days, the same bound the server checks.
   */
  def today(): String = BusinessDay.today()

  /** Selected tab, kept at module level so it survives navigating away from the page and back within the session. */
  private val currentTab = Var(WarehouseTab.Products)

  private val tabBase = "rounded-full px-3 py-1 text-xs font-medium border transition-colors"

  private def tabCls(active: Boolean): String =
    if active then s"$tabBase bg-slate-900 text-white border-slate-900"
    else s"$tabBase bg-white text-slate-600 border-slate-300 hover:bg-slate-100"

  private def render(tab: WarehouseTab): HtmlElement = tab match
    case WarehouseTab.Products => ProductsTab()
    case WarehouseTab.Suppliers => SuppliersTab()
    case WarehouseTab.History => HistoryTab()
    case WarehouseTab.Flows => FlowsTab()

  def apply(): HtmlElement =
    div(
      cls := "space-y-4",
      div(
        cls := "flex flex-wrap gap-2",
        WarehouseTab.values.toList.map { tab =>
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
end WarehousePage
