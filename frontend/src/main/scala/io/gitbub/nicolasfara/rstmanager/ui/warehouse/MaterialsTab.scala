package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, ListSearch, Notifications, Pagination, Paging, SubView, TableSort, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/** What the materials tab shows in place of its list. */
enum MaterialTarget derives CanEqual:
  /** "+ Nuovo materiale" (Admin). */
  case New

  /** "Modifica" (Admin), on the snapshot the user clicked. */
  case Edit(material: MaterialResponse)

  /** The material sheet ("Movimento", Operator / "Movimenti", Viewer), focused on its form or on its movements. */
  case Sheet(material: MaterialResponse, focus: SheetFocus)

  /** "Carica da Excel" (Admin). */
  case Import
end MaterialTarget

/**
 * Material registry of the warehouse: internal code, unit of measure, customer price, and the MPNs it is bought as (each tied to a supplier). The
 * material suppliers are derived from the MPNs. Removing a material archives it, so it stays available to the history.
 *
 * The list (full width) shows today's stock and value of each material — stacked cards below `lg`, a compact table from `lg` (see [[MaterialCards]])
 * — with at most three actions per material: "Movimento" (Operator) and "Movimenti" (Viewer) open the material sheet ([[MaterialSheetView]]: the
 * movement form over the movements list) focused on the form or on the list, and "Modifica" (Admin) opens [[MaterialFormView]], where the material is
 * also archived. "+ Nuovo materiale" and "Carica da Excel" (Admin, [[MaterialImportView]]) sit in the list toolbar. Each one is a page shown in place
 * of the list ([[SubView]]): the list stays mounted, so search, page, sort, and filters survive, and going back restores the scroll and the focus.
 * The outcome of the last upload stays above the list until dismissed.
 */
object MaterialsTab:

  def apply(): HtmlElement =
    val showArchived = Var(false)
    val lastImport = Var(Option.empty[MaterialImportResponse])
    val view = SubView[MaterialTarget]()

    val materials = loadable(AppBus.warehouseTicks.combineWith(showArchived.signal))(() => ApiClient.listMaterials(showArchived.now()))
    val suppliers = loadable(AppBus.warehouseTicks)(() => ApiClient.listSuppliers())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[MaterialResponse](pager)(
      "code" -> Column.text(_.internalCode),
      "description" -> Column.text(_.description),
      "type" -> Column.optionalText(_.materialType),
      "unit" -> Column.text(m => WarehouseFormats.unitShortLabel(m.unit)),
      "stock" -> Column.decimal(_.stock.quantity),
      "value" -> Column.optionalDecimal(_.stock.value),
      "price" -> Column.decimal(_.customerPrice),
      "suppliers" -> Column.optionalText(_.suppliers.headOption.map(_.name)),
    )

    def imported(summary: MaterialImportResponse): Unit =
      lastImport.set(Some(summary))
      Notifications.success(MaterialImport.confirmationText(summary))
      // The user may have left the page during the upload (and opened another one): close only the import page.
      view.closeIfShowing(MaterialTarget.Import)

    def renderPage(target: MaterialTarget): HtmlElement = target match
      case MaterialTarget.New => MaterialFormView(None, materials, suppliers, () => view.close())
      case MaterialTarget.Edit(material) => MaterialFormView(Some(material), materials, suppliers, () => view.close())
      case MaterialTarget.Sheet(material, focus) =>
        MaterialSheetView(material, focus, materials, latest => view.replace(MaterialTarget.Edit(latest)), () => view.close())
      case MaterialTarget.Import => MaterialImportView(() => view.close(), imported)

    // ---- List: cards below lg, compact table from lg -------------------------------------------

    /** The actions of a material (at most three): "Movimento" (Operator), "Movimenti" (Viewer), "Modifica" (Admin); an archived one only lists. */
    def actionButtons(material: MaterialResponse): List[Modifier[HtmlElement]] =
      List(
        Option.when(!material.archived)(
          roleGated(Role.Operator)(
            button(
              tpe := "button",
              cls := btnSmall,
              aria.label := s"Registra un movimento di ${material.internalCode}",
              "Movimento",
              onClick --> (_ => view.open(MaterialTarget.Sheet(material, SheetFocus.Movement))),
            ),
          ),
        ),
        roleGated(Role.Viewer)(
          button(
            tpe := "button",
            cls := btnSmall,
            aria.label := s"Movimenti di ${material.internalCode}",
            "Movimenti",
            onClick --> (_ => view.open(MaterialTarget.Sheet(material, SheetFocus.History))),
          ),
        ),
        Option.when(!material.archived)(
          roleGated(Role.Admin)(
            button(
              tpe := "button",
              cls := btnSmall,
              aria.label := s"Modifica ${material.internalCode}",
              "Modifica",
              onClick --> (_ => view.open(MaterialTarget.Edit(material))),
            ),
          ),
        ),
      )
    end actionButtons

    def renderCard(material: MaterialResponse): HtmlElement =
      MaterialCards.listCard(WarehouseViews.materialSummary(material), Nil)(actionButtons(material)*)

    def renderRow(material: MaterialResponse): HtmlElement =
      val summary = WarehouseViews.materialSummary(material)
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        cls := (if material.archived then "bg-slate-50 text-slate-400" else ""),
        td(cls := "px-3 py-2", MaterialCards.materialCell(summary)),
        td(cls := "px-3 py-2 text-right", MaterialCards.stockCell(summary)),
        td(cls := "px-3 py-2 text-right", MaterialCards.valuePriceCell(summary)),
        td(cls := "px-3 py-2 text-slate-600", MaterialCards.suppliersCell(summary)),
        td(cls := "px-3 py-2", div(cls := "flex flex-col items-end gap-1.5", actionButtons(material))),
      )

    val importBanner: Modifier[HtmlElement] =
      child.maybe <-- lastImport.signal.map(_.map { summary =>
        div(
          role := "status",
          cls := "flex items-start justify-between gap-3 border-b border-emerald-200 bg-emerald-50 px-4 py-2 text-sm text-emerald-800",
          div(
            cls := "min-w-0",
            div(MaterialImport.confirmationText(summary)),
            // The problems of the ignored materials stay at hand, to fix the file and load it again.
            Option.when(summary.ignored.nonEmpty)(
              detailsTag(
                cls := "mt-1 text-amber-800",
                summaryTag(cls := "cursor-pointer text-xs font-medium", s"Errori dei materiali ignorati (${summary.problems.size})"),
                MaterialImportView.problemsList(summary),
              ),
            ),
          ),
          button(
            tpe := "button",
            cls := "inline-flex min-h-10 shrink-0 items-center rounded-md px-2 py-0.5 text-xs font-medium text-emerald-700 hover:bg-emerald-100 md:min-h-0",
            "Chiudi",
            onClick --> (_ => lastImport.set(None)),
          ),
        )
      })

    def tableCard: HtmlElement =
      card(
        cls := "overflow-hidden",
        div(
          cls := "space-y-2 border-b border-slate-100 p-3",
          div(
            cls := "flex flex-wrap items-center gap-2",
            div(cls := "w-full sm:w-80", searchBox(query, "Cerca per codice, descrizione, MPN, fornitore…", pager)),
            label(
              cls := "inline-flex min-h-10 items-center gap-2 text-xs font-medium text-slate-600 sm:mr-auto lg:min-h-0",
              input(typ := "checkbox", checked <-- showArchived.signal, onChange.mapToChecked --> { show => showArchived.set(show); pager.reset() }),
              "Mostra archiviati",
            ),
            roleGated(Role.Admin)(
              button(
                tpe := "button",
                cls := s"$btnGhost min-h-10 md:min-h-0",
                "Carica da Excel",
                title := "Carica materiali da un file Excel",
                onClick --> (_ =>
                  lastImport.set(None); view.open(MaterialTarget.Import)
                ),
              ),
            ),
            roleGated(Role.Admin)(
              button(
                tpe := "button",
                cls := s"$btnPrimary min-h-10 md:min-h-0",
                "+ Nuovo materiale",
                onClick --> (_ => view.open(MaterialTarget.New)),
              ),
            ),
          ),
          div(
            cls := "lg:hidden",
            sort.picker(
              List(
                "code" -> "Codice",
                "description" -> "Descrizione",
                "type" -> "Tipologia",
                "unit" -> "UdM",
                "stock" -> "Giacenza",
                "value" -> "Valore",
                "price" -> "Prezzo cliente",
                "suppliers" -> "Fornitori",
              ),
            ),
          ),
        ),
        importBanner,
        renderResult(materials) { all =>
          if all.isEmpty then div(cls := "p-4", emptyState("Nessun materiale in magazzino."))
          else
            searchedPages(sort.sorted(all), query.signal, ListSearch.material, pager, "Nessun materiale corrisponde alla ricerca.") { list =>
              div(
                // Below lg: stacked cards. From lg: the compact table.
                div(cls := "divide-y divide-slate-100 lg:hidden", list.map(renderCard)),
                div(
                  cls := "hidden overflow-x-auto lg:block",
                  table(
                    cls := "w-full text-sm",
                    thead(
                      cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                      tr(
                        sort.header("px-3 py-2", "code" -> "Codice", "description" -> "Descrizione", "type" -> "Tipo", "unit" -> "UdM"),
                        sort.header("stock", "Giacenza", "px-3 py-2 text-right"),
                        sort.header("px-3 py-2 text-right", "value" -> "Valore", "price" -> "Prezzo"),
                        sort.header("suppliers", "Fornitori", "px-3 py-2"),
                        th(cls := "px-3 py-2", span(cls := "sr-only", "Azioni")),
                      ),
                    ),
                    tbody(list.map(renderRow)),
                  ),
                ),
              )
            }
        },
      )

    div(
      div(view.listMods, tableCard),
      view.page(renderPage),
    )
  end apply
end MaterialsTab
