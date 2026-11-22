package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, ListSearch, Notifications, Pagination, Paging, SubView, TableSort }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/**
 * Supplier registry of the warehouse: the suppliers the material MPNs are bought from, searched and paginated on screen. "+ Nuovo fornitore" and
 * "Modifica" (Admin) open the [[SupplierFormView]] page in place of the list (kept mounted, so search, page and sort survive); "Elimina" asks first.
 */
object SuppliersTab:

  def apply(): HtmlElement =
    val view = SubView[SupplierFormView.Target]()
    val actionError = Var(Option.empty[ApiError])
    val pendingDelete = Var(Option.empty[SupplierResponse])

    // A supplier referenced by a material is refused with 409 `supplier-in-use`; the error details name the materials.
    def delete(supplier: SupplierResponse): Unit =
      actionError.set(None)
      ApiClient.deleteSupplier(supplier.id).foreach {
        case Right(_) =>
          Notifications.success(s"Fornitore ${supplier.name} eliminato")
          AppBus.mutatedWarehouse()
        case Left(err) => showError(actionError, "Eliminazione fornitore")(err)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(s => s"Eliminare il fornitore «${s.name}»?")),
        message = Signal.fromValue("L'operazione non è reversibile. Non è possibile se un materiale di magazzino ne usa gli MPN."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { s => delete(s); pendingDelete.set(None) },
      )

    val data = loadable(AppBus.warehouseTicks)(() => ApiClient.listSuppliers())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[SupplierResponse](pager)(
      "supplier" -> Column.text(_.name),
      "contacts" -> Column.optionalText(s => s.email.orElse(s.phone)),
      "address" -> Column.optionalText(_.address),
    )

    /** True once a successful reload of the registry no longer lists the supplier being edited (never for a new one). */
    def goneMeanwhile(target: SupplierFormView.Target): Signal[Boolean] = target match
      case SupplierFormView.Target.New => Signal.fromValue(false)
      case SupplierFormView.Target.Edit(supplier) => data.map(_.exists(_.exists(SupplierForm.isGone(supplier.id, _)))).distinct

    // ---- List: cards below md, table from md ---------------------------------------------------

    def actionButtons(s: SupplierResponse): List[HtmlElement] =
      List(
        button(
          tpe := "button",
          cls := btnSmall,
          aria.label := s"Modifica ${s.name}",
          "Modifica",
          onClick --> (_ => view.open(SupplierFormView.Target.Edit(s))),
        ),
        button(tpe := "button", cls := btnDanger, aria.label := s"Elimina ${s.name}", "Elimina", onClick --> (_ => pendingDelete.set(Some(s)))),
      )

    def contactsCell(s: SupplierResponse): HtmlElement =
      div(
        cls := "min-w-0 wrap-anywhere",
        s.email.map(e => div(e)),
        s.phone.map(p => div(cls := "text-xs", p)),
        Option.when(s.email.isEmpty && s.phone.isEmpty)(div(cls := "text-xs text-slate-400", "—")),
      )

    def renderRow(s: SupplierResponse): HtmlElement =
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(
          cls := "px-4 py-2 wrap-anywhere",
          div(cls := "font-medium text-slate-800", s.name),
          s.vatNumber.map(vat => div(cls := "text-xs text-slate-500", s"P. IVA $vat")),
          s.notes.map(n => div(cls := "text-xs text-slate-400", n)),
        ),
        td(cls := "px-4 py-2 text-slate-500", contactsCell(s)),
        td(cls := "px-4 py-2 wrap-anywhere text-slate-500", s.address.getOrElse("—")),
        td(cls := "px-4 py-2 text-right", roleGated(Role.Admin)(div(cls := "flex justify-end gap-2", actionButtons(s)))),
      )

    def renderCard(s: SupplierResponse): HtmlElement =
      div(
        cls := "space-y-3 p-4",
        div(
          div(cls := "break-words font-medium text-slate-800", s.name),
          s.vatNumber.map(vat => div(cls := "text-xs text-slate-500", s"P. IVA $vat")),
          s.notes.map(n => div(cls := "break-words text-xs text-slate-400", n)),
        ),
        div(
          cls := "grid grid-cols-2 gap-3 text-sm text-slate-600",
          readField("Contatti", contactsCell(s)),
          readField("Indirizzo", s.address.getOrElse("—")),
        ),
        roleGated(Role.Admin)(div(cls := cardActionsCls, actionButtons(s))),
      )

    div(
      div(
        cls := "space-y-4",
        view.listMods,
        div(
          cls := "flex justify-end",
          roleGated(Role.Admin)(
            button(
              tpe := "button",
              cls := s"$btnPrimary min-h-10 md:min-h-0",
              "+ Nuovo fornitore",
              onClick --> (_ => view.open(SupplierFormView.Target.New)),
            ),
          ),
        ),
        child.maybe <-- actionError.signal.map(_.map(errorBanner(_, () => actionError.set(None)))),
        card(
          cls := "overflow-hidden",
          div(
            cls := "space-y-2 border-b border-slate-100 p-3",
            searchBox(query, "Cerca per nome, P. IVA, contatti…", pager),
            div(cls := "md:hidden", sort.picker(List("supplier" -> "Fornitore", "contacts" -> "Contatti", "address" -> "Indirizzo"))),
          ),
          renderResult(data) { all =>
            if all.isEmpty then div(cls := "p-4", emptyState("Nessun fornitore registrato."))
            else
              searchedPages(sort.sorted(all), query.signal, ListSearch.supplier, pager, "Nessun fornitore corrisponde alla ricerca.") { suppliers =>
                div(
                  // Mobile: stacked cards. Desktop (md+): the table.
                  div(cls := "divide-y divide-slate-100 md:hidden", suppliers.map(renderCard)),
                  div(
                    cls := "hidden overflow-x-auto md:block",
                    table(
                      cls := "w-full text-sm",
                      thead(
                        cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                        tr(
                          sort.header("supplier", "Fornitore", "px-4 py-2"),
                          sort.header("contacts", "Contatti", "px-4 py-2"),
                          sort.header("address", "Indirizzo", "px-4 py-2"),
                          th(cls := "px-4 py-2", span(cls := "sr-only", "Azioni")),
                        ),
                      ),
                      tbody(suppliers.map(renderRow)),
                    ),
                  ),
                )
              }
          },
        ),
      ),
      view.page(target => SupplierFormView(target, () => view.close(), goneMeanwhile(target))),
      confirmDeleteModal,
    )
  end apply
end SuppliersTab
