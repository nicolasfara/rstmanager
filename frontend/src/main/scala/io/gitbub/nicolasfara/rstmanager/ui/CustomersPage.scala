package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/**
 * Customer registry: a full-width, searched and paged list; "+ Nuovo cliente" and "Modifica" open the [[CustomerFormView]] page in its place. Needed
 * to reference a customer when creating an order.
 */
object CustomersPage:

  private def boatCell(c: CustomerResponse): HtmlElement =
    val mooring = List(c.port, c.boatBerth.map(b => s"posto $b")).flatten.mkString(", ")
    if c.boatName.isEmpty && c.boatModel.isEmpty && mooring.isEmpty then div(cls := "text-xs text-slate-400", "—")
    else
      div(
        c.boatName.map(n => div(n)),
        c.boatModel.map(m => div(cls := "text-xs", m)),
        Option.when(mooring.nonEmpty)(div(cls := "text-xs text-slate-400", mooring)),
      )

  private def locationLine(c: CustomerResponse): Option[String] =
    Some(List(c.city, c.country).flatten.mkString(", ")).filter(_.nonEmpty)

  private def contactsCell(c: CustomerResponse): HtmlElement =
    if c.email.isEmpty && c.phone.isEmpty then div(cls := "text-xs text-slate-400", "—")
    else div(c.email.map(e => div(e)), c.phone.map(p => div(cls := "text-xs", p)))

  private def displayName(c: CustomerResponse): String = c.businessName.getOrElse(s"${c.name} ${c.surname}")

  private val listErrorId = "cust-list-error"

  def apply(): HtmlElement =
    val view = SubView[CustomerTarget]()
    val listError = Var(Option.empty[ApiError])

    /** Opens the form page of `target`; a list message (e.g. a refused delete) would be stale on return, so it goes. */
    def openPage(target: CustomerTarget): Unit =
      listError.set(None)
      view.open(target)
    val pendingDelete = Var(Option.empty[CustomerResponse])

    def delete(c: CustomerResponse): Unit =
      ApiClient.deleteCustomer(c.id).foreach {
        case Right(_) =>
          listError.set(None)
          Notifications.success(s"Cliente ${displayName(c)} eliminato")
          AppBus.mutatedCustomers()
        case Left(err) =>
          showBannerError(listError, "Eliminazione cliente")(err)
          FormFocus.focus(listErrorId)
      }

    val confirmDeleteDialog =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(c => s"Eliminare il cliente «${displayName(c)}»?")),
        message = Signal.fromValue("Il cliente viene rimosso dall'anagrafica. L'operazione non è reversibile."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { c => delete(c); pendingDelete.set(None) },
      )

    val data = loadable(AppBus.customersTicks)(() => ApiClient.listCustomers())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[CustomerResponse](pager)(
      "customer" -> Column.text(displayName),
      "contacts" -> Column.optionalText(c => c.email.orElse(c.phone)),
      "boat" -> Column.optionalText(c => c.boatName.orElse(c.boatModel)),
      "type" -> Column.text(c => CustomerForm.typeLabel(c.customerType)),
    )

    def typeBadge(c: CustomerResponse): HtmlElement = badge(CustomerForm.typeLabel(c.customerType), "bg-slate-50 text-slate-600 border-slate-200")

    def actionButtons(c: CustomerResponse): List[HtmlElement] =
      List(
        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => openPage(CustomerTarget.Edit(c)))),
        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(c)))),
      )

    def renderRow(c: CustomerResponse): HtmlElement =
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(
          cls := "px-4 py-2",
          div(cls := "font-medium text-slate-800", displayName(c)),
          c.businessName.map(_ => div(cls := "text-xs text-slate-500", s"${c.name} ${c.surname}")),
          locationLine(c).map(l => div(cls := "text-xs text-slate-400", l)),
        ),
        td(cls := "px-4 py-2 text-slate-500", contactsCell(c)),
        td(cls := "px-4 py-2 text-slate-500", boatCell(c)),
        td(cls := "px-4 py-2", typeBadge(c)),
        td(
          cls := "px-4 py-2 text-right",
          roleGated(Role.Admin)(div(cls := "flex justify-end gap-2", actionButtons(c))),
        ),
      )

    def renderCard(c: CustomerResponse): HtmlElement =
      div(
        cls := "space-y-3 p-4",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "break-words font-medium text-slate-800", displayName(c)),
            c.businessName.map(_ => div(cls := "text-xs text-slate-500", s"${c.name} ${c.surname}")),
            locationLine(c).map(l => div(cls := "text-xs text-slate-400", l)),
          ),
          div(cls := "shrink-0", typeBadge(c)),
        ),
        div(
          cls := "grid grid-cols-2 gap-3 text-sm text-slate-600",
          div(cls := "min-w-0 break-words", div(cls := "text-xs font-medium text-slate-500", "Contatti"), contactsCell(c)),
          div(cls := "min-w-0 break-words", div(cls := "text-xs font-medium text-slate-500", "Barca"), boatCell(c)),
        ),
        roleGated(Role.Admin)(div(cls := cardActionsCls, actionButtons(c))),
      )

    div(
      div(
        view.listMods,
        RegistryPage.listHeader("Clienti", "+ Nuovo cliente", () => openPage(CustomerTarget.New)),
        RegistryPage.listError(listError, listErrorId),
        card(
          cls := "overflow-hidden",
          div(
            cls := "space-y-2 border-b border-slate-100 p-3",
            searchBox(query, "Cerca per nome, ragione sociale, contatti, barca…", pager),
            div(cls := "md:hidden", sort.picker(List("customer" -> "Cliente", "contacts" -> "Contatti", "boat" -> "Barca", "type" -> "Tipo"))),
          ),
          renderResult(data) { customers =>
            if customers.isEmpty then div(cls := "p-4", emptyState("Nessun cliente registrato."))
            else
              searchedPages(sort.sorted(customers), query.signal, ListSearch.customer, pager, "Nessun cliente corrisponde alla ricerca.") { shown =>
                div(
                  // Mobile: stacked cards. Desktop (md+): the table.
                  div(cls := "divide-y divide-slate-100 md:hidden", shown.map(renderCard)),
                  div(
                    cls := "hidden overflow-x-auto md:block",
                    table(
                      cls := "w-full text-sm",
                      thead(
                        cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                        tr(
                          sort.header("customer", "Cliente", "px-4 py-2"),
                          sort.header("contacts", "Contatti", "px-4 py-2"),
                          sort.header("boat", "Barca", "px-4 py-2"),
                          sort.header("type", "Tipo", "px-4 py-2"),
                          th(cls := "px-4 py-2"),
                        ),
                      ),
                      tbody(shown.map(renderRow)),
                    ),
                  ),
                )
              }
          },
        ),
      ),
      view.page(target => CustomerFormView(target, data, () => view.close())),
      confirmDeleteDialog,
    )
  end apply
end CustomersPage
