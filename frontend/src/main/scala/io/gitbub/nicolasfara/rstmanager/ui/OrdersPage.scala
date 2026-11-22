package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.{ AuthService, Role }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/**
 * Orders: filterable list with lifecycle transitions. "+ Nuovo ordine", "Modifica" and "Dettagli" open the order form page ([[OrderFormView]]) in
 * place of the list, which stays mounted but hidden ([[SubView]]), so its filters, search, page and scroll survive. Completing or delivering an order
 * asks for an explicit acknowledgement first.
 */
object OrdersPage:

  /** Status filter chips: filter value -> label. An empty value means "all". */
  private val statusFilters = List(
    "" -> "Tutti",
    "in_progress" -> "In corso",
    "suspended" -> "Sospesi",
    "completed" -> "Completati",
    "delivered" -> "Consegnati",
    "cancelled" -> "Annullati",
  )

  private val chipBase = "rounded-full px-3 py-1 text-xs font-medium border transition-colors"
  private def chipCls(active: Boolean): String =
    if active then s"$chipBase bg-slate-900 text-white border-slate-900"
    else s"$chipBase bg-white text-slate-600 border-slate-300 hover:bg-slate-100"

  /** A lifecycle transition awaiting an explicit user acknowledgement. */
  private final case class ConfirmData(orderId: UUID, action: String, title: String, message: String)

  def apply(): HtmlElement =
    val pageError = Var(Option.empty[ApiError])
    val view = SubView[OrderTarget]()
    val statusFilter = Var("")
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val ordersData = loadable(AppBus.ordersTicks)(() => ApiClient.listOrders())
    val customersData = loadable(AppBus.customersTicks)(() => ApiClient.listCustomers())
    val tasksData = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())
    val manufacturingCatalogData = loadable(AppBus.manufacturingsTicks)(() => ApiClient.listManufacturingCatalog())
    val employeesData = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val manufacturingCatalogs = Var(List.empty[ManufacturingCatalogResponse])

    val customersMap: Signal[Map[UUID, String]] = customersData.map {
      case Some(Right(list)) => list.map(c => c.id -> c.businessName.getOrElse(s"${c.name} ${c.surname}")).toMap
      case _ => Map.empty
    }
    val customerOptions: Signal[List[(String, String)]] = customersData.map {
      case Some(Right(list)) => list.map(c => c.id.toString -> c.businessName.getOrElse(s"${c.name} ${c.surname}"))
      case _ => Nil
    }
    val employeeOptions: Signal[List[(String, String)]] = employeesData.map {
      case Some(Right(list)) => ("" -> "— auto (pianificazione sceglie) —") :: list.map(e => e.id.toString -> s"${e.name} ${e.surname}")
      case _ => List("" -> "—")
    }

    // The form pages get the shared loaded data; each opening mounts a fresh page, so it starts from a fresh draft.
    val formDeps = OrderFormView.Deps(
      customerOptions,
      tasksData.map {
        case Some(Right(list)) => list
        case _ => Nil
      },
      manufacturingCatalogs.signal,
      employeeOptions,
      ordersData.map {
        case Some(Right(list)) => list.map(_.number)
        case _ => Nil
      },
      ordersData.map {
        case Some(Right(list)) => Some(list.map(_.id).toSet)
        case _ => None
      },
    )

    // ---- Row actions -----------------------------------------------------------------------------
    val confirm = Var(Option.empty[ConfirmData])

    def transition(id: UUID, action: String): Unit =
      ApiClient.orderTransition(id, TransitionRequest(action, None)).foreach {
        case Right(_) => AppBus.mutatedOrders()
        case Left(err) => showError(pageError, "Transizione ordine")(err)
      }

    def cancelOrder(id: UUID): Unit =
      ApiClient.deleteOrder(id, None).foreach {
        case Right(_) => AppBus.mutatedOrders()
        case Left(err) => showError(pageError, "Annullamento ordine")(err)
      }

    /** Completing/delivering/cancelling changes the order lifecycle, so it goes through an acknowledgement dialog first. */
    def requestTransition(order: OrderResponse, action: String): Unit = action match
      case "complete" =>
        confirm.set(
          Some(
            ConfirmData(
              order.id,
              action,
              "Completare l'ordine?",
              s"Stai per segnare l'ordine ${order.number} come completato. Verifica che tutte le lavorazioni siano concluse.",
            ),
          ),
        )
      case "deliver" =>
        confirm.set(
          Some(
            ConfirmData(
              order.id,
              action,
              "Confermare la consegna?",
              s"Stai per segnare l'ordine ${order.number} come consegnato. L'operazione è definitiva.",
            ),
          ),
        )
      case "cancel" =>
        confirm.set(
          Some(
            ConfirmData(
              order.id,
              action,
              "Annullare l'ordine?",
              s"Stai per annullare l'ordine ${order.number}: verrà tolto dalla pianificazione. Potrai riaprirlo in seguito con \"Riapri\".",
            ),
          ),
        )
      case _ => transition(order.id, action)

    /** Runs an acknowledged action: cancellation goes through the delete endpoint, everything else is a lifecycle transition. */
    def runConfirmed(data: ConfirmData): Unit =
      if data.action == "cancel" then cancelOrder(data.orderId) else transition(data.orderId, data.action)

    def transitionButtons(order: OrderResponse): List[HtmlElement] =
      val actions = order.status match
        case "in_progress" => List("suspend" -> "Sospendi", "complete" -> "Completa")
        case "suspended" => List("reactivate" -> "Riattiva", "complete" -> "Completa")
        case "completed" => List("deliver" -> "Consegna", "reopen" -> "Riapri")
        case "cancelled" => List("reopen" -> "Riapri")
        case _ => Nil
      actions.map { case (action, label) =>
        button(tpe := "button", cls := btnSmall, label, onClick --> (_ => requestTransition(order, action)))
      }

    // ---- Confirmation (ACK) dialog ---------------------------------------------------------------
    val ackDialog =
      confirmDialog(
        isOpen = confirm.signal.map(_.isDefined),
        titleText = confirm.signal.map(_.fold("")(_.title)),
        message = confirm.signal.map(_.fold("")(_.message)),
        confirmLabel = "Conferma",
        tone = DialogTone.Neutral,
        onCancel = () => confirm.set(None),
        onConfirm = () => confirm.now().foreach { c => runConfirmed(c); confirm.set(None) },
      )

    // ---- Row / table rendering -------------------------------------------------------------------
    def orderActionButtons(order: OrderResponse): List[HtmlElement] =
      val canOperate = AuthService.currentHasRole(Role.Operator)
      val detailsButton = button(
        tpe := "button",
        cls := btnSmall,
        if OrderStatus.isEditable(order.status) && canOperate then "Modifica" else "Dettagli",
        onClick --> (_ => view.open(OrderFormView.targetFor(order))),
      )
      // Cancelling removes the order (DELETE), which the server restricts to admins.
      val cancelButton =
        if order.status == "cancelled" || !AuthService.currentHasRole(Role.Admin) then Nil
        else List(button(tpe := "button", cls := btnDanger, "Annulla ordine", onClick --> (_ => requestTransition(order, "cancel"))))
      detailsButton :: (if canOperate then transitionButtons(order) else Nil) ++ cancelButton

    def renderOrderCard(order: OrderResponse): HtmlElement =
      val taskCount = order.manufacturings.map(_.tasks.size).sum
      div(
        cls := "space-y-3 p-4",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "break-words font-medium text-slate-800", order.number),
            div(
              cls := "mt-1 text-sm text-slate-600",
              child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId))),
            ),
            order.description.map(d => div(cls := "mt-1 break-words text-xs text-slate-400", d)).getOrElse(emptyNode),
          ),
          div(cls := "shrink-0", statusBadge(order.status)),
        ),
        div(
          cls := "grid grid-cols-2 gap-3 text-sm",
          readField("Priorità", OrderFormView.priorityBadge(order.priority)),
          readField("Consegna", Formats.date(order.deliveryDate)),
          readField("Lavorazioni", s"${order.manufacturings.size} lav."),
          readField("Task", s"$taskCount task"),
        ),
        div(
          cls := cardActionsCls,
          orderActionButtons(order),
        ),
      )
    end renderOrderCard

    def renderRow(order: OrderResponse): HtmlElement =
      val taskCount = order.manufacturings.map(_.tasks.size).sum
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(
          cls := "px-4 py-2",
          div(cls := "font-medium text-slate-800", order.number),
          order.description.map(d => div(cls := "text-xs text-slate-400", d)).getOrElse(emptyNode),
        ),
        td(cls := "px-4 py-2 text-slate-600", child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId)))),
        td(cls := "px-4 py-2", statusBadge(order.status)),
        td(cls := "px-4 py-2", OrderFormView.priorityBadge(order.priority)),
        td(cls := "px-4 py-2 text-slate-500", Formats.date(order.deliveryDate)),
        td(cls := "px-4 py-2 text-slate-500", s"${order.manufacturings.size} lav. · $taskCount task"),
        td(
          cls := "px-4 py-2",
          div(
            cls := "flex flex-wrap justify-end gap-2",
            orderActionButtons(order),
          ),
        ),
      )
    end renderRow

    // Status in lifecycle order, priority urgent first: sorting by them groups the rows the way the filter chips list them.
    val statusRank = statusFilters.map(_._1).filter(_.nonEmpty).zipWithIndex.toMap
    val sort = TableSort[OrderResponse](
      customersMap.map { customers =>
        Map(
          "number" -> Column.text(_.number),
          "customer" -> Column.optionalText(o => customers.get(o.customerId)),
          "status" -> Column.by(o => statusRank.getOrElse(o.status, statusRank.size)),
          "priority" -> Column.by(o => if o.priority == "urgent" then 0 else 1),
          "delivery" -> Column.text(_.deliveryDate),
          "manufacturings" -> Column.by(_.manufacturings.size),
        )
      },
      pager,
    )

    val filterBar =
      div(
        cls := "mb-4 flex flex-wrap items-center gap-2",
        statusFilters.map { case (value, labelText) =>
          button(
            tpe := "button",
            cls <-- statusFilter.signal.map(current => chipCls(current == value)),
            labelText,
            onClick --> { _ => statusFilter.set(value); pager.reset() },
          )
        },
        div(cls := "w-full sm:ml-auto sm:w-80", searchBox(query, "Cerca per numero, cliente, lavorazione…", pager)),
        div(
          cls := "w-full md:hidden",
          sort.picker(
            List(
              "number" -> "Ordine",
              "customer" -> "Cliente",
              "status" -> "Stato",
              "priority" -> "Priorità",
              "delivery" -> "Consegna",
              "manufacturings" -> "Lavorazioni",
            ),
          ),
        ),
      )

    val searchFields: Signal[OrderResponse => List[String]] =
      customersMap.map(customers => order => ListSearch.order(order, customers.get(order.customerId)))

    def ordersTable(orders: List[OrderResponse]): HtmlElement =
      val filtered = sort.sorted(orders).combineWith(statusFilter.signal, query.signal, searchFields).map { (sorted, filter, typed, fieldsOf) =>
        sorted.filter(o => (filter.isEmpty || o.status == filter) && SearchTerms.matchesAny(typed, fieldsOf(o)))
      }
      pager.view(filtered) { visible =>
        if visible.isEmpty then div(cls := "p-4", emptyState("Nessun ordine corrisponde ai filtri."))
        else
          div(
            div(cls := "divide-y divide-slate-100 md:hidden", visible.map(renderOrderCard)),
            div(
              cls := "hidden overflow-x-auto md:block",
              table(
                cls := "w-full text-sm",
                thead(
                  cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                  tr(
                    sort.header("number", "Ordine", "px-4 py-2"),
                    sort.header("customer", "Cliente", "px-4 py-2"),
                    sort.header("status", "Stato", "px-4 py-2"),
                    sort.header("priority", "Priorità", "px-4 py-2"),
                    sort.header("delivery", "Consegna", "px-4 py-2"),
                    sort.header("manufacturings", "Lavorazioni", "px-4 py-2"),
                    th(cls := "px-4 py-2"),
                  ),
                ),
                tbody(visible.map(renderRow)),
              ),
            ),
          )
        end if
      }
    end ordersTable

    // ---- Page layout -----------------------------------------------------------------------------
    div(
      manufacturingCatalogData --> {
        case Some(Right(list)) => manufacturingCatalogs.set(list)
        case _ => ()
      },
      div(
        view.listMods,
        div(
          cls := "mb-4 flex items-center justify-between",
          sectionTitle("Ordini"),
          roleGated(Role.Operator)(
            button(
              tpe := "button",
              cls := btnPrimary,
              "+ Nuovo ordine",
              onClick --> (_ => view.open(OrderTarget.Create)),
            ),
          ),
        ),
        child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mb-4", errorBanner(e)))),
        filterBar,
        card(
          cls := "overflow-hidden",
          renderResult(ordersData) { orders =>
            if orders.isEmpty then div(cls := "p-4", emptyState("Nessun ordine. Crea il primo ordine."))
            else ordersTable(orders)
          },
        ),
      ),
      view.page(target => OrderFormView(formDeps, target, () => view.close())),
      ackDialog,
    )
  end apply
end OrdersPage
