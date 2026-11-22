package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.{ AuthService, Role }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** What the orders page shows in place of its list: the create page, the edit page of an editable order, or the read-only detail of any other. */
enum OrderTarget derives CanEqual:
  case Create
  case Edit(order: OrderResponse)
  case Detail(order: OrderResponse)

/**
 * The order form page, one continuous full page for Create, Edit and Detail ([[OrderCreateView]] and [[OrderEditView]]), plus the pieces both share
 * so the three modes look the same: the header fields, the summary sidebar, the mobile bar and the recap.
 */
object OrderFormView:

  /** The shared data of the orders page the form pages need. `knownOrderIds` is `None` until the order list loaded successfully. */
  final case class Deps(
      customerOptions: Signal[List[(String, String)]],
      tasks: Signal[List[TaskResponse]],
      catalogs: Signal[List[ManufacturingCatalogResponse]],
      employeeOptions: Signal[List[(String, String)]],
      existingOrderNumbers: Signal[List[String]],
      knownOrderIds: Signal[Option[Set[UUID]]],
  )

  /** `Edit` for an in-progress or suspended order and an Operator, `Detail` otherwise (the server enforces both). */
  def targetFor(order: OrderResponse): OrderTarget =
    if OrderStatus.isEditable(order.status) && AuthService.currentHasRole(Role.Operator) then OrderTarget.Edit(order) else OrderTarget.Detail(order)

  /** The page of `target`; `onExit` goes back to the orders list. */
  def apply(deps: Deps, target: OrderTarget, onExit: () => Unit): HtmlElement = target match
    case OrderTarget.Create => OrderCreateView(deps, onExit)
    case OrderTarget.Edit(order) => OrderEditView(deps, order, true, onExit)
    case OrderTarget.Detail(order) => OrderEditView(deps, order, false, onExit)

  // ---- Shared vocabulary -------------------------------------------------------------------------

  val backLabel = "← Torna agli ordini"

  val priorityOptions: List[(String, String)] = List("normal" -> "Normale", "urgent" -> "Urgente")

  def priorityLabel(priority: String): String = if priority == "urgent" then "Urgente" else "Normale"

  def priorityBadge(priority: String): HtmlElement = priority match
    case "urgent" => badge("Urgente", "bg-rose-50 text-rose-700 border-rose-200")
    case _ => badge("Normale", "bg-slate-50 text-slate-600 border-slate-200")

  def orDash(text: String): String = if text.trim.nn.isEmpty then "—" else text

  /**
   * The "×N" badge of a manufacturing done `times` > 1 times (none for one). It stays outside the truncated label so a phone still shows it, and
   * reads "quantità N" to a screen reader.
   */
  def quantityBadge(times: Int): Option[HtmlElement] =
    Option.when(times > 1)(
      span(
        cls := "shrink-0 rounded-full bg-slate-100 px-1.5 py-0.5 text-xs font-semibold tabular-nums text-slate-600",
        title := s"Quantità: la lavorazione si esegue $times volte (durate e materiali sono già i totali)",
        span(aria.hidden := true, s"×$times"),
        span(cls := "sr-only", s"quantità $times"),
      ),
    )

  /**
   * The stock warnings of the whole draft (non-blocking): one amber line per material the draft needs more of than is available, then `footnote`
   * (what the user can still do). None without warnings.
   */
  def stockWarningsBox(warnings: List[OrderDrafts.StockWarning], footnote: String): Option[HtmlElement] =
    Option.when(warnings.nonEmpty)(
      div(
        cls := "space-y-1 rounded-md border border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-800",
        role := "status",
        div(cls := "font-semibold", s"Materiali da ordinare (${warnings.size})"),
        ul(cls := "space-y-0.5", warnings.map(w => li(span(cls := "font-medium", w.internalCode), s": ${OrderDrafts.stockWarningText(w)}"))),
        div(cls := "text-amber-700", footnote),
      ),
    )

  /** The label of `selected` among `options` (customers, employees); "—" when unknown. */
  def optionLabel(options: Signal[List[(String, String)]], selected: Signal[String]): Signal[String] =
    selected.combineWith(options).map((id, list) => list.collectFirst { case (value, labelText) if value == id => labelText }.getOrElse("—"))

  def customerName(deps: Deps, customerId: Signal[String]): Signal[String] = optionLabel(deps.customerOptions, customerId)

  // ---- Header fields -----------------------------------------------------------------------------

  /** The grid of the "Testata" section: one column below `sm`, two from `sm`. */
  val headerGridCls = "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2"

  /**
   * The "Deadline fine lavorazione" field: the date `input` (receiving the field's `bind`), "↺ Usa proposta" while `showReset`, and an amber note
   * while `late` (the deadline falls after the customer delivery).
   */
  def deadlineField(
      spec: FieldSpec,
      input: Modifier[HtmlElement] => HtmlElement,
      showReset: Signal[Boolean],
      onReset: () => Unit,
      late: Signal[Boolean],
  ): HtmlElement =
    div(
      field(spec)(input),
      child.maybe <-- showReset.distinct.map { show =>
        Option.when(show)(button(tpe := "button", cls := s"$btnLink mt-1 text-xs", "↺ Usa proposta", onClick --> (_ => onReset())))
      },
      child.maybe <-- late.distinct.map { isLate =>
        Option.when(isLate)(
          div(
            cls := "mt-1 rounded-md bg-amber-50 px-2 py-1 text-xs text-amber-800",
            "La deadline è successiva alla consegna cliente: verifica le date.",
          ),
        )
      },
    )

  /** "Priorità" as a segmented choice; the group carries DOM id `id`. */
  def priorityField(value: Signal[String], writer: Observer[String], id: String): HtmlElement =
    OrderMfgDraftCard.labelled("Priorità", segmented(value, writer, priorityOptions, List(aria.label := "Priorità", idAttr := id)))

  /** "Descrizione ordine" as a textarea over both columns. */
  def descriptionField(value: Signal[String], writer: Observer[String], id: String): HtmlElement =
    div(
      cls := "sm:col-span-2",
      field(FieldSpec("Descrizione ordine", id))(bind => textAreaInput(value, writer, "Opzionale").amend(bind)),
    )

  // ---- Summary -----------------------------------------------------------------------------------

  /** What the "Riepilogo" shows, live. */
  final case class Summary(
      number: Signal[String],
      customer: Signal[String],
      delivery: Signal[String],
      deadline: Signal[String],
      priority: Signal[String],
      manufacturings: Signal[Int],
      minutes: Signal[Int],
      materialsCost: Signal[String],
  )

  /** The sticky sidebar: "Riepilogo" with the summary rows, then `extra` (checklists, actions). */
  def sidebar(summary: Summary)(extra: Modifier[HtmlElement]*): HtmlElement =
    card(
      cls := "space-y-4 p-4",
      h2(cls := "text-sm font-semibold text-slate-800", "Riepilogo"),
      div(
        cls := "space-y-1.5",
        summaryRow("Numero", summary.number),
        summaryRow("Cliente", summary.customer),
        summaryRow("Consegna", summary.delivery),
        summaryRow("Deadline", summary.deadline),
        summaryRow("Priorità", summary.priority),
      ),
      div(
        cls := "space-y-1.5 border-t border-slate-100 pt-3",
        summaryRow("Lavorazioni", summary.manufacturings.map(_.toString)),
        summaryRow("Ore totali", summary.minutes.map(OrderCreateModel.hoursText)),
        summaryRow("Costo materiali", summary.materialsCost.map(WarehouseFormats.formatEuro)),
      ),
      extra,
    )

  /** "n lav. · h:mm h · € x", the totals of the mobile bar. */
  def mobileTotals(summary: Summary): Signal[String] =
    summary.manufacturings.combineWith(summary.minutes, summary.materialsCost).map { (count, minutes, cost) =>
      s"$count lav. · ${OrderCreateModel.hoursText(minutes)} · ${WarehouseFormats.formatEuro(cost)}"
    }

  /** The "⚠ n" button of the mobile bar (nothing while `count` is 0), going to the first issue. */
  def issueBadge(count: Signal[Int], onSelect: () => Unit): Modifier[HtmlElement] =
    child.maybe <-- count.map { n =>
      Option.when(n > 0)(
        button(
          tpe := "button",
          cls := "min-h-10 shrink-0 rounded-full bg-amber-50 px-2.5 py-1 text-xs font-medium text-amber-700 hover:bg-amber-100",
          aria.label := s"$n elementi da completare: vai al primo",
          s"⚠ $n",
          onClick --> (_ => onSelect()),
        ),
      )
    }

  /** The fixed bottom bar below `lg`: `leading` (live totals or changes), `extra`, and the primary button, disabled while `disabledSignal`. */
  def mobileBar(
      leading: Modifier[HtmlElement],
      extra: Modifier[HtmlElement],
      primaryLabel: String,
      busyLabel: String,
      busy: Signal[Boolean],
      disabledSignal: Signal[Boolean],
      onPrimary: () => Unit,
  ): HtmlElement =
    div(
      cls := "flex items-center gap-3",
      div(cls := "min-w-0 flex-1 truncate text-sm tabular-nums text-slate-700", leading),
      extra,
      button(
        tpe := "button",
        cls := s"$btnPrimary min-h-10 shrink-0",
        disabled <-- disabledSignal,
        aria.busy <-- busy,
        child.text <-- busy.map(isBusy => if isBusy then busyLabel else primaryLabel),
        onClick --> (_ => onPrimary()),
      ),
    )

  // ---- Recap -------------------------------------------------------------------------------------

  /**
   * One line of the recap: the manufacturing label (empty = not chosen yet), its totals, its completion day (`dd/MM`, empty = none) and its
   * multiplier ×N (a badge from 2).
   */
  final case class RecapItem(label: String, totals: String, day: String, quantity: Int = 1)

  def recapList(items: List[RecapItem]): HtmlElement =
    if items.isEmpty then p(cls := "text-sm text-slate-400", "Nessuna lavorazione aggiunta.")
    else
      ul(
        cls := "divide-y divide-slate-100 rounded-md border border-slate-200",
        items.zipWithIndex.map { (item, index) =>
          li(
            cls := "flex flex-col gap-0.5 px-3 py-2 text-sm sm:flex-row sm:items-baseline sm:justify-between sm:gap-3",
            span(
              cls := "flex min-w-0 items-baseline gap-1.5",
              span(
                cls := "min-w-0 truncate",
                span(cls := "mr-1.5 tabular-nums text-slate-400", s"#${index + 1}"),
                span(
                  cls := (if item.label.isEmpty then "italic text-slate-400" else "text-slate-800"),
                  if item.label.isEmpty then "Nuova lavorazione" else item.label,
                ),
              ),
              Option.when(item.label.nonEmpty)(item.quantity).flatMap(quantityBadge),
            ),
            span(cls := "shrink-0 text-xs tabular-nums text-slate-500", item.totals + (if item.day.isEmpty then "" else s" · entro ${item.day}")),
          )
        },
      )

  /** The "Totale ordine" line under the recap. */
  def recapTotal(totals: Signal[String]): HtmlElement =
    div(
      cls := "mt-2 flex justify-between gap-3 px-3 text-sm",
      span(cls := "text-slate-500", "Totale ordine"),
      span(cls := "font-semibold tabular-nums text-slate-800", child.text <-- totals),
    )
end OrderFormView
