package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.MaterialDrafts.{ Context, Row }

/**
 * The materials ("Materiali") of an order manufacturing: the read-only lines with their frozen prices and cost, the Operator-gated editor modal of an
 * existing manufacturing (`PUT …/materials`), and the inline editor of a manufacturing draft in the create-order form.
 *
 * Every rule (parsing, validation, price freezing, costs) lives in the pure [[MaterialDrafts]]; this object only binds it to Laminar. The products
 * offered are the active warehouse products, loaded on demand and refreshed on [[AppBus.warehouseTicks]].
 */
object MaterialsEditor:

  private def hint(text: String): HtmlElement = div(cls := "mt-1 text-xs text-slate-400", text)

  /** The active warehouse products, loaded when the returned signal is first observed. */
  private def activeProducts(): Signal[Option[ApiClient.Result[List[ProductResponse]]]] =
    loadable(AppBus.warehouseTicks)(() => ApiClient.listProducts(false))

  /** The context of a manufacturing with the `current` lines: the loaded products (none while loading or after a failure) plus the current ones. */
  private def contextOf(products: Signal[Option[ApiClient.Result[List[ProductResponse]]]], current: List[MaterialResponse]): Signal[Context] =
    products.map {
      case Some(Right(list)) => Context.of(list, current)
      case _ => Context.of(Nil, current)
    }

  /** A loading hint or the error of the product list; nothing once loaded. */
  private def productsStatus(products: Signal[Option[ApiClient.Result[List[ProductResponse]]]]): Modifier[HtmlElement] =
    child.maybe <-- products.map {
      case None => Some(hint("Caricamento prodotti…"))
      case Some(Left(err)) => Some(div(cls := "mb-2", errorBanner(err)))
      case Some(Right(_)) => None
    }

  // ---- Rows editor -------------------------------------------------------------------------------

  /**
   * The editable lines bound to `rows`, checked against `context`: per line the product (searchable), the quantity, an optional price override (the
   * placeholder shows the price that applies otherwise), and the line cost; "+ Materiale" adds a line, the footer shows the total and the validation
   * messages. Blank lines are ignored.
   */
  def rowsEditor(rows: Var[List[Row]], context: Signal[Context], nextKey: () => Int): HtmlElement =
    val options = context.map(_.options).distinct

    def update(key: Int)(change: Row => Row): Unit = rows.update(_.map(row => if row.key == key then change(row) else row))

    def renderRow(key: Int, rowSignal: Signal[Row]): HtmlElement =
      val withContext = rowSignal.combineWith(context)
      val preview = withContext.map(MaterialDrafts.pricePreview)
      div(
        cls := "grid grid-cols-1 gap-2 rounded-md border border-slate-200 bg-white p-2 sm:grid-cols-[minmax(0,1fr)_7.5rem_8.5rem_6.5rem_auto] sm:items-start",
        field(
          "Prodotto",
          searchableSelect(rowSignal.map(_.productId), Observer[String](v => update(key)(_.copy(productId = v))), options, "Cerca prodotto…", 20),
        ),
        field(
          "Quantità",
          div(
            textInput(rowSignal.map(_.quantity), Observer[String](v => update(key)(_.copy(quantity = v))), "2,5").amend(inputMode := "decimal"),
            div(
              cls := "mt-1 text-xs text-slate-400",
              child.text <-- withContext.map((row, ctx) =>
                MaterialDrafts
                  .productOf(row, ctx)
                  .fold("")(p => WarehouseFormats.unitLabel(p.unit) + (if MaterialDrafts.isPieces(p.unit) then " (interi)" else "")),
              ),
            ),
          ),
        ),
        field(
          "Prezzo unit. (€)",
          div(
            textInput(rowSignal.map(_.unitPrice), Observer[String](v => update(key)(_.copy(unitPrice = v))), "")
              .amend(
                inputMode := "decimal",
                placeholder <-- preview.map(
                  _.filter(_.source != MaterialDrafts.PriceSource.Manual).fold("")(p => WarehouseFormats.decimalInputText(p.unitPrice, 2, 4)),
                ),
              ),
            div(cls := "mt-1 text-xs text-slate-400", child.text <-- preview.map(_.fold("")(MaterialDrafts.priceNote))),
          ),
        ),
        div(
          span(cls := "mb-1 block text-xs font-medium text-slate-500", "Costo"),
          div(
            cls := "py-1.5 text-sm font-medium tabular-nums text-slate-800",
            child.text <-- withContext.map((row, ctx) => MaterialDrafts.lineCost(row, ctx).fold("—")(WarehouseFormats.formatEuro)),
          ),
        ),
        button(
          tpe := "button",
          cls := s"$btnDanger justify-center sm:mt-5",
          "Rimuovi",
          onClick --> (_ => rows.update(_.filterNot(_.key == key))),
        ),
      )
    end renderRow

    val errors = rows.signal.combineWith(context).map(MaterialDrafts.errors)
    div(
      cls := "space-y-2",
      div(cls := "space-y-2", children <-- rows.signal.split(_.key)((key, _, rowSignal) => renderRow(key, rowSignal))),
      div(
        cls := "flex flex-wrap items-center justify-between gap-2",
        button(tpe := "button", cls := btnSmall, "+ Materiale", onClick --> (_ => rows.update(_ :+ Row.empty(nextKey())))),
        div(
          cls := "text-sm text-slate-600",
          "Costo materiali: ",
          span(
            cls := "font-semibold tabular-nums text-slate-800",
            child.text <-- rows.signal.combineWith(context).map((list, ctx) => WarehouseFormats.formatEuro(MaterialDrafts.totalCost(list, ctx))),
          ),
        ),
      ),
      child.maybe <-- errors.map(list => Option.when(list.nonEmpty)(ul(cls := "list-disc pl-5 text-xs text-rose-600", list.map(e => li(e))))),
    )
  end rowsEditor

  // ---- Create-order draft ------------------------------------------------------------------------

  /**
   * The materials of a manufacturing draft in the create-order form. The products are loaded only once a line is added; the loaded context is also
   * written to `contextSink`, which the form validates the lines against on submit.
   */
  def draftEditor(rows: Var[List[Row]], nextKey: () => Int, contextSink: Observer[Context]): HtmlElement =
    div(
      cls := "mt-2 space-y-2",
      div(cls := "text-xs font-medium text-slate-500", "Materiali"),
      child <-- rows.signal.map(_.isEmpty).distinct.map { empty =>
        if empty then
          div(
            cls := "flex flex-wrap items-center gap-2",
            button(tpe := "button", cls := btnSmall, "+ Materiale", onClick --> (_ => rows.set(List(Row.empty(nextKey()))))),
            span(cls := "text-xs text-slate-400", "Nessun materiale (si possono aggiungere anche dopo la creazione)"),
          )
        else
          val products = activeProducts()
          val context = contextOf(products, Nil)
          div(
            context --> contextSink,
            productsStatus(products),
            rowsEditor(rows, context, nextKey),
            hint("Senza un prezzo inserito si congela il prezzo di listino del prodotto alla creazione dell'ordine."),
          )
      },
    )

  // ---- Existing manufacturing --------------------------------------------------------------------

  private def quantityText(line: MaterialResponse): String =
    line.unit.fold(WarehouseFormats.formatDecimal(line.quantity, 0, MaterialDrafts.quantityScale))(WarehouseFormats.formatQty(line.quantity, _))

  private def renderLine(line: MaterialResponse): HtmlElement =
    div(
      cls := "flex flex-col gap-0.5 py-1.5 text-sm sm:flex-row sm:items-baseline sm:justify-between sm:gap-3",
      div(
        cls := "min-w-0",
        span(cls := "font-medium text-slate-800", line.internalCode.getOrElse(Formats.shortId(line.productId))),
        line.description.map(d => span(cls := "text-slate-600", s" – $d")),
        Option.when(line.archived || line.internalCode.isEmpty)(
          span(cls := "ml-2", badge(if line.archived then "Archiviato" else "Non in magazzino", "bg-slate-100 text-slate-500 border-slate-200")),
        ),
      ),
      div(
        cls := "flex shrink-0 items-baseline gap-3 tabular-nums",
        span(cls := "text-slate-600", quantityText(line)),
        span(cls := "text-xs text-slate-400", s"× ${WarehouseFormats.formatUnitPrice(line.unitPrice)}"),
        span(cls := "font-medium text-slate-800", WarehouseFormats.formatEuro(line.lineCost)),
      ),
    )

  /**
   * The "Materiali" section of a manufacturing of an existing order: its lines (code – description, quantity, frozen unit price, line cost) and the
   * materials cost, with the Operator-gated "Modifica materiali" opening the editor modal while the order is in progress or suspended (the server
   * enforces both). After a save it reports the order and warehouse mutations and hands the updated order to `onSaved`.
   */
  def section(order: OrderResponse, manufacturing: ManufacturingResponse, onSaved: OrderResponse => Unit): HtmlElement =
    val open = Var(false)
    val close: () => Unit = () => open.set(false)
    div(
      cls := "mt-3 rounded-md border border-slate-100 bg-slate-50/60 px-3 py-2",
      div(
        cls := "flex flex-wrap items-center justify-between gap-2",
        div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Materiali"),
        div(
          cls := "flex items-center gap-2 text-xs text-slate-500",
          "Costo materiali: ",
          span(cls := "text-sm font-semibold tabular-nums text-slate-800", WarehouseFormats.formatEuro(manufacturing.materialsCost)),
        ),
      ),
      if manufacturing.materials.isEmpty then div(cls := "py-1.5 text-xs text-slate-400", "Nessun materiale.")
      else div(cls := "divide-y divide-slate-100", manufacturing.materials.map(renderLine)),
      roleGated(Role.Operator)(
        if OrderStatus.isEditable(order.status) then
          div(
            cls := "mt-1 flex justify-end",
            button(tpe := "button", cls := btnSmall, "Modifica materiali", onClick --> (_ => open.set(true))),
          )
        else hint("I materiali si possono modificare solo per un ordine in corso o sospeso."),
      ),
      modal(open.signal, Signal.fromValue(s"Materiali — ${manufacturing.code}"), "max-w-4xl", close)(
        div(child.maybe <-- open.signal.distinct.map(isOpen => Option.when(isOpen)(editorDialog(order, manufacturing, close, onSaved)))),
      ),
    )
  end section

  /** The editor of an existing manufacturing, rebuilt on every opening from its current lines. */
  private def editorDialog(
      order: OrderResponse,
      manufacturing: ManufacturingResponse,
      close: () => Unit,
      onSaved: OrderResponse => Unit,
  ): HtmlElement =
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1
      keyCounter
    val rows = Var(MaterialDrafts.rowsOf(manufacturing.materials, () => nextKey()))
    val tracker = DirtyTracker(rows.now(), rows)
    val products = activeProducts()
    val context = contextOf(products, manufacturing.materials)
    val currentContext = Var(Context.of(Nil, manufacturing.materials))
    val error = Var(Option.empty[ApiError])
    val saving = Var(false)
    val canSave = rows.signal.combineWith(context, tracker.isDirty, saving.signal).map { (list, ctx, dirty, busy) =>
      dirty && !busy && MaterialDrafts.errors(list, ctx).isEmpty
    }

    def save(): Unit =
      MaterialDrafts.request(rows.now(), currentContext.now()) match
        case Right(lines) if !saving.now() =>
          saving.set(true)
          error.set(None)
          ApiClient.setManufacturingMaterials(order.id, manufacturing.id, lines).foreach { result =>
            saving.set(false)
            result match
              case Right(updated) =>
                AppBus.mutatedOrders()
                AppBus.mutatedWarehouse()
                close()
                onSaved(updated)
              case Left(err) => showError(error, "Materiali lavorazione")(err)
          }
        case _ => ()

    div(
      cls := "space-y-3",
      context --> currentContext.writer,
      div(
        cls := "rounded-md bg-slate-50 px-3 py-2 text-xs text-slate-600",
        span(cls := "font-medium text-slate-800", manufacturing.code),
        manufacturing.description.map(d => span(s" — $d")),
        div(cls := "mt-0.5", s"Ordine ${order.number} · ", OrderStatus.label(manufacturing.status)),
      ),
      Option.when(manufacturing.status == "completed")(
        div(
          cls := "rounded-md border border-amber-200 bg-amber-50 p-2 text-xs text-amber-800",
          "La lavorazione è completata: salvando, lo scarico dei materiali in magazzino viene aggiornato automaticamente.",
        ),
      ),
      productsStatus(products),
      rowsEditor(rows, context, () => nextKey()),
      hint("Senza un prezzo inserito, una riga già presente mantiene il prezzo congelato; un nuovo prodotto congela il prezzo di listino attuale."),
      child.maybe <-- error.signal.map(_.map(errorBanner)),
      div(
        cls := "flex justify-end gap-2 border-t border-slate-100 pt-3",
        button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => close())),
        button(
          tpe := "button",
          cls := btnPrimary,
          child.text <-- saving.signal.map(busy => if busy then "Salvataggio…" else "Salva materiali"),
          disabled <-- canSave.map(!_),
          onClick --> (_ => save()),
        ),
      ),
    )
  end editorDialog
end MaterialsEditor
