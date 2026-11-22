package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.MaterialDrafts.{ Context, Row }
import io.gitbub.nicolasfara.rstmanager.ui.OrderDrafts.StockWarning
import org.scalajs.dom

/**
 * The materials ("Materiali") of an order manufacturing: the read-only lines with their frozen prices and cost (detail page), the inline editor of a
 * saved manufacturing on the edit page (its lines are part of the page draft, saved with `PUT …/materials`), the inline editor of a new manufacturing
 * draft, and the price-less editor of the per-unit materials of a catalog manufacturing. The order editors show the non-blocking stock warnings of
 * [[OrderDrafts.stockWarnings]].
 *
 * Every rule (parsing, validation, price freezing, costs) lives in the pure [[MaterialDrafts]]; this object only binds it to Laminar. The materials
 * offered are the active warehouse materials, loaded on demand and refreshed on [[AppBus.warehouseTicks]].
 */
object MaterialsEditor:

  /** The active warehouse materials, loaded when the returned signal is first observed. */
  def activeMaterials(): Signal[Option[ApiClient.Result[List[MaterialResponse]]]] =
    loadable(AppBus.warehouseTicks)(() => ApiClient.listMaterials(false))

  /** The context of a new manufacturing: the loaded materials (none while loading or after a failure). */
  private def contextOf(materials: Signal[Option[ApiClient.Result[List[MaterialResponse]]]]): Signal[Context] =
    materials.map {
      case Some(Right(list)) => Context.of(list, Nil)
      case _ => Context.empty
    }

  /** A loading hint or the error of the material list; nothing once loaded. */
  def materialsStatus(materials: Signal[Option[ApiClient.Result[List[MaterialResponse]]]]): Modifier[HtmlElement] =
    child.maybe <-- materials.map {
      case None => Some(hint("Caricamento materiali…"))
      case Some(Left(err)) => Some(div(cls := "mb-2", errorBanner(err)))
      case Some(Right(_)) => None
    }

  private val noWarnings: Signal[Map[UUID, StockWarning]] = Signal.fromValue(Map.empty)

  // ---- Rows editor -------------------------------------------------------------------------------

  /**
   * The editable lines bound to `rows`, checked against `context`: per line the material (searchable), the quantity, an optional price override (the
   * placeholder shows the price that applies otherwise), and the line cost; "+ Materiale" adds a line, the footer shows the total and the validation
   * messages. Blank lines are ignored.
   */
  def rowsEditor(rows: Var[List[Row]], context: Signal[Context], nextKey: () => Int): HtmlElement =
    rowsEditor(rows.signal, rows.update, context, nextKey, None, true, noWarnings)

  /**
   * The per-unit materials of a catalog manufacturing bound to `rows`: like [[rowsEditor]] without price and cost (an order freezes the customer
   * price when the manufacturing is added), with a warning on a line naming an archived material.
   */
  def quantitiesEditor(rows: Var[List[Row]], context: Signal[Context], nextKey: () => Int): HtmlElement =
    rowsEditor(rows.signal, rows.update, context, nextKey, None, false, noWarnings)

  /** The amber stock warning of a line whose material the order needs more of than is available. */
  private def stockWarningNote(warning: StockWarning): HtmlElement =
    div(
      cls := "rounded-md border border-amber-200 bg-amber-50 px-2 py-1 text-xs text-amber-800 sm:col-span-full",
      role := "status",
      OrderDrafts.stockWarningText(warning),
    )

  /**
   * Variant of [[rowsEditor]] over `rows` changed through `update` (lines held in a larger draft), focusing the material of the row with key
   * `focusKey` once it is mounted (a line just added from outside); without `priced` the price and cost columns are left out; `warnings` are the
   * stock warnings by material shown under the lines.
   */
  private def rowsEditor(
      rows: Signal[List[Row]],
      update: (List[Row] => List[Row]) => Unit,
      context: Signal[Context],
      nextKey: () => Int,
      focusKey: Option[Int],
      priced: Boolean,
      warnings: Signal[Map[UUID, StockWarning]],
  ): HtmlElement =
    val options = context.map(_.options).distinct
    // The row whose material gets the focus when mounted: the one "+ Materiale" just added.
    var pendingFocus = focusKey

    def addRow(): Unit =
      val row = Row.empty(nextKey())
      pendingFocus = Some(row.key)
      update(_ :+ row)

    def focusMaterial(key: Int, rowElement: dom.Element): Unit =
      if pendingFocus.contains(key) then
        pendingFocus = None
        rowElement.querySelector("input") match
          case material: dom.HTMLElement => material.focus()
          case _ => ()

    def updateRow(key: Int)(change: Row => Row): Unit = update(_.map(row => if row.key == key then change(row) else row))

    def renderRow(key: Int, rowSignal: Signal[Row]): HtmlElement =
      val withContext = rowSignal.combineWith(context)
      val preview = withContext.map(MaterialDrafts.pricePreview)
      val selected = withContext.map(MaterialDrafts.materialOf)
      val rowWarning =
        rowSignal.map(_.materialId.trim.nn).combineWith(warnings).map((id, byMaterial) => byMaterial.find(_._1.toString == id).map(_._2))
      div(
        cls := "grid grid-cols-1 gap-2 rounded-md border border-slate-200 bg-white p-2 sm:items-start",
        // Without price the editor sits in the narrow catalog form: the material gets a line of its own, quantity and "Rimuovi" go under it.
        cls := (if priced then "sm:grid-cols-[minmax(0,1fr)_7.5rem_8.5rem_6.5rem_auto]" else "sm:grid-cols-[minmax(0,1fr)_auto]"),
        onMountCallback(ctx => focusMaterial(key, ctx.thisNode.ref)),
        field(
          "Materiale",
          searchableSelect(rowSignal.map(_.materialId), Observer[String](v => updateRow(key)(_.withMaterial(v))), options, "Cerca materiale…", 20),
        ).amend(Option.when(!priced)(cls := "sm:col-span-full")),
        field(
          "Quantità",
          div(
            textInput(rowSignal.map(_.quantity), Observer[String](v => updateRow(key)(_.withQuantity(v))), "2,5").amend(inputMode := "decimal"),
            div(
              cls := "mt-1 text-xs text-slate-400",
              child.text <-- withContext.map((row, ctx) =>
                MaterialDrafts
                  .materialOf(row, ctx)
                  .fold("")(p => WarehouseFormats.unitLabel(p.unit) + (if MaterialDrafts.isPieces(p.unit) then " (interi)" else "")),
              ),
            ),
          ),
        ),
        Option.when(priced)(
          field(
            "Prezzo unit. (€)",
            div(
              textInput(rowSignal.map(_.unitPrice), Observer[String](v => updateRow(key)(_.copy(unitPrice = v))), "")
                .amend(
                  inputMode := "decimal",
                  placeholder <-- preview.map(
                    _.filter(_.source != MaterialDrafts.PriceSource.Manual).fold("")(p => WarehouseFormats.decimalInputText(p.unitPrice, 2, 4)),
                  ),
                ),
              div(cls := "mt-1 text-xs text-slate-400", child.text <-- preview.map(_.fold("")(MaterialDrafts.priceNote))),
            ),
          ),
        ),
        Option.when(priced)(
          div(
            span(cls := "mb-1 block text-xs font-medium text-slate-500", "Costo"),
            div(
              cls := "py-1.5 text-sm font-medium tabular-nums text-slate-800",
              child.text <-- withContext.map((row, ctx) => MaterialDrafts.lineCost(row, ctx).fold("—")(WarehouseFormats.formatEuro)),
            ),
          ),
        ),
        button(
          tpe := "button",
          cls := s"$btnDanger justify-center sm:mt-5",
          "Rimuovi",
          onClick --> (_ => update(_.filterNot(_.key == key))),
        ),
        child.maybe <-- rowWarning.map(_.map(stockWarningNote)),
        child.maybe <-- selected.map(_.filter(p => !priced && p.archived).map { material =>
          div(
            cls := "rounded-md border border-amber-200 bg-amber-50 px-2 py-1 text-xs text-amber-800 sm:col-span-full",
            role := "status",
            s"${material.internalCode} è archiviato: resta nella lavorazione a catalogo, ma non viene proposto nei nuovi ordini.",
          )
        }),
      )
    end renderRow

    val errors = rows
      .combineWith(context)
      .map((list, ctx) => if priced then MaterialDrafts.errors(list, ctx) else MaterialDrafts.quantityErrors(list, ctx))
    div(
      cls := "space-y-2",
      div(cls := "space-y-2", children <-- rows.split(_.key)((key, _, rowSignal) => renderRow(key, rowSignal))),
      div(
        cls := "flex flex-wrap items-center justify-between gap-2",
        button(tpe := "button", cls := btnSmall, "+ Materiale", onClick --> (_ => addRow())),
        Option.when(priced)(
          div(
            cls := "text-sm text-slate-600",
            "Costo materiali: ",
            span(
              cls := "font-semibold tabular-nums text-slate-800",
              child.text <-- rows.combineWith(context).map((list, ctx) => WarehouseFormats.formatEuro(MaterialDrafts.totalCost(list, ctx))),
            ),
          ),
        ),
      ),
      errorList(errors),
    )
  end rowsEditor

  // ---- Create-order draft ------------------------------------------------------------------------

  /**
   * The materials of a manufacturing draft in the create-order form. The warehouse materials are loaded only once a line is added; the loaded context
   * is also written to `contextSink`, which the form validates the lines against on submit.
   */
  def draftEditor(rows: Var[List[Row]], nextKey: () => Int, contextSink: Observer[Context]): HtmlElement =
    draftEditor(rows, nextKey, contextSink, emptyMod, noWarnings)

  /**
   * Variant of [[draftEditor]] whose block carries DOM id `anchorId`, the target [[FormFocus.focus]] moves to for a material issue, and showing the
   * stock `warnings` of the whole draft on the lines of their materials.
   */
  def draftEditor(
      rows: Var[List[Row]],
      nextKey: () => Int,
      contextSink: Observer[Context],
      anchorId: String,
      warnings: Signal[Map[UUID, StockWarning]],
  ): HtmlElement =
    draftEditor(rows, nextKey, contextSink, idAttr := anchorId, warnings)

  private def draftEditor(
      rows: Var[List[Row]],
      nextKey: () => Int,
      contextSink: Observer[Context],
      anchor: Modifier[HtmlElement],
      warnings: Signal[Map[UUID, StockWarning]],
  ): HtmlElement =
    // The first line added from the empty state, whose material gets the focus once the editor is shown.
    var firstAdded = Option.empty[Int]
    div(
      cls := "space-y-2",
      anchor,
      subheading("Materiali"),
      child <-- rows.signal.map(_.isEmpty).distinct.map { empty =>
        if empty then
          div(
            cls := "flex flex-wrap items-center gap-2",
            button(
              tpe := "button",
              cls := btnSmall,
              "+ Materiale",
              onClick --> { _ =>
                val row = Row.empty(nextKey())
                firstAdded = Some(row.key)
                rows.set(List(row))
              },
            ),
            span(cls := "text-xs text-slate-400", "Nessun materiale (si possono aggiungere anche dopo la creazione)"),
          )
        else
          val materials = activeMaterials()
          val context = contextOf(materials)
          val focusKey = firstAdded
          firstAdded = None
          div(
            context --> contextSink,
            materialsStatus(materials),
            rowsEditor(rows.signal, rows.update, context, nextKey, focusKey, true, warnings),
            hint("Senza un prezzo inserito si congela il prezzo di listino del materiale alla creazione dell'ordine."),
          )
      },
    )
  end draftEditor

  // ---- Existing manufacturing --------------------------------------------------------------------

  private def quantityText(line: MaterialLineResponse): String =
    line.unit.fold(WarehouseFormats.formatDecimal(line.quantity, 0, MaterialDrafts.quantityScale))(WarehouseFormats.formatQty(line.quantity, _))

  /** One read-only material line: code – description (with an "Archiviato" / "Non in magazzino" badge), quantity, frozen unit price, line cost. */
  def renderLine(line: MaterialLineResponse): HtmlElement =
    div(
      cls := "flex flex-col gap-0.5 py-1.5 text-sm sm:flex-row sm:items-baseline sm:justify-between sm:gap-3",
      div(
        cls := "min-w-0",
        span(cls := "font-medium text-slate-800", line.internalCode.getOrElse(Formats.shortId(line.materialId))),
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
   * The read-only materials of a saved manufacturing (detail page): its lines (code – description, quantity, frozen unit price, line cost) and the
   * materials cost.
   */
  def linesView(manufacturing: ManufacturingResponse): HtmlElement =
    div(
      cls := "space-y-1",
      div(
        cls := "flex flex-wrap items-center justify-between gap-2",
        subheading("Materiali"),
        div(
          cls := "flex items-center gap-2 text-xs text-slate-500",
          "Costo materiali: ",
          span(cls := "text-sm font-semibold tabular-nums text-slate-800", WarehouseFormats.formatEuro(manufacturing.materialsCost)),
        ),
      ),
      if manufacturing.materials.isEmpty then div(cls := "py-1.5 text-xs text-slate-400", "Nessun materiale.")
      else div(cls := "divide-y divide-slate-100", manufacturing.materials.map(renderLine)),
    )

  /**
   * The inline materials editor of a saved manufacturing on the edit page: the lines are part of the page draft (`rows`, changed through `update`),
   * checked against the active warehouse materials plus the current server `lines` (their frozen prices and the materials no longer active). The
   * loaded context is also written to `contextSink`, which the page validates and saves the lines with. A `completed` manufacturing shows that saving
   * updates the stock unload. `warnings` are the stock warnings of the order shown on the lines of their materials.
   */
  def savedEditor(
      lines: Signal[List[MaterialLineResponse]],
      rows: Signal[List[Row]],
      update: (List[Row] => List[Row]) => Unit,
      nextKey: () => Int,
      contextSink: Observer[Context],
      anchorId: String,
      completed: Signal[Boolean],
      warnings: Signal[Map[UUID, StockWarning]],
  ): HtmlElement =
    val materials = activeMaterials()
    val context = materials.combineWith(lines).map {
      case (Some(Right(list)), current) => Context.of(list, current)
      case (_, current) => Context.of(Nil, current)
    }
    div(
      idAttr := anchorId,
      cls := "space-y-2",
      context --> contextSink,
      subheading("Materiali"),
      child.maybe <-- completed.distinct.map(done =>
        Option.when(done)(
          warningNote("La lavorazione è completata: salvando, lo scarico dei materiali in magazzino viene aggiornato automaticamente."),
        ),
      ),
      materialsStatus(materials),
      rowsEditor(rows, update, context, nextKey, None, true, warnings),
      hint("Senza un prezzo inserito, una riga già presente mantiene il prezzo congelato; un nuovo materiale congela il prezzo di listino attuale."),
    )
  end savedEditor
end MaterialsEditor
