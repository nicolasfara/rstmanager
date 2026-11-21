package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{
  AppBus,
  FormFocus,
  FormTouch,
  ListSearch,
  Notifications,
  Pagination,
  Paging,
  TableSort,
  WarehouseFormats,
}
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.messages
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MaterialForm.{ Field, MpnField, MpnRow }

/**
 * Material registry of the warehouse: internal code, unit of measure, customer price, and the MPNs it is bought as (each tied to a supplier). The
 * material suppliers are derived from the MPNs. Removing a material archives it, so it stays available to the history.
 *
 * The table also shows today's stock and value of each material, with the stock actions of a row: purchase, unload, and adjustment (Operator) and the
 * movements list (Viewer), in the dialogs of [[StockDialogs]]. Admins can also load many materials at once from an Excel workbook, in the
 * [[MaterialImportDialog]]; the outcome of the last upload stays above the table until dismissed.
 */
object MaterialsTab:

  private val materialTypesListId = "warehouse-material-types"

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val form = Var(MaterialForm.empty)
    val showArchived = Var(false)
    val pendingArchive = Var(Option.empty[MaterialResponse])
    val stockDialog = Var(Option.empty[StockDialog])
    // Suppliers of the material being edited, so an MPN of a supplier deleted meanwhile still shows a name in its row.
    val editedSuppliers = Var(List.empty[MaterialSupplierDto])
    val importOpen = Var(false)
    val lastImport = Var(Option.empty[MaterialImportResponse])

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

    val supplierOptions: Signal[List[(String, String)]] =
      suppliers.combineWith(editedSuppliers.signal).map { (loaded, edited) =>
        val active = loaded match
          case Some(Right(list)) => list.map(s => s.id.toString -> s.name)
          case _ => Nil
        val activeIds = active.map(_._1).toSet
        active ++ edited.filterNot(s => activeIds.contains(s.id.toString)).map(s => s.id.toString -> s"${s.name} (eliminato)")
      }

    val materialTypes: Signal[List[String]] = materials.map {
      case Some(Right(list)) => MaterialForm.materialTypes(list)
      case _ => Nil
    }

    val touch = FormTouch()
    val submitting = Var(false)

    def resetForm(): Unit =
      form.set(MaterialForm.empty)
      editedSuppliers.set(Nil)
      formError.set(None)
      touch.reset()

    val formAnchor = FormAnchor()

    def edit(material: MaterialResponse): Unit =
      formAnchor.reveal()
      form.set(MaterialForm.fromResponse(material))
      editedSuppliers.set(material.suppliers)
      formError.set(None)
      touch.reset()

    def submit(): Unit =
      if !submitting.now() then
        val current = form.now()
        current.request match
          case None =>
            touch.attemptSubmit()
            current.issues.headOption.foreach(issue => FormFocus.focus(MaterialForm.fieldId(issue.field)))
          case Some(request) =>
            submitting.set(true)
            formError.set(None)
            val effect = current.editingId match
              case Some(id) => ApiClient.updateMaterial(id, request)
              case None => ApiClient.createMaterial(request)
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(_) =>
                  resetForm()
                  Notifications.success(s"Materiale ${request.internalCode} salvato")
                  AppBus.mutatedWarehouse()
                case Left(err) => showError(formError, "Salvataggio materiale")(err)
            }

    def archive(id: UUID): Unit =
      ApiClient.archiveMaterial(id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(id) then resetForm()
          AppBus.mutatedWarehouse()
        case Left(err) => showError(formError, "Archiviazione materiale")(err)
      }

    val confirmArchiveModal =
      confirmDialog(
        isOpen = pendingArchive.signal.map(_.isDefined),
        titleText = Signal.fromValue("Archivia materiale"),
        message = pendingArchive.signal.map(
          _.fold("")(p =>
            s"Archiviare il materiale ${p.internalCode} — ${p.description}? Non sarà più modificabile, ma resterà nello storico del magazzino; codice e MPN tornano disponibili.",
          ),
        ),
        confirmLabel = "Archivia",
        tone = DialogTone.Danger,
        onCancel = () => pendingArchive.set(None),
        onConfirm = () => pendingArchive.now().foreach { p => archive(p.id); pendingArchive.set(None) },
      )

    val issues = form.signal.map(_.issues).distinct

    /** The raw (ungated) inline error of `field`. */
    def rawError(field: Field): Signal[Option[String]] = form.signal.map(_.inlineError(field)).distinct

    /** The [[FieldSpec]] of `field`, its error shown once the field was left or a submit was attempted. */
    def spec(labelText: String, field: Field, required: Boolean): FieldSpec =
      touch.spec(labelText, MaterialForm.fieldId(field), required, rawError(field))

    // ---- MPN rows editor -------------------------------------------------------------------------

    def rowSignal(row: MpnRow): Signal[MpnRow] = form.signal.map(_.mpns.find(_.key == row.key).getOrElse(row))

    def updateRow(key: Int)(patch: MpnRow => MpnRow): Unit = form.update(_.updateRow(key)(patch))

    def renderMpnRow(row: MpnRow): HtmlElement =
      def rowSpec(labelText: String, mpnField: MpnField, required: Boolean): FieldSpec = spec(labelText, Field.Mpn(row.key, mpnField), required)
      div(
        cls := "rounded-md border border-slate-200 p-2",
        div(
          cls := "flex items-start gap-2",
          div(
            cls := "min-w-0 flex-1",
            field(rowSpec("Fornitore", MpnField.Supplier, true))(bind =>
              searchableSelect(
                rowSignal(row).map(_.supplierId),
                Observer[String](next => updateRow(row.key)(_.copy(supplierId = next))),
                supplierOptions,
                "Cerca un fornitore…",
                8,
                bind,
              ),
            ),
          ),
          button(
            tpe := "button",
            cls := s"$btnDanger mt-5",
            aria.label := "Rimuovi MPN",
            "Rimuovi",
            onClick --> (_ => form.update(_.removeRow(row.key))),
          ),
        ),
        div(
          cls := "mt-2 grid gap-2 sm:grid-cols-2",
          field(rowSpec("Codice MPN", MpnField.Code, true))(bind =>
            textInput(rowSignal(row).map(_.code), Observer[String](v => updateRow(row.key)(_.copy(code = v))), "AC-152-NAVY").amend(bind),
          ),
          field(FieldSpec("Descrizione fornitore", s"material-mpn-${row.key}-description"))(bind =>
            textInput(rowSignal(row).map(_.description), Observer[String](v => updateRow(row.key)(_.copy(description = v))), "Opzionale")
              .amend(bind),
          ),
          field(
            rowSpec("Prezzo acquisto previsto (€)", MpnField.ExpectedCost, false)
              .copy(hintText = Some("Proposto come costo unitario nelle righe DDT")),
          )(bind =>
            textInput(rowSignal(row).map(_.expectedUnitCost), Observer[String](v => updateRow(row.key)(_.copy(expectedUnitCost = v))), "Opzionale")
              .amend(inputMode := "decimal", bind),
          ),
        ),
      )
    end renderMpnRow

    // ---- Form ------------------------------------------------------------------------------------

    val pricePreview: Signal[Option[String]] = form.signal.map(state =>
      state.parsedPrice.map(price => s"${WarehouseFormats.formatUnitPrice(price)} / ${WarehouseFormats.unitShortLabel(state.unit)}"),
    )

    def formCard: HtmlElement =
      card(
        cls := "self-start p-4",
        formAnchor.bind,
        child <-- form.signal
          .map(_.editingId.isDefined)
          .distinct
          .map(editing => sectionTitle(if editing then "Modifica materiale" else "Nuovo materiale")),
        p(cls := "mt-1 text-xs text-slate-500", "I campi con * sono obbligatori."),
        div(
          cls := "mt-4 space-y-5",
          fieldGroup("Identificazione")(
            div(
              cls := "grid grid-cols-2 gap-3",
              field(spec("Codice", Field.InternalCode, true))(bind =>
                textInput(form.signal.map(_.internalCode), Observer[String](v => form.update(_.copy(internalCode = v))), "TES-001").amend(bind),
              ),
              field(spec("Unità di misura", Field.Unit, true))(bind =>
                staticSelect(form.signal.map(_.unit), Observer[String](v => form.update(_.copy(unit = v))), MeasureUnit.options).amend(bind),
              ),
              div(
                cls := "col-span-2",
                field(spec("Descrizione", Field.Description, true))(bind =>
                  textInput(form.signal.map(_.description), Observer[String](v => form.update(_.copy(description = v))), "Tessuto acrilico blu navy")
                    .amend(bind),
                ),
              ),
              div(
                cls := "col-span-2",
                field(FieldSpec("Tipologia", "material-type", hintText = Some("Scegli tra le tipologie già usate o scrivine una nuova")))(bind =>
                  div(
                    textInput(form.signal.map(_.materialType), Observer[String](v => form.update(_.copy(materialType = v))), "Opzionale")
                      .amend(listId := materialTypesListId, bind),
                    dataList(idAttr := materialTypesListId, children <-- materialTypes.map(_.map(t => option(value := t)))),
                  ),
                ),
              ),
            ),
          ),
          fieldGroup("Prezzo")(
            field(spec("Prezzo cliente (€)", Field.CustomerPrice, true))(bind =>
              div(
                textInput(form.signal.map(_.customerPrice), Observer[String](v => form.update(_.copy(customerPrice = v))), "18,50")
                  .amend(inputMode := "decimal", bind),
                child.maybe <-- pricePreview.map(_.map(hint)),
              ),
            ),
          ),
          fieldGroup("MPN (codici fornitore)")(
            child.maybe <-- form.signal
              .map(_.mpns.isEmpty)
              .distinct
              .map(empty => Option.when(empty)(div(cls := "text-xs text-slate-400", "Nessun MPN: i fornitori del materiale derivano dagli MPN."))),
            div(cls := "space-y-2", children <-- form.signal.map(_.mpns).split(_.key)((_, initial, _) => renderMpnRow(initial))),
            button(tpe := "button", idAttr := MaterialForm.addMpnId, cls := btnSmall, "+ Aggiungi MPN", onClick --> (_ => form.update(_.addRow))),
          ),
          fieldGroup("Note")(
            field(FieldSpec("Note", "material-notes"))(bind =>
              textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "Note generiche (opzionale)").amend(bind),
            ),
          ),
        ),
        errorList(touch.submittedSignal.combineWith(issues).map((attempted, list) => if attempted then list.messages else Nil)),
        child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
        div(
          cls := "mt-3",
          child <-- form.signal
            .map(_.editingId.isDefined)
            .distinct
            .map(editing =>
              formActions(
                if editing then "Salva" else "Crea",
                "Salvataggio…",
                submitting.signal,
                Option.when(editing)(() => resetForm()),
                () => submit(),
              ),
            ),
        ),
      )

    // ---- Table -----------------------------------------------------------------------------------

    def suppliersCell(material: MaterialResponse): HtmlElement =
      if material.suppliers.isEmpty then div(cls := "text-xs text-slate-400", "—")
      else
        div(
          material.suppliers.map { s =>
            if s.active then div(s.name)
            else div(cls := "text-slate-400 line-through", title := "Fornitore eliminato", s.name)
          },
          Option.when(material.mpns.nonEmpty)(div(cls := "text-xs text-slate-400", material.mpns.map(_.code).mkString(", "))),
        )

    def stockCell(material: MaterialResponse): HtmlElement =
      div(
        div(
          cls := "tabular-nums",
          cls := (if material.stock.negative then "font-medium text-rose-700" else ""),
          WarehouseFormats.formatQty(material.stock.quantity, material.unit),
        ),
        Option.when(material.stock.negative)(
          div(
            cls := "mt-1",
            title := "Giacenza negativa: verificare i movimenti",
            badge("Anomalia", "bg-rose-50 text-rose-700 border-rose-200"),
          ),
        ),
      )

    def actionsCell(material: MaterialResponse): HtmlElement =
      def open(dialog: StockDialog): Unit = stockDialog.set(Some(dialog))
      div(
        cls := "flex flex-col items-end gap-1.5",
        div(
          cls := "flex flex-wrap justify-end gap-1.5",
          Option.when(!material.archived)(
            div(
              roleGated(Role.Operator)(
                div(
                  cls := "flex flex-wrap justify-end gap-1.5",
                  button(tpe := "button", cls := btnSmall, "Carico", onClick --> (_ => open(StockDialog.Purchase(material)))),
                  button(tpe := "button", cls := btnSmall, "Scarico", onClick --> (_ => open(StockDialog.Unload(material)))),
                  button(tpe := "button", cls := btnSmall, "Rettifica", onClick --> (_ => open(StockDialog.Adjust(material)))),
                ),
              ),
            ),
          ),
          div(
            roleGated(Role.Viewer)(button(tpe := "button", cls := btnSmall, "Movimenti", onClick --> (_ => open(StockDialog.Movements(material))))),
          ),
        ),
        Option.when(!material.archived)(
          div(
            roleGated(Role.Admin)(
              div(
                cls := "flex justify-end gap-1.5",
                button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(material))),
                button(tpe := "button", cls := btnDanger, "Archivia", onClick --> (_ => pendingArchive.set(Some(material)))),
              ),
            ),
          ),
        ),
      )
    end actionsCell

    def renderRow(material: MaterialResponse): HtmlElement =
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        cls := (if material.archived then "bg-slate-50 text-slate-400" else ""),
        td(
          cls := "px-4 py-2 font-medium",
          div(cls := (if material.archived then "text-slate-500" else "text-slate-800"), material.internalCode),
          Option.when(material.archived)(
            div(
              cls := "mt-1",
              badge(
                material.archivedOn.fold("Archiviato")(day => s"Archiviato il ${day.split("-").nn.reverse.mkString("/")}"),
                "bg-slate-100 text-slate-500 border-slate-200",
              ),
            ),
          ),
        ),
        td(
          cls := "px-4 py-2",
          div(cls := (if material.archived then "" else "text-slate-700"), material.description),
          material.notes.map(n => div(cls := "text-xs text-slate-400", n)),
        ),
        td(cls := "px-4 py-2 text-slate-500", material.materialType.getOrElse("—")),
        td(cls := "px-4 py-2 text-slate-500", title := WarehouseFormats.unitLabel(material.unit), WarehouseFormats.unitShortLabel(material.unit)),
        td(cls := "px-4 py-2 whitespace-nowrap text-right", stockCell(material)),
        td(
          cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums",
          material.stock.maxUnitCost.map(cost => title := s"Costo unitario max: ${WarehouseFormats.formatUnitPrice(cost)}"),
          material.stock.value.fold[Modifier[HtmlElement]](span(cls := "text-slate-400", title := "Nessun costo d'acquisto registrato", "—"))(value =>
            WarehouseFormats.formatEuro(value),
          ),
        ),
        td(cls := "px-4 py-2 whitespace-nowrap text-right tabular-nums", WarehouseFormats.formatUnitPrice(material.customerPrice)),
        td(cls := "px-4 py-2 text-slate-600", suppliersCell(material)),
        td(cls := "px-4 py-2 text-right", actionsCell(material)),
      )

    def tableCard: HtmlElement =
      card(
        cls := "overflow-hidden",
        div(
          cls := "flex flex-wrap items-center justify-end gap-3 border-b border-slate-100 px-4 py-2",
          div(cls := "mr-auto w-full sm:w-80", searchBox(query, "Cerca per codice, descrizione, MPN, fornitore…", pager)),
          roleGated(Role.Admin)(
            button(
              tpe := "button",
              cls := btnSmall,
              "Carica materiali da Excel",
              onClick --> (_ =>
                lastImport.set(None); importOpen.set(true)
              ),
            ),
          ),
          label(
            cls := "inline-flex items-center gap-2 text-xs font-medium text-slate-600",
            input(typ := "checkbox", checked <-- showArchived.signal, onChange.mapToChecked --> { show => showArchived.set(show); pager.reset() }),
            "Mostra archiviati",
          ),
        ),
        child.maybe <-- lastImport.signal.map(_.map { summary =>
          div(
            cls := "flex items-start justify-between gap-3 border-b border-emerald-200 bg-emerald-50 px-4 py-2 text-sm text-emerald-800",
            div(
              cls := "min-w-0",
              div(MaterialImport.confirmationText(summary)),
              // The problems of the ignored materials stay at hand, to fix the file and load it again.
              Option.when(summary.ignored.nonEmpty)(
                detailsTag(
                  cls := "mt-1 text-amber-800",
                  summaryTag(cls := "cursor-pointer text-xs font-medium", s"Errori dei materiali ignorati (${summary.problems.size})"),
                  MaterialImportDialog.problemsList(summary),
                ),
              ),
            ),
            button(
              tpe := "button",
              cls := "shrink-0 rounded-md px-2 py-0.5 text-xs font-medium text-emerald-700 hover:bg-emerald-100",
              "Chiudi",
              onClick --> (_ => lastImport.set(None)),
            ),
          )
        }),
        renderResult(materials) { all =>
          if all.isEmpty then div(cls := "p-4", emptyState("Nessun materiale in magazzino."))
          else
            searchedPages(sort.sorted(all), query.signal, ListSearch.material, pager, "Nessun materiale corrisponde alla ricerca.") { list =>
              div(
                cls := "overflow-x-auto",
                table(
                  cls := "w-full text-sm",
                  thead(
                    cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                    tr(
                      sort.header("code", "Codice", "px-4 py-2"),
                      sort.header("description", "Descrizione", "px-4 py-2"),
                      sort.header("type", "Tipologia", "px-4 py-2"),
                      sort.header("unit", "UdM", "px-4 py-2"),
                      sort.header("stock", "Giacenza", "px-4 py-2 text-right"),
                      sort.header("value", "Valore", "px-4 py-2 text-right"),
                      sort.header("price", "Prezzo cliente", "px-4 py-2 text-right"),
                      sort.header("suppliers", "Fornitori", "px-4 py-2"),
                      th(cls := "px-4 py-2"),
                    ),
                  ),
                  tbody(list.map(renderRow)),
                ),
              )
            }
        },
      )

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[26rem_1fr]"),
      roleGated(Role.Admin)(formCard),
      tableCard,
      confirmArchiveModal,
      StockDialogs(stockDialog),
      MaterialImportDialog(importOpen, summary => lastImport.set(Some(summary))),
    )
  end apply
end MaterialsTab
