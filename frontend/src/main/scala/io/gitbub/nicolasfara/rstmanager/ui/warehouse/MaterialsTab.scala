package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{
  AppBus,
  DraftGuard,
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
 * The list shows today's stock and value of each material — stacked cards below `xl`, a compact table from `xl` (see [[MaterialCards]]) — with at
 * most three actions per material: "Movimento" (Operator: purchase, unload, or adjustment in one dialog with a kind switch) and "Movimenti" (Viewer),
 * in the dialogs of [[StockDialogs]], and "Modifica" (Admin). Archiving is done from the edit form. Admins can also load many materials at once from
 * an Excel workbook, in the [[MaterialImportDialog]]; the outcome of the last upload stays above the list until dismissed.
 */
object MaterialsTab:

  private val materialTypesListId = "warehouse-material-types"

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val form = Var(MaterialForm.empty)
    val showArchived = Var(false)
    val pendingArchive = Var(Option.empty[MaterialResponse])
    val stockDialog = Var(Option.empty[StockDialog])
    // The material being edited: its suppliers keep a name in the row of an MPN whose supplier was deleted meanwhile, and it is what "Archivia" asks
    // about.
    val editedMaterial = Var(Option.empty[MaterialResponse])
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
      suppliers.combineWith(editedMaterial.signal.map(_.fold(Nil)(_.suppliers))).map { (loaded, edited) =>
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
    val draft = DraftGuard[MaterialForm.State](
      () => form.now(),
      s => DraftGuard.discardTitle(Option.when(s.editingId.isDefined)(s.internalCode.trim.nn), "il nuovo materiale"),
    )

    def resetForm(): Unit =
      form.set(MaterialForm.empty)
      editedMaterial.set(None)
      formError.set(None)
      touch.reset()
      draft.markClean()

    val formAnchor = FormAnchor()

    // Replacing a dirty draft asks first; the form is revealed once the new content is loaded.
    def edit(material: MaterialResponse): Unit =
      draft.discardThen { () =>
        formAnchor.revealAndFocus()
        form.set(MaterialForm.fromResponse(material))
        editedMaterial.set(Some(material))
        formError.set(None)
        touch.reset()
        draft.markClean()
      }

    /** "+ Nuovo materiale" (mobile, where the form follows the table): leaves a material being edited, then shows the form. */
    def startNew(): Unit =
      if form.now().editingId.isDefined then
        draft.discardThen(() =>
          resetForm(); formAnchor.revealAndFocus(),
        )
      else formAnchor.revealAndFocus()

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
                  Notifications.success(s"Materiale ${request.internalCode} ${if current.editingId.isDefined then "salvato" else "creato"}")
                  AppBus.mutatedWarehouse()
                case Left(err) => showError(formError, "Salvataggio materiale")(err)
            }

    def archive(material: MaterialResponse): Unit =
      ApiClient.archiveMaterial(material.id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(material.id) then resetForm()
          Notifications.success(s"Materiale ${material.internalCode} archiviato")
          AppBus.mutatedWarehouse()
        case Left(err) => showError(formError, "Archiviazione materiale")(err)
      }

    val confirmArchiveModal =
      confirmDialog(
        isOpen = pendingArchive.signal.map(_.isDefined),
        titleText = pendingArchive.signal.map(_.fold("")(p => s"Archiviare il materiale «${p.internalCode} · ${p.description}»?")),
        message = Signal.fromValue(
          "Non sarà più modificabile, ma resterà nello storico del magazzino; codice e MPN tornano disponibili. L'operazione non è reversibile.",
        ),
        confirmLabel = "Archivia",
        tone = DialogTone.Danger,
        onCancel = () => pendingArchive.set(None),
        onConfirm = () => pendingArchive.now().foreach { p => archive(p); pendingArchive.set(None) },
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
        // Mobile: the table comes first (finding a material must not mean scrolling past the form); "+ Nuovo materiale" reaches the form.
        cls := "order-last self-start p-4 lg:order-none",
        formAnchor.bind,
        draft.bind,
        child <-- form.signal
          .map(_.editingId.isDefined)
          .distinct
          .map(editing => sectionTitle(if editing then "Modifica materiale" else "Nuovo materiale")),
        requiredLegend.amend(cls := "mt-1"),
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
                Option.when(editing)(() => draft.discardThen(() => resetForm())),
                () => submit(),
              ),
            ),
        ),
        child.maybe <-- editedMaterial.signal.map(_.map(dangerZone)),
      )

    /** Archiving the material being edited, out of the list rows: asks for confirmation first. */
    def dangerZone(material: MaterialResponse): HtmlElement =
      div(
        cls := "mt-6 rounded-md border border-rose-200 bg-rose-50/40 p-3",
        div(cls := "text-xs font-semibold uppercase tracking-wide text-rose-700", "Zona pericolosa"),
        p(
          cls := "mt-1 text-xs text-slate-600",
          "Archiviando il materiale non sarà più modificabile né movimentabile, ma resterà nello storico del magazzino.",
        ),
        button(
          tpe := "button",
          cls := s"$btnDanger mt-2 min-h-9",
          "Archivia materiale",
          onClick --> (_ => pendingArchive.set(Some(material))),
        ),
      )

    // ---- List: cards below xl, compact table from xl -------------------------------------------

    /** The actions of a material (at most three): "Movimento" (Operator), "Movimenti" (Viewer), "Modifica" (Admin); an archived one only lists. */
    def actionButtons(material: MaterialResponse): List[Modifier[HtmlElement]] =
      def open(dialog: StockDialog): Unit = stockDialog.set(Some(dialog))
      List(
        Option.when(!material.archived)(
          roleGated(Role.Operator)(
            button(
              tpe := "button",
              cls := btnSmall,
              aria.label := s"Registra un movimento di ${material.internalCode}",
              "Movimento",
              onClick --> (_ => open(StockDialog.Purchase(material))),
            ),
          ),
        ),
        roleGated(Role.Viewer)(
          button(
            tpe := "button",
            cls := btnSmall,
            aria.label := s"Movimenti di ${material.internalCode}",
            "Movimenti",
            onClick --> (_ => open(StockDialog.Movements(material))),
          ),
        ),
        Option.when(!material.archived)(
          roleGated(Role.Admin)(
            button(
              tpe := "button",
              cls := btnSmall,
              aria.label := s"Modifica ${material.internalCode}",
              "Modifica",
              onClick --> (_ => edit(material)),
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

    def tableCard: HtmlElement =
      card(
        cls := "overflow-hidden",
        div(
          cls := "space-y-2 border-b border-slate-100 p-3",
          div(
            cls := "flex flex-wrap items-center gap-2",
            div(cls := "w-full sm:mr-auto sm:w-80", searchBox(query, "Cerca per codice, descrizione, MPN, fornitore…", pager)),
            roleGated(Role.Admin)(
              button(tpe := "button", cls := s"$btnSmall min-h-10 lg:hidden", "+ Nuovo materiale", onClick --> (_ => startNew())),
            ),
            roleGated(Role.Admin)(
              button(
                tpe := "button",
                cls := s"$btnSmall min-h-10 xl:min-h-0",
                "Carica da Excel",
                title := "Carica materiali da un file Excel",
                onClick --> (_ =>
                  lastImport.set(None); importOpen.set(true)
                ),
              ),
            ),
            label(
              cls := "inline-flex min-h-10 items-center gap-2 text-xs font-medium text-slate-600 xl:min-h-0",
              input(typ := "checkbox", checked <-- showArchived.signal, onChange.mapToChecked --> { show => showArchived.set(show); pager.reset() }),
              "Mostra archiviati",
            ),
          ),
          div(
            cls := "xl:hidden",
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
                // Below xl: stacked cards (from lg the side form leaves the list too narrow for a table). From xl: the compact table.
                div(cls := "divide-y divide-slate-100 xl:hidden", list.map(renderCard)),
                div(
                  cls := "hidden overflow-x-auto xl:block",
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
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[26rem_1fr]"),
      roleGated(Role.Admin)(formCard),
      tableCard,
      confirmArchiveModal,
      StockDialogs(stockDialog),
      MaterialImportDialog(importOpen, summary => lastImport.set(Some(summary))),
      draft.dialog,
    )
  end apply
end MaterialsTab
