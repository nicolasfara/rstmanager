package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MaterialForm.MpnRow

/**
 * Material registry of the warehouse: internal code, unit of measure, customer price, and the MPNs it is bought as (each tied to a supplier). The
 * material suppliers are derived from the MPNs. Removing a material archives it, so it stays available to the history.
 *
 * The table also shows today's stock and value of each material, with the stock actions of a row: purchase, unload, and adjustment (Operator) and the
 * movements list (Viewer), in the dialogs of [[StockDialogs]].
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

    val materials = loadable(AppBus.warehouseTicks.combineWith(showArchived.signal))(() => ApiClient.listMaterials(showArchived.now()))
    val suppliers = loadable(AppBus.warehouseTicks)(() => ApiClient.listSuppliers())

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

    def resetForm(): Unit =
      form.set(MaterialForm.empty)
      editedSuppliers.set(Nil)
      formError.set(None)

    def edit(material: MaterialResponse): Unit =
      form.set(MaterialForm.fromResponse(material))
      editedSuppliers.set(material.suppliers)
      formError.set(None)

    def submit(): Unit =
      val current = form.now()
      current.request.foreach { request =>
        val effect = current.editingId match
          case Some(id) => ApiClient.updateMaterial(id, request)
          case None => ApiClient.createMaterial(request)
        effect.foreach {
          case Right(_) => resetForm(); AppBus.mutatedWarehouse()
          case Left(err) => showError(formError, "Salvataggio materiale")(err)
        }
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

    val formErrors: Signal[List[String]] = form.signal.map(_.errors)

    // ---- MPN rows editor -------------------------------------------------------------------------

    def rowSignal(row: MpnRow): Signal[MpnRow] = form.signal.map(_.mpns.find(_.key == row.key).getOrElse(row))

    def updateRow(key: Int)(patch: MpnRow => MpnRow): Unit = form.update(_.updateRow(key)(patch))

    def renderMpnRow(row: MpnRow): HtmlElement =
      div(
        cls := "rounded-md border border-slate-200 p-2",
        div(
          cls := "flex items-end gap-2",
          div(
            cls := "flex-1",
            field(
              "Fornitore",
              searchableSelect(
                rowSignal(row).map(_.supplierId),
                Observer[String](next => updateRow(row.key)(_.copy(supplierId = next))),
                supplierOptions,
                "Cerca un fornitore…",
                maxResults = 8,
              ),
            ),
          ),
          button(tpe := "button", cls := s"$btnDanger mb-0.5", "Rimuovi", onClick --> (_ => form.update(_.removeRow(row.key)))),
        ),
        div(
          cls := "mt-2 grid gap-2 sm:grid-cols-2",
          field(
            "Codice MPN",
            textInput(rowSignal(row).map(_.code), Observer[String](v => updateRow(row.key)(_.copy(code = v))), "AC-152-NAVY"),
          ),
          field(
            "Descrizione fornitore",
            textInput(rowSignal(row).map(_.description), Observer[String](v => updateRow(row.key)(_.copy(description = v))), "Opzionale"),
          ),
        ),
      )

    // ---- Form ------------------------------------------------------------------------------------

    val pricePreview: Signal[Option[String]] = form.signal.map(state =>
      state.parsedPrice.map(price => s"${WarehouseFormats.formatUnitPrice(price)} / ${WarehouseFormats.unitShortLabel(state.unit)}"),
    )

    def formCard: HtmlElement =
      card(
        cls := "self-start p-4",
        child <-- form.signal
          .map(_.editingId.isDefined)
          .distinct
          .map(editing => sectionTitle(if editing then "Modifica materiale" else "Nuovo materiale")),
        div(
          cls := "mt-3 grid grid-cols-2 gap-3",
          field("Codice", textInput(form.signal.map(_.internalCode), Observer[String](v => form.update(_.copy(internalCode = v))), "TES-001")),
          field("Unità di misura", staticSelect(form.signal.map(_.unit), Observer[String](v => form.update(_.copy(unit = v))), MeasureUnit.options)),
          div(
            cls := "col-span-2",
            field(
              "Descrizione",
              textInput(form.signal.map(_.description), Observer[String](v => form.update(_.copy(description = v))), "Tessuto acrilico blu navy"),
            ),
          ),
          field(
            "Tipologia",
            div(
              textInput(form.signal.map(_.materialType), Observer[String](v => form.update(_.copy(materialType = v))), "Opzionale")
                .amend(listId := materialTypesListId),
              dataList(idAttr := materialTypesListId, children <-- materialTypes.map(_.map(t => option(value := t)))),
            ),
          ),
          field(
            "Prezzo cliente (€)",
            div(
              textInput(form.signal.map(_.customerPrice), Observer[String](v => form.update(_.copy(customerPrice = v))), "18,50")
                .amend(inputMode := "decimal"),
              child.maybe <-- pricePreview.map(_.map(text => div(cls := "mt-1 text-xs text-slate-400", text))),
            ),
          ),
          div(
            cls := "col-span-2",
            field(
              "Note",
              textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "Note generiche (opzionale)"),
            ),
          ),
        ),
        div(
          cls := "mt-4 space-y-2",
          div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "MPN (codici fornitore)"),
          child.maybe <-- form.signal
            .map(_.mpns.isEmpty)
            .distinct
            .map(empty => Option.when(empty)(div(cls := "text-xs text-slate-400", "Nessun MPN: i fornitori del materiale derivano dagli MPN."))),
          children <-- form.signal.map(_.mpns).split(_.key)((_, initial, _) => renderMpnRow(initial)),
          button(tpe := "button", cls := btnSmall, "+ Aggiungi MPN", onClick --> (_ => form.update(_.addRow))),
        ),
        child.maybe <-- form.signal.combineWith(formErrors).map { (state, errors) =>
          Option.when(!state.pristine && errors.nonEmpty)(ul(cls := "mt-3 list-disc pl-5 text-xs text-rose-600", errors.map(e => li(e))))
        },
        child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
        div(
          cls := "mt-3 flex gap-2",
          button(
            tpe := "button",
            cls := btnPrimary,
            child.text <-- form.signal.map(_.editingId.fold("Crea")(_ => "Salva")),
            disabled <-- formErrors.map(_.nonEmpty),
            onClick --> (_ => submit()),
          ),
          child.maybe <-- form.signal.map(_.editingId.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
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
          cls := "flex items-center justify-end border-b border-slate-100 px-4 py-2",
          label(
            cls := "inline-flex items-center gap-2 text-xs font-medium text-slate-600",
            input(typ := "checkbox", checked <-- showArchived.signal, onChange.mapToChecked --> showArchived),
            "Mostra archiviati",
          ),
        ),
        renderResult(materials) { list =>
          if list.isEmpty then emptyState("Nessun materiale in magazzino.")
          else
            div(
              cls := "overflow-x-auto",
              table(
                cls := "w-full text-sm",
                thead(
                  cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                  tr(
                    th(cls := "px-4 py-2", "Codice"),
                    th(cls := "px-4 py-2", "Descrizione"),
                    th(cls := "px-4 py-2", "Tipologia"),
                    th(cls := "px-4 py-2", "UdM"),
                    th(cls := "px-4 py-2 text-right", "Giacenza"),
                    th(cls := "px-4 py-2 text-right", "Valore"),
                    th(cls := "px-4 py-2 text-right", "Prezzo cliente"),
                    th(cls := "px-4 py-2", "Fornitori"),
                    th(cls := "px-4 py-2"),
                  ),
                ),
                tbody(list.map(renderRow)),
              ),
            )
        },
      )

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[26rem_1fr]"),
      roleGated(Role.Admin)(formCard),
      tableCard,
      confirmArchiveModal,
      StockDialogs(stockDialog),
    )
  end apply
end MaterialsTab
