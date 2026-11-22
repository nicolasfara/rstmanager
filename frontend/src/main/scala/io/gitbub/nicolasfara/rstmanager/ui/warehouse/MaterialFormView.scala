package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, DraftGuard, FormFocus, FormTouch, Notifications, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats.MeasureUnit
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MaterialForm.{ Field, MpnField, MpnRow }

/**
 * The material create/edit page ("Nuovo materiale" / "Modifica materiale «TES-001»"), shown in place of the materials list: internal code, unit of
 * measure, customer price, and the MPNs it is bought as (each tied to a supplier; the material suppliers derive from them). The rules live in
 * [[MaterialForm]]; an error shows under its field once the field was left or after a submit, and an invalid submit focuses the first one.
 *
 * Saving shows a toast, calls [[AppBus.mutatedWarehouse]], and goes back to the list (`onExit`); an edit with no change goes back without a request.
 * "← Torna ai materiali" and "Annulla" ask before dropping unsaved changes. In edit mode the last block archives the material (after a confirmation):
 * it stays in the warehouse history, and the page goes back to the list. When a successful reload of `materials` no longer lists the material being
 * edited (archived or removed by someone else), a note says so and saving is blocked.
 */
object MaterialFormView:

  private val materialTypesListId = "warehouse-material-types"
  private val submitErrorId = "material-submit-error"

  /** The page for a new material (`editing` = `None`) or for editing `editing`, with the tab's `materials` and `suppliers` lists. */
  def apply(
      editing: Option[MaterialResponse],
      materials: Signal[Option[ApiClient.Result[List[MaterialResponse]]]],
      suppliers: Signal[Option[ApiClient.Result[List[SupplierResponse]]]],
      onExit: () => Unit,
  ): HtmlElement =
    val form = Var(editing.fold(MaterialForm.empty)(MaterialForm.fromResponse))
    val formError = Var(Option.empty[ApiError])
    val pendingArchive = Var(false)
    val gone = Var(false)
    val touch = FormTouch()
    val submitting = Var(false)
    val draft = DraftGuard[MaterialForm.State](
      () => form.now(),
      _ => DraftGuard.discardTitle(editing.map(_.internalCode), "il nuovo materiale"),
    )
    // Nothing to lose once the material is gone: the draft cannot be saved anyway.
    val leave: () => Unit = () => if gone.now() then onExit() else draft.discardThen(onExit)

    // The suppliers of the MPN rows: the active ones, plus those of the edited material deleted meanwhile (marked), so its rows keep a name.
    val supplierOptions: Signal[List[(String, String)]] =
      suppliers.map { loaded =>
        val active = loaded match
          case Some(Right(list)) => list.map(s => s.id.toString -> s.name)
          case _ => Nil
        val activeIds = active.map(_._1).toSet
        val deleted = editing.fold(Nil)(_.suppliers).filterNot(s => activeIds.contains(s.id.toString))
        active ++ deleted.map(s => s.id.toString -> s"${s.name} (eliminato)")
      }

    val materialTypes: Signal[List[String]] = materials.map {
      case Some(Right(list)) => MaterialForm.materialTypes(list)
      case _ => Nil
    }

    def submit(): Unit =
      if !submitting.now() then
        val current = form.now()
        current.request match
          case None =>
            touch.attemptSubmit()
            current.issues.headOption.foreach(issue => FormFocus.focus(MaterialForm.fieldId(issue.field)))
          // An edit without changes: nothing to send.
          case Some(_) if editing.isDefined && !draft.isDirtyNow => onExit()
          case Some(request) =>
            submitting.set(true)
            formError.set(None)
            val effect = editing match
              case Some(material) => ApiClient.updateMaterial(material.id, request)
              case None => ApiClient.createMaterial(request)
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(_) =>
                  Notifications.success(s"Materiale ${request.internalCode} ${if editing.isDefined then "salvato" else "creato"}")
                  AppBus.mutatedWarehouse()
                  onExit()
                case Left(err) =>
                  showError(formError, "Salvataggio materiale")(err)
                  FormFocus.focus(submitErrorId)
            }
        end match

    def archive(material: MaterialResponse): Unit =
      ApiClient.archiveMaterial(material.id).foreach {
        case Right(_) =>
          Notifications.success(s"Materiale ${material.internalCode} archiviato")
          AppBus.mutatedWarehouse()
          onExit()
        case Left(err) =>
          showError(formError, "Archiviazione materiale")(err)
          FormFocus.focus(submitErrorId)
      }

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
        cls := "rounded-md border border-slate-200 p-3",
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
            cls := s"$btnDanger mt-5 min-h-10 md:min-h-0",
            aria.label := "Rimuovi MPN",
            "Rimuovi",
            onClick --> (_ => form.update(_.removeRow(row.key))),
          ),
        ),
        div(
          cls := "mt-3 grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
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

    // ---- Sections --------------------------------------------------------------------------------

    val fieldGrid = "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2"

    val pricePreview: Signal[Option[String]] = form.signal.map(state =>
      state.parsedPrice.map(price => s"${WarehouseFormats.formatUnitPrice(price)} / ${WarehouseFormats.unitShortLabel(state.unit)}"),
    )

    val identification =
      formSection("Identificazione", "")(
        div(
          cls := fieldGrid,
          field(spec("Codice", Field.InternalCode, true))(bind =>
            textInput(form.signal.map(_.internalCode), Observer[String](v => form.update(_.copy(internalCode = v))), "TES-001").amend(bind),
          ),
          field(spec("Unità di misura", Field.Unit, true))(bind =>
            staticSelect(form.signal.map(_.unit), Observer[String](v => form.update(_.copy(unit = v))), MeasureUnit.options).amend(bind),
          ),
          div(
            cls := "sm:col-span-2",
            field(spec("Descrizione", Field.Description, true))(bind =>
              textInput(form.signal.map(_.description), Observer[String](v => form.update(_.copy(description = v))), "Tessuto acrilico blu navy")
                .amend(bind),
            ),
          ),
          div(
            cls := "sm:col-span-2",
            field(FieldSpec("Tipologia", "material-type", hintText = Some("Scegli tra le tipologie già usate o scrivine una nuova")))(bind =>
              div(
                textInput(form.signal.map(_.materialType), Observer[String](v => form.update(_.copy(materialType = v))), "Opzionale")
                  .amend(listId := materialTypesListId, bind),
                dataList(idAttr := materialTypesListId, children <-- materialTypes.map(_.map(t => option(value := t)))),
              ),
            ),
          ),
        ),
      )

    val price =
      formSection("Prezzo", "")(
        div(
          cls := fieldGrid,
          field(spec("Prezzo cliente (€)", Field.CustomerPrice, true))(bind =>
            div(
              textInput(form.signal.map(_.customerPrice), Observer[String](v => form.update(_.copy(customerPrice = v))), "18,50")
                .amend(inputMode := "decimal", bind),
              child.maybe <-- pricePreview.map(_.map(hint)),
            ),
          ),
        ),
      )

    val mpns =
      formSection("MPN (codici fornitore)", "I codici con cui il materiale si acquista; i fornitori del materiale derivano da questi.")(
        child.maybe <-- form.signal
          .map(_.mpns.isEmpty)
          .distinct
          .map(empty => Option.when(empty)(emptyState("Nessun MPN."))),
        div(cls := "space-y-3 empty:hidden", children <-- form.signal.map(_.mpns).split(_.key)((_, initial, _) => renderMpnRow(initial))),
        button(
          tpe := "button",
          idAttr := MaterialForm.addMpnId,
          cls := s"$btnGhost min-h-10 md:min-h-0",
          "+ Aggiungi MPN",
          onClick --> (_ => form.update(_.addRow)),
        ),
      )

    val notesSection =
      formSection("Note", "")(
        field(FieldSpec("Note", "material-notes"))(bind =>
          textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "Note generiche (opzionale)").amend(bind),
        ),
        child.maybe <-- formError.signal.map(_.map(err => errorBanner(err).amend(idAttr := submitErrorId))),
      )

    val blocked: Signal[Option[String]] = gone.signal.map(isGone => Option.when(isGone)("Materiale non più disponibile"))

    val goneNote: Modifier[HtmlElement] =
      child.maybe <-- gone.signal.map(isGone =>
        Option.when(isGone)(
          warningNote(
            s"Il materiale «${editing.fold("")(_.internalCode)}» non è più in anagrafica: potrebbe essere stato archiviato da un altro utente. " +
              "Le modifiche non possono essere salvate.",
          ),
        ),
      )

    val title = editing.fold("Nuovo materiale")(m => s"Modifica materiale «${m.internalCode}»")

    formPage(
      PageHeader(
        "← Torna ai materiali",
        leave,
        Signal.fromValue(title),
        context = List("Magazzino", "Materiali") ++ editing.map(_.internalCode).toList,
        notes = goneNote,
        initialFocus = if editing.isDefined then PageFocus.Title else PageFocus.FirstField,
      ),
    )(
      draft.bind,
      editing.map(material =>
        DraftGuard.trackGone(
          materials.map {
            case Some(Right(list)) => !list.exists(m => m.id == material.id && !m.archived)
            case _ => false
          },
          gone,
        )(() => ()),
      ),
      formColumn(
        identification,
        price,
        mpns,
        notesSection,
        editing.map(material => dangerZone(material, gone.signal, () => pendingArchive.set(true))),
        stickyActionBar(
          dirtyNote(form.signal.map(_ => draft.isDirtyNow)),
          formActionsInline(
            if editing.isDefined then "Salva modifiche" else "Crea materiale",
            if editing.isDefined then "Salvataggio…" else "Creazione…",
            submitting.signal,
            blocked,
            Some(leave),
            () => submit(),
          ),
        ),
      ),
      draft.dialog,
      editing.map(material =>
        confirmDialog(
          isOpen = pendingArchive.signal,
          titleText = Signal.fromValue(s"Archiviare il materiale «${material.internalCode} · ${material.description}»?"),
          message = Signal.fromValue(
            "Non sarà più modificabile, ma resterà nello storico del magazzino; codice e MPN tornano disponibili. L'operazione non è reversibile.",
          ),
          confirmLabel = "Archivia",
          tone = DialogTone.Danger,
          onCancel = () => pendingArchive.set(false),
          onConfirm = () =>
            pendingArchive.set(false)
            archive(material),
        ),
      ),
    )
  end apply

  /** The last block of the edit page: archiving the material, out of the list rows; asks for confirmation first (`onArchive`). */
  private def dangerZone(material: MaterialResponse, gone: Signal[Boolean], onArchive: () => Unit): HtmlElement =
    sectionTag(
      aria.label := "Zona pericolosa",
      cls := "rounded-xl border border-rose-200 bg-rose-50/40 p-4",
      h2(cls := "text-sm font-semibold text-rose-700", "Zona pericolosa"),
      p(
        cls := "mt-1 text-sm text-slate-600",
        s"Archiviando «${material.internalCode}» non sarà più modificabile né movimentabile, ma resterà nello storico del magazzino.",
      ),
      button(
        tpe := "button",
        cls := s"$btnDanger mt-3 min-h-10 md:min-h-9",
        "Archivia materiale",
        disabled <-- gone,
        onClick --> (_ => onArchive()),
      ),
    )
end MaterialFormView
