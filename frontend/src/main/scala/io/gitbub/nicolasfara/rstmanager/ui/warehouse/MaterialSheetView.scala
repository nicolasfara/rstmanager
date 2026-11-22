package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
  Notifications,
  Pagination,
  Paging,
  TableSort,
  WarehouseFormats,
}
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.{ Column, Direction, SortBy }
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MovementForm.{ AdjustmentState, Draft, Field, PurchaseState, UnloadState }
import org.scalajs.dom

/** Which part of the material sheet an opening is about: the movement form ("Movimento") or the movements list ("Movimenti"). */
enum SheetFocus derives CanEqual:
  case Movement
  case History

/**
 * The "Scheda materiale" page of a material, shown in place of the materials list: the header with the current stock, the movement form ("Registra
 * movimento": purchase, unload, or adjustment; Operator, not for an archived material) on top, and the movements list below (Viewer) with the running
 * balance and the void action (Admin). The list also shows the automatic consumptions by order manufacturings ("Scarico lavorazione", and "Storno
 * lavorazione" when a reopened manufacturing gives the material back) with their order, which cannot be voided, and the purchases loaded by a
 * delivery note ("DDT n. X"), corrected only through the delivery note.
 *
 * The form holds one [[MovementForm.Draft]]: the "Carico | Scarico | Rettifica" switch replaces it with the other kind's form, handing over the
 * fields already typed ([[MovementForm.Carry]]). A recorded movement keeps the user on the page: a toast, [[AppBus.mutatedWarehouse]] (which
 * refreshes the list below, the materials list and the reports), the header stock from the server's answer, the form back to the blank state of the
 * same kind with the focus on its first field, and the new movement marked on the first page of the list (newest first). Several movements on the
 * same material are therefore quick; another material is one "← Torna ai materiali" away.
 *
 * The header follows the material in the tab's `materials` list, so the stock stays current; when a successful reload no longer lists it (archived or
 * removed by someone else), a note says so and recording is blocked.
 */
object MaterialSheetView:

  private val disabledInputCls = "disabled:bg-slate-100 disabled:text-slate-400"

  /** The DOM id of the kind switch (the focus of a "Movimento" opening). */
  val kindSwitchId = "movement-kind"

  /** The DOM id of the movements section (the focus of a "Movimenti" opening). */
  val movementsSectionId = "material-movements"

  private val submitErrorId = "movement-submit-error"

  /**
   * The sheet of `material` (the snapshot the user clicked), focused on `focus`. `materials` is the tab's materials list, followed for the header;
   * `onEdit` opens the edit page of the material (Admin) and `onExit` goes back to the list, both after the draft guard.
   */
  def apply(
      material: MaterialResponse,
      focus: SheetFocus,
      materials: Signal[Option[ApiClient.Result[List[MaterialResponse]]]],
      onEdit: MaterialResponse => Unit,
      onExit: () => Unit,
  ): HtmlElement =
    val today = WarehousePage.today()
    // The latest known state of the material: the tab's list, or the answer to a movement just recorded.
    val latest = Var(material)
    // True once a successful reload of the list no longer has the material.
    val gone = Var(false)
    // The movement recorded last on this page, marked in the list below.
    val recorded = Var(Option.empty[UUID])
    // What the blank form of each kind is built from: the opening snapshot, then the material as returned by the last movement.
    val formMaterial = Var(material)
    val draft = Var(MovementForm.blank(MovementKind.Purchase, material, today))
    // A blank form (of any kind) has nothing to lose; neither has a form that can no longer be saved.
    val guard = DraftGuard[Option[Draft]](
      () => if gone.now() then None else MovementForm.unsaved(draft.now(), formMaterial.now(), today),
      _ => DraftGuard.discardTitle(None, "il movimento non registrato"),
    )
    val leave: () => Unit = () => guard.discardThen(onExit)
    val canRecord = !material.archived

    val stockBadge: Signal[HtmlElement] = latest.signal.map { current =>
      val text = s"Giacenza: ${WarehouseFormats.formatQty(current.stock.quantity, current.unit)}"
      if current.stock.negative then
        span(title := "Giacenza negativa: verificare i movimenti", badge(s"$text (negativa)", "bg-rose-50 text-rose-700 border-rose-200"))
      else badge(text, "bg-slate-50 text-slate-700 border-slate-200")
    }

    val notes: Modifier[HtmlElement] = List[Modifier[HtmlElement]](
      Option.when(material.archived)(infoNote("Materiale archiviato: non movimentabile.")),
      child.maybe <-- gone.signal.map(isGone =>
        Option.when(isGone)(
          warningNote(
            s"Il materiale «${material.internalCode}» non è più in anagrafica: potrebbe essere stato archiviato da un altro utente. " +
              "I movimenti non possono essere registrati.",
          ),
        ),
      ),
    )

    val editAction: Modifier[HtmlElement] =
      Option.when(canRecord)(
        roleGated(Role.Admin)(
          button(
            tpe := "button",
            cls := s"$btnSmall min-h-10 md:min-h-0",
            aria.label := s"Modifica ${material.internalCode}",
            "Modifica",
            disabled <-- gone.signal,
            onClick --> (_ => guard.discardThen(() => onEdit(latest.now()))),
          ),
        ),
      )

    val pageFocus = focus match
      case SheetFocus.Movement if canRecord => PageFocus.Element(kindSwitchId)
      case _ => PageFocus.Element(movementsSectionId)

    formPage(
      PageHeader(
        "← Torna ai materiali",
        leave,
        Signal.fromValue(s"${material.internalCode} · ${material.description}"),
        context = List("Magazzino", "Materiali"),
        status = child <-- stockBadge,
        actions = editAction,
        notes = notes,
        requiredLegend = false,
        initialFocus = pageFocus,
      ),
    )(
      guard.bind,
      materials --> {
        case Some(Right(list)) =>
          list.find(_.id == material.id) match
            case Some(found) =>
              latest.set(found)
              gone.set(false)
            case None => gone.set(true)
        case _ => ()
      },
      formColumn("max-w-6xl")(
        Option.when(canRecord)(
          roleGated(Role.Operator)(movementSection(material, today, draft, formMaterial, latest, gone.signal, recorded, guard)),
        ),
        formSection("Movimenti", "Dal più recente; il saldo è la giacenza dopo ogni movimento.")(movementsView(material, recorded.signal))
          .amend(idAttr := movementsSectionId, aria.label := "Movimenti"),
      ),
      guard.dialog,
    )
  end apply

  // ---- Movement form -----------------------------------------------------------------------------

  private def dateInput(valueSignal: Signal[String], writer: Observer[String], today: String): HtmlElement =
    textInput(valueSignal, writer, "", "date").amend(maxAttr := today)

  private def decimalInput(valueSignal: Signal[String], writer: Observer[String], placeholderText: String): HtmlElement =
    textInput(valueSignal, writer, placeholderText).amend(inputMode := "decimal")

  private def piecesHint(unit: String): Option[String] =
    Option.when(WarehouseFormats.MeasureUnit.fromWire(unit).exists(!_.allowsFractions))("Solo quantità intere")

  /** The hints of a field joined with " · ", `None` when there is none. */
  private def hints(texts: Option[String]*): Option[String] = Some(texts.flatten.mkString(" · ")).filter(_.nonEmpty)

  /**
   * Section 1, "Registra movimento": the kind switch, the fields of the kind (an error shows after leaving its field or after a submit), and the
   * submit, always enabled except while sending or when the material is gone. On success the form resets to the same kind and the page stays.
   */
  private def movementSection(
      material: MaterialResponse,
      today: String,
      draft: Var[Draft],
      formMaterial: Var[MaterialResponse],
      latest: Var[MaterialResponse],
      gone: Signal[Boolean],
      recorded: Var[Option[UUID]],
      guard: DraftGuard[Option[Draft]],
  ): HtmlElement =
    val touch = FormTouch()
    val submitting = Var(false)
    val submitError = Var(Option.empty[ApiError])
    val kind = draft.signal.map(_.kind).distinct

    def spec(labelText: String, field: Field, required: Boolean, hintText: Option[String]): FieldSpec =
      val raw = draft.signal.map(_.inlineError(field)).distinct
      val id = MovementForm.fieldId(field)
      hintText.fold(touch.spec(labelText, id, required, raw))(text => touch.spec(labelText, id, required, text, raw))

    // The state of each kind, read by its fields (mounted only while the draft has that kind; the fallback is never shown).
    val purchase = draft.signal.map {
      case Draft.Purchase(state) => state
      case _ => MovementForm.purchase(material, today)
    }
    val unload = draft.signal.map {
      case Draft.Unload(state) => state
      case _ => MovementForm.unload(material, today)
    }
    val adjustment = draft.signal.map {
      case Draft.Adjustment(state) => state
      case _ => MovementForm.adjustment(material, today)
    }
    def updatePurchase(patch: PurchaseState => PurchaseState): Unit = draft.update {
      case Draft.Purchase(state) => Draft.Purchase(patch(state))
      case other => other
    }
    def updateUnload(patch: UnloadState => UnloadState): Unit = draft.update {
      case Draft.Unload(state) => Draft.Unload(patch(state))
      case other => other
    }
    def updateAdjustment(patch: AdjustmentState => AdjustmentState): Unit = draft.update {
      case Draft.Adjustment(state) => Draft.Adjustment(patch(state))
      case other => other
    }

    def switchKind(key: String): Unit =
      MovementKind.fromKey(key).foreach(next => draft.update(MovementForm.switchKind(_, next, formMaterial.now(), today)))

    def request(current: Draft): Option[() => Future[ApiClient.Result[MovementRecordedResponse]]] = current match
      case Draft.Purchase(state) => state.request.map(body => () => ApiClient.recordPurchase(material.id, body))
      case Draft.Unload(state) => state.request.map(body => () => ApiClient.recordUnload(material.id, body))
      case Draft.Adjustment(state) => state.request.map(body => () => ApiClient.recordAdjustment(material.id, body))

    def recordedOk(kindDone: MovementKind, done: MovementRecordedResponse): Unit =
      Notifications.success(kindDone.recordedText(material.internalCode))
      AppBus.mutatedWarehouse()
      latest.set(done.material)
      formMaterial.set(done.material)
      val fresh = MovementForm.blank(kindDone, done.material, today)
      draft.set(fresh)
      touch.reset()
      guard.markClean()
      recorded.set(Some(done.movementId))
      FormFocus.focus(MovementForm.fieldId(fresh.firstField))

    def submit(): Unit =
      if !submitting.now() then
        val current = draft.now()
        current.issues.headOption match
          case Some(issue) =>
            touch.attemptSubmit()
            FormFocus.focus(MovementForm.fieldId(issue.field))
          case None =>
            request(current).foreach { effect =>
              submitting.set(true)
              submitError.set(None)
              effect().foreach { result =>
                submitting.set(false)
                result match
                  case Right(done) => recordedOk(current.kind, done)
                  case Left(err) =>
                    showError(submitError, s"${current.kind.label} ${material.internalCode}")(err)
                    FormFocus.focus(submitErrorId)
              }
            }

    val unitLabel = WarehouseFormats.unitShortLabel(material.unit)
    val fieldGrid = "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2"

    def purchaseFields: HtmlElement =
      val mpns = MovementForm.mpnOptions(formMaterial.now())
      val mpnChoices = ("" -> "Seleziona un MPN…") :: mpns.map(o => o.key -> o.label)
      div(
        cls := "space-y-4",
        div(
          cls := fieldGrid,
          div(
            cls := "sm:col-span-2",
            if mpns.isEmpty then
              warningNote("Il materiale non ha MPN: aggiungine uno in anagrafica («Modifica») prima di registrare un carico.")
                .amend(idAttr := MovementForm.fieldId(Field.Mpn))
            else
              field(spec("MPN acquistato (fornitore)", Field.Mpn, true, None))(bind =>
                staticSelect(purchase.map(_.mpnKey), Observer[String](v => updatePurchase(_.copy(mpnKey = v))), mpnChoices).amend(bind),
              ),
          ),
          field(spec(s"Quantità ($unitLabel)", Field.Quantity, true, piecesHint(material.unit)))(bind =>
            decimalInput(purchase.map(_.quantity), Observer[String](v => updatePurchase(_.copy(quantity = v))), "25,5").amend(bind),
          ),
          field(spec("Costo unitario (€)", Field.UnitCost, true, None))(bind =>
            decimalInput(purchase.map(_.unitCost), Observer[String](v => updatePurchase(_.copy(unitCost = v))), "3,50").amend(bind),
          ),
          field(spec("Data", Field.Day, true, None))(bind =>
            dateInput(purchase.map(_.on), Observer[String](v => updatePurchase(_.copy(on = v))), today).amend(bind),
          ),
          field(FieldSpec("Note", "movement-note"))(bind =>
            textInput(purchase.map(_.note), Observer[String](v => updatePurchase(_.copy(note = v))), "Es. DDT 1234 (opzionale)").amend(bind),
          ),
        ),
        div(
          cls := "flex items-baseline justify-between rounded-md border border-slate-200 px-3 py-2 text-sm",
          span(cls := "text-slate-500", "Totale carico"),
          span(
            aria.live := "polite",
            cls := "font-semibold tabular-nums text-slate-800",
            child.text <-- purchase.map(_.totalPreview.fold("—")(WarehouseFormats.formatEuro)),
          ),
        ),
      )
    end purchaseFields

    def unloadFields: HtmlElement =
      div(
        cls := "space-y-4",
        div(
          cls := fieldGrid,
          field(spec(s"Quantità ($unitLabel)", Field.Quantity, true, piecesHint(material.unit)))(bind =>
            decimalInput(unload.map(_.quantity), Observer[String](v => updateUnload(_.copy(quantity = v))), "2").amend(bind),
          ),
          field(spec("Data", Field.Day, true, None))(bind =>
            dateInput(unload.map(_.on), Observer[String](v => updateUnload(_.copy(on = v))), today).amend(bind),
          ),
          div(
            cls := "sm:col-span-2",
            field(spec("Motivo", Field.Reason, true, None))(bind =>
              textInput(unload.map(_.reason), Observer[String](v => updateUnload(_.copy(reason = v))), "Es. campionatura per cliente").amend(bind),
            ),
          ),
        ),
        infoNote("Lo scarico è rifiutato se la giacenza scenderebbe sotto zero in quella data o in seguito."),
      )

    def adjustmentFields: HtmlElement =
      div(
        cls := fieldGrid,
        field(
          spec(
            s"Variazione ($unitLabel)",
            Field.Quantity,
            true,
            hints(Some("Negativa per togliere, positiva per aggiungere"), piecesHint(material.unit)),
          ),
        )(bind => textInput(adjustment.map(_.delta), Observer[String](v => updateAdjustment(_.copy(delta = v))), "-2 oppure 10").amend(bind)),
        field(spec("Costo unitario (€)", Field.UnitCost, false, Some("Solo per rettifiche in aumento: vale come prezzo d'acquisto")))(bind =>
          decimalInput(adjustment.map(_.unitCost), Observer[String](v => updateAdjustment(_.copy(unitCost = v))), "Opzionale")
            .amend(cls := disabledInputCls, disabled <-- adjustment.map(!_.costEnabled), bind),
        ),
        field(spec("Data", Field.Day, true, None))(bind =>
          dateInput(adjustment.map(_.on), Observer[String](v => updateAdjustment(_.copy(on = v))), today).amend(bind),
        ),
        field(spec("Motivo", Field.Reason, true, None))(bind =>
          textInput(adjustment.map(_.reason), Observer[String](v => updateAdjustment(_.copy(reason = v))), "Es. inventario").amend(bind),
        ),
      )

    val blocked: Signal[Option[String]] = gone.map(isGone => Option.when(isGone)("Materiale non più disponibile"))
    val unsaved: Signal[Boolean] = draft.signal.combineWith(formMaterial.signal).map((d, m) => MovementForm.unsaved(d, m, today).isDefined)

    div(
      cls := "max-w-3xl",
      formSection("Registra movimento", "La giacenza e i movimenti qui sotto si aggiornano appena il movimento è registrato.")(
        div(
          // Outside the rebuilt fields, so the focus stays on the switch when the kind changes.
          segmented(
            kind.map(_.key),
            Observer[String](switchKind),
            MovementKind.options,
            List[Modifier[HtmlElement]](idAttr := kindSwitchId, aria.label := "Tipo di movimento", forwardFocusToChecked),
          ),
        ),
        child <-- kind.map {
          case MovementKind.Purchase => purchaseFields
          case MovementKind.Unload => unloadFields
          case MovementKind.Adjustment => adjustmentFields
        },
        child.maybe <-- submitError.signal.map(_.map(err => errorBanner(err).amend(idAttr := submitErrorId))),
        div(
          cls := "flex flex-wrap items-center gap-3 border-t border-slate-100 pt-3",
          dirtyNote(unsaved),
          child <-- kind.map(k => formActionsInline(k.submitLabel, "Registrazione…", submitting.signal, blocked, None, () => submit())),
        ),
      ),
    )
  end movementSection

  /**
   * On the kind switch: when the group itself gets the focus (the page focusing [[kindSwitchId]]), hands it to the checked option, so the arrow keys
   * work at once.
   */
  private def forwardFocusToChecked: Modifier[HtmlElement] =
    inContext { group =>
      onFocus --> { event =>
        if event.target.eq(group.ref) then
          group.ref.querySelector("[aria-checked='true']") match
            case option: dom.HTMLElement => option.focus()
            case _ => ()
      }
    }

  // ---- Movements ---------------------------------------------------------------------------------

  /** The movements list with its filters, newest first; `recorded` (the movement just recorded on the page) is marked and brings back page 1. */
  private def movementsView(material: MaterialResponse, recorded: Signal[Option[UUID]]): HtmlElement =
    val today = WarehousePage.today()
    val from = Var("")
    val to = Var("")
    val actionError = Var(Option.empty[ApiError])
    val pendingVoid = Var(Option.empty[MovementResponse])
    val voidReason = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[MovementResponse](pager)(
      // Day, then recording order: the order of the running balance.
      "date" -> Column.by[MovementResponse, (String, Int)](m => (m.on, m.sequence)),
      "kind" -> Column.text(WarehouseViews.movementKindLabel),
      "origin" -> Column.optionalText(m => WarehouseViews.consumptionSource(m).orElse(m.supplier.map(_.name))),
      "quantity" -> Column.decimal(_.delta),
      "unitCost" -> Column.optionalDecimal(_.unitCost),
      "value" -> Column.optionalDecimal(_.value),
      "balance" -> Column.decimal(_.balance),
    )
    // Newest first, so a movement just recorded today shows at the top.
    sort.current.set(Some(SortBy("date", Direction.Descending)))
    val range = from.signal.combineWith(to.signal).map(WarehouseViews.validateOptionalRange).distinct
    // The last valid filter: an invalid one shows a hint and keeps the list of the previous filter.
    val applied = Var((Option.empty[String], Option.empty[String]))
    val movements =
      loadable(AppBus.warehouseTicks.combineWith(applied.signal.distinct))(() =>
        ApiClient.listMovements(material.id, applied.now()._1, applied.now()._2),
      )

    def cancelVoid(): Unit =
      pendingVoid.set(None)
      voidReason.set("")

    def confirmVoid(): Unit =
      val reason = voidReason.now().trim.nn
      pendingVoid.now().filter(_ => reason.nonEmpty).foreach { movement =>
        cancelVoid()
        actionError.set(None)
        ApiClient.voidMovement(material.id, movement.id, reason).foreach {
          case Right(_) => AppBus.mutatedWarehouse()
          case Left(err) => showError(actionError, "Annullo movimento")(err)
        }
      }

    val voidDialog =
      confirmDialog(
        isOpen = pendingVoid.signal.map(_.isDefined),
        titleText = Signal.fromValue("Annulla movimento"),
        message = pendingVoid.signal.map(_.fold("") { m =>
          s"Annullare il movimento “${WarehouseViews.movementKindLabel(m)}” del ${IsoDay.display(m.on)} " +
            s"(${WarehouseViews.signedQty(m.delta, material.unit)})? Resta nello storico, ma non conta più nella giacenza."
        }),
        confirmLabel = "Annulla movimento",
        tone = DialogTone.Danger,
        body = div(cls := "mt-4 text-left", field("Motivo dell'annullo", textInput(voidReason, "Es. carico registrato due volte"))),
        confirmEnabled = voidReason.signal.map(_.trim.nn.nonEmpty),
        onCancel = () => cancelVoid(),
        onConfirm = () => confirmVoid(),
      )

    def balanceBox(label: String, value: String): HtmlElement =
      div(
        cls := "rounded-md border border-slate-200 px-3 py-2",
        div(cls := "text-xs text-slate-500", label),
        div(
          cls := (if WarehouseViews.signOf(value) < 0 then "font-semibold tabular-nums text-rose-700"
                  else "font-semibold tabular-nums text-slate-800"),
          WarehouseFormats.formatQty(value, material.unit),
        ),
      )

    def struckCls(m: MovementResponse): String = if m.voided then "line-through text-slate-400" else ""

    def qtyCls(m: MovementResponse): String =
      if m.voided then ""
      else if WarehouseViews.signOf(m.delta) > 0 then "text-emerald-700"
      else if WarehouseViews.signOf(m.delta) < 0 then "text-rose-700"
      else ""

    def balanceCls(m: MovementResponse): String = if WarehouseViews.signOf(m.balance) < 0 then "text-rose-700" else "text-slate-700"

    def isRecorded(m: MovementResponse): Signal[Boolean] = recorded.map(_.contains(m.id))

    /** The background of a row: grey when voided, green when just recorded on this page. */
    def rowBackground(m: MovementResponse): Modifier[HtmlElement] =
      cls <-- isRecorded(m).map(fresh => if fresh then "bg-emerald-50" else if m.voided then "bg-slate-50" else "")

    /** "Appena registrato" (text, not only the green row) on the movement just recorded. */
    def recordedBadge(m: MovementResponse): Modifier[HtmlElement] =
      child.maybe <-- isRecorded(m).map(fresh =>
        Option.when(fresh)(div(cls := "mt-1", badge("Appena registrato", "bg-emerald-50 text-emerald-700 border-emerald-200"))),
      )

    def originNode(m: MovementResponse): Modifier[HtmlElement] =
      WarehouseViews
        .consumptionSource(m)
        .fold[Modifier[HtmlElement]](
          m.supplier
            .fold[Modifier[HtmlElement]]("—")(s => div(s.name, Option.when(!s.active)(span(cls := "text-xs text-slate-400", " (eliminato)")))),
        )(source => div(cls := "text-slate-700", source))

    /** Reason, note, and the void badge with its reason. */
    def notesNode(m: MovementResponse): Modifier[HtmlElement] =
      val struck = struckCls(m)
      List[Modifier[HtmlElement]](
        m.reason.map(r => div(cls := (if m.voided then struck else "text-slate-600"), r)),
        m.note.map(n => div(cls := (if m.voided then struck else "text-slate-400"), n)),
        Option.when(m.voided)(
          div(
            cls := "mt-1 text-rose-700",
            badge(m.voidedOn.fold("Annullato")(day => s"Annullato il ${IsoDay.display(day)}"), "bg-rose-50 text-rose-700 border-rose-200"),
            m.voidReason.map(r => div(cls := "mt-0.5", r)),
          ),
        ),
      )

    // A consumption follows its order manufacturing (reopening it reverses the consumption), a delivery note purchase its delivery note (edited or
    // cancelled in the "DDT" tab): neither is voided by hand.
    def voidAction(m: MovementResponse, extraCls: String): Modifier[HtmlElement] =
      Option.when(!material.archived && WarehouseViews.isVoidable(m))(
        div(cls := extraCls, roleGated(Role.Admin)(button(tpe := "button", cls := btnDanger, "Annulla", onClick --> (_ => pendingVoid.set(Some(m)))))),
      )

    /**
     * The card of a movement below `lg` (nine columns do not fit a tablet): date · kind and origin on the left, signed quantity and balance on the
     * right, then costs and notes.
     */
    def renderMovementCard(m: MovementResponse): HtmlElement =
      val struck = struckCls(m)
      div(
        cls := "space-y-2 p-3 text-sm",
        rowBackground(m),
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := s"min-w-0 $struck",
            div(
              cls := "font-medium",
              cls := (if m.voided then "" else "text-slate-800"),
              s"${IsoDay.display(m.on)} · ${WarehouseViews.movementKindLabel(m)}",
            ),
            WarehouseViews.deliveryNoteSource(m).map(source => div(cls := "text-xs text-slate-500", source)),
            div(cls := "break-words text-slate-600", originNode(m)),
            m.mpnCode.map(code => div(cls := "break-words text-xs text-slate-400", code)),
            recordedBadge(m),
          ),
          div(
            cls := "shrink-0 text-right",
            div(cls := s"whitespace-nowrap font-medium tabular-nums ${qtyCls(m)} $struck", WarehouseViews.signedQty(m.delta, material.unit)),
            div(
              cls := s"whitespace-nowrap text-xs tabular-nums ${balanceCls(m)}",
              s"Saldo ${WarehouseFormats.formatQty(m.balance, material.unit)}",
            ),
          ),
        ),
        Option.when(m.unitCost.isDefined || m.value.isDefined)(
          div(
            cls := s"text-xs text-slate-500 tabular-nums $struck",
            s"Costo unit. ${m.unitCost.fold("—")(WarehouseFormats.formatUnitPrice)} · Valore ${m.value.fold("—")(WarehouseFormats.formatEuro)}",
          ),
        ),
        div(cls := "break-words text-xs", notesNode(m)),
        // The card only shows below `lg`: touch target.
        voidAction(m, "*:min-h-10"),
      )
    end renderMovementCard

    def renderMovement(m: MovementResponse): HtmlElement =
      val struck = struckCls(m)
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        rowBackground(m),
        td(cls := "px-3 py-2 whitespace-nowrap", span(cls := struck, IsoDay.display(m.on)), recordedBadge(m)),
        td(
          cls := s"px-3 py-2 $struck",
          WarehouseViews.movementKindLabel(m),
          WarehouseViews.deliveryNoteSource(m).map(source => div(cls := "text-xs text-slate-500", source)),
        ),
        td(cls := s"px-3 py-2 wrap-anywhere $struck", originNode(m), m.mpnCode.map(code => div(cls := "text-xs text-slate-400", code))),
        td(
          cls := s"px-3 py-2 whitespace-nowrap text-right font-medium tabular-nums ${qtyCls(m)} $struck",
          WarehouseViews.signedQty(m.delta, material.unit),
        ),
        td(cls := s"px-3 py-2 whitespace-nowrap text-right tabular-nums $struck", m.unitCost.fold("—")(WarehouseFormats.formatUnitPrice)),
        td(cls := s"px-3 py-2 whitespace-nowrap text-right tabular-nums $struck", m.value.fold("—")(WarehouseFormats.formatEuro)),
        td(cls := s"px-3 py-2 whitespace-nowrap text-right tabular-nums ${balanceCls(m)}", WarehouseFormats.formatQty(m.balance, material.unit)),
        td(cls := "px-3 py-2 wrap-anywhere text-xs", notesNode(m)),
        td(cls := "px-3 py-2 text-right", voidAction(m, "")),
      )
    end renderMovement

    def renderList(list: MovementsResponse): HtmlElement =
      div(
        cls := "space-y-3",
        div(
          cls := "grid grid-cols-2 gap-3 sm:max-w-md",
          balanceBox(list.from.fold("Saldo iniziale")(day => s"Saldo prima del ${IsoDay.display(day)}"), list.openingBalance),
          balanceBox(list.to.fold("Saldo finale")(day => s"Saldo al ${IsoDay.display(day)}"), list.closingBalance),
        ),
        Option.when(list.movements.nonEmpty)(
          div(
            cls := "lg:hidden",
            sort.picker(
              List(
                "date" -> "Data",
                "kind" -> "Tipo",
                "origin" -> "Origine",
                "quantity" -> "Quantità",
                "unitCost" -> "Costo unit.",
                "value" -> "Valore",
                "balance" -> "Saldo",
              ),
            ),
          ),
        ),
        if list.movements.isEmpty then emptyState("Nessun movimento nel periodo selezionato.")
        else
          pager.view(sort.sorted(list.movements)) { page =>
            div(
              // Phone and tablet: stacked cards. From lg: the table.
              div(cls := "divide-y divide-slate-100 rounded-md border border-slate-200 lg:hidden", page.map(renderMovementCard)),
              div(
                cls := "hidden overflow-x-auto rounded-md border border-slate-200 lg:block",
                table(
                  cls := "w-full text-sm",
                  thead(
                    cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                    tr(
                      sort.header("date", "Data", "px-3 py-2"),
                      sort.header("kind", "Tipo", "px-3 py-2"),
                      sort.header("origin", "Fornitore / Origine", "px-3 py-2"),
                      sort.header("quantity", "Quantità", "px-3 py-2 text-right"),
                      sort.header("unitCost", "Costo unit.", "px-3 py-2 text-right"),
                      sort.header("value", "Valore", "px-3 py-2 text-right"),
                      sort.header("balance", "Saldo", "px-3 py-2 text-right"),
                      th(cls := "px-3 py-2", "Motivo / Note"),
                      th(cls := "px-3 py-2", span(cls := "sr-only", "Azioni")),
                    ),
                  ),
                  tbody(page.map(renderMovement)),
                ),
              ),
            )
          },
      )

    div(
      cls := "space-y-3",
      range --> {
        case Right(bounds) => applied.set(bounds); pager.reset()
        case Left(_) => ()
      },
      // A movement just recorded is on the first page (newest first).
      recorded.changes --> (_ => pager.reset()),
      div(
        // Phone: "Dal" and "Al" side by side, the reset below them; from sm the three on one line.
        cls := "grid grid-cols-2 items-end gap-3 sm:flex sm:flex-wrap",
        div(cls := "min-w-0 sm:w-40", field("Dal", dateInput(from.signal, from.writer, today))),
        div(cls := "min-w-0 sm:w-40", field("Al", dateInput(to.signal, to.writer, today))),
        button(
          tpe := "button",
          cls := s"$btnGhost col-span-2 mb-0.5 justify-self-start",
          "Tutti i movimenti",
          disabled <-- from.signal.combineWith(to.signal).map((f, t) => f.isEmpty && t.isEmpty),
          onClick --> { _ => from.set(""); to.set("") },
        ),
      ),
      child.maybe <-- range.map(_.left.toOption.map(message => div(cls := "text-xs text-rose-600", message))),
      child.maybe <-- actionError.signal.map(_.map(err => errorBanner(err, () => actionError.set(None)))),
      renderResult(movements)(renderList),
      voidDialog,
    )
  end movementsView
end MaterialSheetView
