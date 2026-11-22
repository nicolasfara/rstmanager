package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, Pagination, Paging, TableSort, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/** What the stock dialog opened from the materials table is about. */
enum StockDialog derives CanEqual:
  case Purchase(material: MaterialResponse)
  case Unload(material: MaterialResponse)
  case Adjust(material: MaterialResponse)
  case Movements(material: MaterialResponse)

  /** The material the dialog is about. */
  def material: MaterialResponse

  /** The key of the movement form kind (see [[StockDialog.movementKinds]]); `None` for the movements list. */
  def movementKind: Option[String] = this match
    case Purchase(_) => Some(StockDialog.purchaseKind)
    case Unload(_) => Some(StockDialog.unloadKind)
    case Adjust(_) => Some(StockDialog.adjustKind)
    case Movements(_) => None
end StockDialog

object StockDialog:
  val purchaseKind = "purchase"
  val unloadKind = "unload"
  val adjustKind = "adjustment"

  /** The kinds of the movement form, as `(key, label)` for its "Carico | Scarico | Rettifica" switch, in that order. */
  val movementKinds: List[(String, String)] = List(purchaseKind -> "Carico", unloadKind -> "Scarico", adjustKind -> "Rettifica")

  /** The movement form of `kind` for `material`; `None` for an unknown kind. */
  def movement(material: MaterialResponse, kind: String): Option[StockDialog] = kind match
    case `purchaseKind` => Some(Purchase(material))
    case `unloadKind` => Some(Unload(material))
    case `adjustKind` => Some(Adjust(material))
    case _ => None

  /** The form the switch moves `dialog` to when `kind` is picked: `None` when `kind` is already its kind (or unknown), so nothing is rebuilt. */
  def switchKind(dialog: StockDialog, kind: String): Option[StockDialog] = movement(dialog.material, kind).filter(_ != dialog)
end StockDialog

/**
 * The stock dialogs of a material: the purchase ("Carico"), unload ("Scarico"), and adjustment ("Rettifica") forms (Operator), and the movements list
 * with the running balance and the void action (Admin). The list also shows the automatic consumptions by order manufacturings ("Scarico
 * lavorazione", and "Storno lavorazione" when a reopened manufacturing gives the material back) with their order, which cannot be voided, and the
 * purchases loaded by a delivery note ("DDT n. X"), corrected only through the delivery note.
 *
 * Driven by one `Var[Option[StockDialog]]` owned by the materials tab: `Some` opens the matching modal, `None` closes it. The three forms share one
 * modal ("Movimento") whose "Carico | Scarico | Rettifica" switch replaces the dialog with the same material's other kind; its title follows the
 * kind. A form is rebuilt on every opening (the state goes through `None` in between) and on every kind switch; a switch hands the shared fields
 * already typed (quantity, cost, day, reason: [[MovementForm.Carry]]) to the new kind's form, an opening never does, so a new opening never shows the
 * values of a previous one. Every successful write calls [[AppBus.mutatedWarehouse]], which refreshes the materials table, the open movements list,
 * and the reports.
 */
object StockDialogs:

  private val disabledInputCls = "disabled:bg-slate-100 disabled:text-slate-400"

  def apply(current: Var[Option[StockDialog]]): HtmlElement =
    val close: () => Unit = () => current.set(None)
    val formDialog: Signal[Option[StockDialog]] = current.signal.map(_.filter {
      case StockDialog.Movements(_) => false
      case _ => true
    })
    val movementsOf: Signal[Option[MaterialResponse]] = current.signal.map(_.collect { case StockDialog.Movements(material) => material })
    // The shared fields of the open form, kept up to date by the form itself.
    val liveCarry = Var(Option.empty[MovementForm.Carry])
    // One-shot hand-over from the switch to the form it opens, keyed by that form and cleared right after the switch: it never reaches an opening.
    val handOver = Var(Option.empty[(StockDialog, MovementForm.Carry)])
    def switchKind(kind: String): Unit =
      current.now().flatMap(StockDialog.switchKind(_, kind)).foreach { next =>
        handOver.set(liveCarry.now().map(next -> _))
        current.set(Some(next))
        handOver.set(None)
      }
    def build(dialog: StockDialog): HtmlElement =
      val carry = handOver.now().collect { case (target, carried) if target == dialog => carried }
      renderForm(dialog, carry, liveCarry.writer.contramap[MovementForm.Carry](Some(_)), close)
    // `contents`: the wrapper must not take a cell of the tab grid; the modals are fixed overlays anyway.
    div(
      cls := "contents",
      modal(formDialog.map(_.isDefined), formDialog.map(_.fold("")(formTitle)), "max-w-lg", close)(
        div(
          // Outside the rebuilt form, so the focus stays on the switch when the kind changes.
          div(
            cls := "mb-4",
            segmented(
              formDialog.map(_.flatMap(_.movementKind).getOrElse("")),
              Observer[String](switchKind),
              StockDialog.movementKinds,
              aria.label := "Tipo di movimento",
            ),
          ),
          child.maybe <-- formDialog.distinct.map(_.map(build)),
        ),
      ),
      modal(movementsOf.map(_.isDefined), movementsOf.map(_.fold("")(p => s"Movimenti — ${p.internalCode}")), "max-w-6xl", close)(
        div(child.maybe <-- movementsOf.distinct.map(_.map(movementsView))),
      ),
    )
  end apply

  private def formTitle(dialog: StockDialog): String = dialog match
    case StockDialog.Purchase(p) => s"Carico — ${p.internalCode}"
    case StockDialog.Unload(p) => s"Scarico — ${p.internalCode}"
    case StockDialog.Adjust(p) => s"Rettifica — ${p.internalCode}"
    case StockDialog.Movements(p) => s"Movimenti — ${p.internalCode}"

  /** The form of `dialog`, prefilled from `carry` (a kind switch) or blank (an opening); it reports its shared fields to `carryOut`. */
  private def renderForm(
      dialog: StockDialog,
      carry: Option[MovementForm.Carry],
      carryOut: Observer[MovementForm.Carry],
      close: () => Unit,
  ): HtmlElement =
    dialog match
      case StockDialog.Purchase(p) => purchaseForm(p, carry, carryOut, close)
      case StockDialog.Unload(p) => unloadForm(p, carry, carryOut, close)
      case StockDialog.Adjust(p) => adjustmentForm(p, carry, carryOut, close)
      case StockDialog.Movements(p) => movementsView(p)

  // ---- Shared pieces -----------------------------------------------------------------------------

  private def materialHeader(material: MaterialResponse): HtmlElement =
    div(
      cls := "mb-4 rounded-md bg-slate-50 px-3 py-2 text-xs text-slate-600",
      span(cls := "font-medium text-slate-800", material.internalCode),
      s" — ${material.description}",
      div(
        cls := "mt-0.5",
        "Giacenza attuale: ",
        span(
          cls := (if material.stock.negative then "font-medium text-rose-700" else "font-medium text-slate-800"),
          WarehouseFormats.formatQty(material.stock.quantity, material.unit),
        ),
      ),
    )

  private def dateInput(valueSignal: Signal[String], writer: Observer[String], today: String): HtmlElement =
    textInput(valueSignal, writer, "", "date").amend(maxAttr := today)

  private def decimalInput(valueSignal: Signal[String], writer: Observer[String], placeholderText: String): HtmlElement =
    textInput(valueSignal, writer, placeholderText).amend(inputMode := "decimal")

  /**
   * The "Annulla" / submit pair of a stock dialog form; the submit button is disabled while `canSubmit` is false or a request is in flight. (The
   * redesigned forms use [[Components.formActions]], whose submit stays enabled.)
   */
  private def stockFormActions(
      submitLabel: String,
      canSubmit: Signal[Boolean],
      submitting: Signal[Boolean],
      close: () => Unit,
      submit: () => Unit,
  ): HtmlElement =
    div(
      cls := "mt-4 flex justify-end gap-2",
      button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => close())),
      button(
        tpe := "button",
        cls := btnPrimary,
        child.text <-- submitting.map(busy => if busy then "Registrazione…" else submitLabel),
        disabled <-- canSubmit.combineWith(submitting).map((ok, busy) => !ok || busy),
        onClick --> (_ => submit()),
      ),
    )

  /** Sends a movement; on success refreshes the warehouse and closes the dialog, on failure shows the server message in the dialog. */
  private def record(
      effect: () => Future[ApiClient.Result[MovementRecordedResponse]],
      submitting: Var[Boolean],
      error: Var[Option[ApiError]],
      context: String,
      close: () => Unit,
  ): Unit =
    if !submitting.now() then
      submitting.set(true)
      error.set(None)
      effect().foreach { result =>
        submitting.set(false)
        result match
          case Right(_) =>
            AppBus.mutatedWarehouse()
            close()
          case Left(err) => showError(error, context)(err)
      }

  private def piecesHint(unit: String): Modifier[HtmlElement] =
    Option.when(WarehouseFormats.MeasureUnit.fromWire(unit).exists(!_.allowsFractions))(hint("Solo quantità intere"))

  // ---- Purchase ----------------------------------------------------------------------------------

  private def purchaseForm(
      material: MaterialResponse,
      carry: Option[MovementForm.Carry],
      carryOut: Observer[MovementForm.Carry],
      close: () => Unit,
  ): HtmlElement =
    val today = WarehousePage.today()
    val fresh = MovementForm.purchase(material, today)
    val state = Var(carry.fold(fresh)(fresh.carried))
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    val unitLabel = WarehouseFormats.unitShortLabel(material.unit)
    val mpnChoices = ("" -> "Seleziona un MPN…") :: state.now().mpns.map(o => o.key -> o.label)
    def submit(): Unit =
      state.now().request.foreach(request => record(() => ApiClient.recordPurchase(material.id, request), submitting, error, "Carico", close))
    div(
      state.signal.map(_.carry) --> carryOut,
      materialHeader(material),
      div(
        cls := "grid gap-3 sm:grid-cols-2",
        div(
          cls := "sm:col-span-2",
          field(
            "MPN acquistato (fornitore)",
            if state.now().mpns.isEmpty then div(cls := "text-xs text-rose-600", "Il materiale non ha MPN: aggiungine uno in anagrafica.")
            else staticSelect(state.signal.map(_.mpnKey), Observer[String](v => state.update(_.copy(mpnKey = v))), mpnChoices),
          ),
        ),
        field(
          s"Quantità ($unitLabel)",
          div(
            decimalInput(state.signal.map(_.quantity), Observer[String](v => state.update(_.copy(quantity = v))), "25,5"),
            piecesHint(material.unit),
          ),
        ),
        field("Costo unitario (€)", decimalInput(state.signal.map(_.unitCost), Observer[String](v => state.update(_.copy(unitCost = v))), "3,50")),
        field("Data", dateInput(state.signal.map(_.on), Observer[String](v => state.update(_.copy(on = v))), today)),
        field("Note", textInput(state.signal.map(_.note), Observer[String](v => state.update(_.copy(note = v))), "Es. DDT 1234 (opzionale)")),
      ),
      div(
        cls := "mt-3 flex items-baseline justify-between rounded-md border border-slate-200 px-3 py-2 text-sm",
        span(cls := "text-slate-500", "Totale carico"),
        span(
          cls := "font-semibold tabular-nums text-slate-800",
          child.text <-- state.signal.map(_.totalPreview.fold("—")(WarehouseFormats.formatEuro)),
        ),
      ),
      errorList(state.signal.map(s => if s.pristine then Nil else s.errors)),
      child.maybe <-- error.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      stockFormActions("Registra carico", state.signal.map(_.request.isDefined), submitting.signal, close, () => submit()),
    )
  end purchaseForm

  // ---- Unload ------------------------------------------------------------------------------------

  private def unloadForm(
      material: MaterialResponse,
      carry: Option[MovementForm.Carry],
      carryOut: Observer[MovementForm.Carry],
      close: () => Unit,
  ): HtmlElement =
    val today = WarehousePage.today()
    val fresh = MovementForm.unload(material, today)
    val state = Var(carry.fold(fresh)(fresh.carried))
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    def submit(): Unit =
      state.now().request.foreach(request => record(() => ApiClient.recordUnload(material.id, request), submitting, error, "Scarico", close))
    div(
      state.signal.map(_.carry) --> carryOut,
      materialHeader(material),
      div(
        cls := "grid gap-3 sm:grid-cols-2",
        field(
          s"Quantità (${WarehouseFormats.unitShortLabel(material.unit)})",
          div(decimalInput(state.signal.map(_.quantity), Observer[String](v => state.update(_.copy(quantity = v))), "2"), piecesHint(material.unit)),
        ),
        field("Data", dateInput(state.signal.map(_.on), Observer[String](v => state.update(_.copy(on = v))), today)),
        div(
          cls := "sm:col-span-2",
          field(
            "Motivo",
            textInput(state.signal.map(_.reason), Observer[String](v => state.update(_.copy(reason = v))), "Es. campionatura per cliente"),
          ),
        ),
      ),
      hint("Lo scarico è rifiutato se la giacenza scenderebbe sotto zero in quella data o in seguito."),
      errorList(state.signal.map(s => if s.pristine then Nil else s.errors)),
      child.maybe <-- error.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      stockFormActions("Registra scarico", state.signal.map(_.request.isDefined), submitting.signal, close, () => submit()),
    )
  end unloadForm

  // ---- Adjustment --------------------------------------------------------------------------------

  private def adjustmentForm(
      material: MaterialResponse,
      carry: Option[MovementForm.Carry],
      carryOut: Observer[MovementForm.Carry],
      close: () => Unit,
  ): HtmlElement =
    val today = WarehousePage.today()
    val fresh = MovementForm.adjustment(material, today)
    val state = Var(carry.fold(fresh)(fresh.carried))
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    def submit(): Unit =
      state.now().request.foreach(request => record(() => ApiClient.recordAdjustment(material.id, request), submitting, error, "Rettifica", close))
    div(
      state.signal.map(_.carry) --> carryOut,
      materialHeader(material),
      div(
        cls := "grid gap-3 sm:grid-cols-2",
        field(
          s"Variazione (${WarehouseFormats.unitShortLabel(material.unit)})",
          div(
            textInput(state.signal.map(_.delta), Observer[String](v => state.update(_.copy(delta = v))), "-2 oppure 10"),
            hint("Negativa per togliere, positiva per aggiungere"),
            piecesHint(material.unit),
          ),
        ),
        field(
          "Costo unitario (€)",
          div(
            decimalInput(state.signal.map(_.unitCost), Observer[String](v => state.update(_.copy(unitCost = v))), "Opzionale")
              .amend(cls := disabledInputCls, disabled <-- state.signal.map(!_.costEnabled)),
            hint("Solo per rettifiche in aumento: vale come prezzo d'acquisto"),
          ),
        ),
        field("Data", dateInput(state.signal.map(_.on), Observer[String](v => state.update(_.copy(on = v))), today)),
        field("Motivo", textInput(state.signal.map(_.reason), Observer[String](v => state.update(_.copy(reason = v))), "Es. inventario")),
      ),
      errorList(state.signal.map(s => if s.pristine then Nil else s.errors)),
      child.maybe <-- error.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      stockFormActions("Registra rettifica", state.signal.map(_.request.isDefined), submitting.signal, close, () => submit()),
    )
  end adjustmentForm

  // ---- Movements ---------------------------------------------------------------------------------

  private def movementsView(material: MaterialResponse): HtmlElement =
    val today = WarehousePage.today()
    val from = Var("")
    val to = Var("")
    val actionError = Var(Option.empty[ApiError])
    val pendingVoid = Var(Option.empty[MovementResponse])
    val voidReason = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[MovementResponse](pager)(
      "date" -> Column.text(_.on),
      "kind" -> Column.text(WarehouseViews.movementKindLabel),
      "origin" -> Column.optionalText(m => WarehouseViews.consumptionSource(m).orElse(m.supplier.map(_.name))),
      "quantity" -> Column.decimal(_.delta),
      "unitCost" -> Column.optionalDecimal(_.unitCost),
      "value" -> Column.optionalDecimal(_.value),
      "balance" -> Column.decimal(_.balance),
    )
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
     * The card of a movement below `lg` (nine columns do not fit a tablet-wide dialog): date · kind and origin on the left, signed quantity and
     * balance on the right, then costs and notes.
     */
    def renderMovementCard(m: MovementResponse): HtmlElement =
      val struck = struckCls(m)
      div(
        cls := "space-y-2 p-3 text-sm",
        cls := (if m.voided then "bg-slate-50" else ""),
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
        cls := (if m.voided then "bg-slate-50" else ""),
        td(cls := s"px-3 py-2 whitespace-nowrap $struck", IsoDay.display(m.on)),
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
                      th(cls := "px-3 py-2"),
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
      child.maybe <-- actionError.signal.map(_.map(errorBanner)),
      renderResult(movements)(renderList),
      voidDialog,
    )
  end movementsView
end StockDialogs
