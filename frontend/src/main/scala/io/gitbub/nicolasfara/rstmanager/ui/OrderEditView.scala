package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.OrderDrafts.{ IssueTarget, MfgField, MfgSnapshot, TaskField, TaskState }
import io.gitbub.nicolasfara.rstmanager.ui.OrderEditDraft.{ Change, EditField, EditOp, HeaderPart, MfgPart, MfgRow, TaskPart, TaskRow }

/**
 * The "Modifica ordine" and "Dettaglio ordine" pages: the same sections, cards and sidebar as "Nuovo ordine" ([[OrderCreateView]]).
 *
 * Edit splits the operations by kind, and says so in the UI. Values (header fields, manufacturing and task fields, materials) are a page draft
 * ([[OrderEditDraft]]) saved by one "Salva modifiche", which sends only what changed. Structure (add or remove a manufacturing or a task) is applied
 * at once by an explicit button or after a confirmation; its response is merged into the draft ([[OrderEditDraft.rebase]]), so unsaved values
 * survive. Detail shows the same page read-only.
 */
object OrderEditView:

  /** The banner of a failed save. */
  val submitErrorId = "oe-submit-error"

  /** The "+ Da catalogo" button of the edit page. */
  val addMfgId = "oe-add-mfg"

  private val newIds = OrderMfgDraftCard.Ids("oe-new")

  /** Manufacturing lifecycle statuses selectable when editing: backend value -> label. */
  private val mfgStatusOptions =
    List("not_started" -> "Non iniziata", "in_progress" -> "In corso", "paused" -> "In pausa", "completed" -> "Completata")

  private def panelId(mfgId: UUID): String = s"oe-mfg-$mfgId-panel"
  private def addTaskId(mfgId: UUID): String = s"oe-mfg-$mfgId-add-task"
  private def newTaskIds(mfgId: UUID): OrderMfgDraftCard.TaskRowIds =
    OrderMfgDraftCard.TaskRowIds(s"oe-mfg-$mfgId-new-task", s"oe-mfg-$mfgId-new-employee", s"oe-mfg-$mfgId-new-duration")

  /** `java.util.UUID.randomUUID` is unavailable on Scala.js (needs SecureRandom); use the Web Crypto API. */
  private def randomUuid(): UUID = UUID.fromString(js.Dynamic.global.crypto.randomUUID().toString).nn

  /** A removal from the saved order awaiting confirmation: a whole manufacturing, or one task of it (scheduled task id, template task id). */
  private final case class PendingRemoval(mfgId: UUID, mfgCode: String, task: Option[(UUID, UUID)])

  /** The inline "+ Task" row open on manufacturing `mfgId`, with its own touch tracker. */
  private final case class AddingTask(mfgId: UUID, draft: TaskDraft, touch: FormTouch)

  /** The outcome of a sequential save: the last order received, the failure that stopped it, and whether some material lines were saved. */
  private final case class SaveRun(last: Option[OrderResponse], failure: Option[ApiError], materialsSaved: Boolean)

  private def execute(orderId: UUID, op: EditOp): Future[ApiClient.Result[OrderResponse]] = op match
    case EditOp.UpdateOrder(request) => ApiClient.updateOrder(orderId, request)
    case EditOp.UpdateMfg(mfgId, request) => ApiClient.updateManufacturing(orderId, mfgId, request)
    case EditOp.SetMfgEmployee(mfgId, employeeId) => ApiClient.setPreferredEmployee(orderId, mfgId, employeeId)
    case EditOp.UpdateTask(mfgId, taskId, request) => ApiClient.updateScheduledTask(orderId, mfgId, taskId, request)
    case EditOp.SetTaskEmployee(mfgId, taskId, employeeId) => ApiClient.setTaskPreferredEmployee(orderId, mfgId, taskId, employeeId)
    case EditOp.OrderDependencies(request) => ApiClient.updateOrderDependencies(orderId, request)
    case EditOp.TaskDependencies(mfgId, request) => ApiClient.updateTaskDependencies(orderId, mfgId, request)
    case EditOp.Materials(mfgId, lines) => ApiClient.setManufacturingMaterials(orderId, mfgId, lines)

  private def isMaterials(op: EditOp): Boolean = op match
    case EditOp.Materials(_, _) => true
    case _ => false

  /** Runs the operations one after another, stopping at the first failure (the API is not atomic). */
  private def runSequential(orderId: UUID, ops: List[EditOp]): Future[SaveRun] =
    ops.foldLeft(Future.successful(SaveRun(None, None, false))) { (acc, op) =>
      acc.flatMap { run =>
        if run.failure.isDefined then Future.successful(run)
        else
          execute(orderId, op).map {
            case Right(order) => run.copy(last = Some(order), materialsSaved = run.materialsSaved || isMaterials(op))
            case Left(err) => run.copy(failure = Some(err))
          }
      }
    }

  /** «Ordine consegnato: in sola lettura. …» for a status that is not editable, «Accesso in sola lettura.» for a Viewer. */
  private def readOnlyReason(order: OrderResponse): String =
    if OrderStatus.isEditable(order.status) then "Accesso in sola lettura."
    else s"Ordine ${OrderStatus.label(order.status).toLowerCase}: in sola lettura. Solo gli ordini in corso o sospesi sono modificabili."

  private def mfgTitle(m: ManufacturingResponse): String = m.description.filter(_.trim.nn.nonEmpty).fold(m.code)(d => s"${m.code} · $d")

  private def mfgTotals(m: ManufacturingResponse): OrderDrafts.MfgTotals =
    OrderDrafts.MfgTotals(m.tasks.size, m.tasks.map(_.expectedMinutes).sum, m.materialsCost)

  private val dirtyDot: HtmlElement =
    span(cls := "text-xs font-medium text-amber-700", span(aria.hidden := true, "● "), "non salvata")

  /** The page of `initial`: editable (`Edit`) or read-only (`Detail`); `onExit` goes back to the orders list. */
  def apply(deps: OrderFormView.Deps, initial: OrderResponse, editable: Boolean, onExit: () => Unit): HtmlElement =
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1
      keyCounter

    val orderId = initial.id
    val number = initial.number
    val draft = Var(OrderEditDraft.fromOrder(initial, () => nextKey()))
    // One request at a time: the save, or a structural operation. `saving` is the save itself (its busy label).
    val busy = Var(false)
    val saving = Var(false)
    val submitError = Var(Option.empty[ApiError])
    val touch = FormTouch()
    val leaveOpen = Var(false)
    val pendingRemoval = Var(Option.empty[PendingRemoval])
    // Errors of the structural operations, shown in their card: "s-<id>" for a saved manufacturing, "n-<key>" for a new one.
    val cardErrors = Var(Map.empty[String, ApiError])
    // One card open at a time, saved or new.
    val savedOpen = Var(Option.when(initial.manufacturings.size == 1)(initial.manufacturings.map(_.id)).flatMap(_.headOption))
    val newOpen = Var(Option.empty[Int])
    val newSeen = Var(Set.empty[Int])
    val newMfgs = Var(List.empty[MfgDraft])
    val removingNew = Var(Option.empty[Int])
    val addingTask = Var(Option.empty[AddingTask])
    // Materials contexts: per saved manufacturing (written by its editor), and the one of the new drafts.
    val materialContexts = Var(Map.empty[UUID, MaterialDrafts.Context])
    val newMaterialsContext = Var(MaterialDrafts.Context.empty)
    // Synchronous mirrors of the loaded catalogs, read by event handlers.
    val catalogs = Var(List.empty[ManufacturingCatalogResponse])
    val tasksById = Var(Map.empty[String, TaskResponse])
    val cardTouches = mutable.Map.empty[Int, FormTouch]

    // Sticky for this opening (see DraftGuard.trackGone).
    val goneFlag = Var(false)
    val gone: Signal[Boolean] = goneFlag.signal

    def catalogById(rawId: String): Option[ManufacturingCatalogResponse] = OrderMfgDraftCard.catalogLookup(catalogs.now())(rawId)

    val taskNames: Signal[UUID => String] =
      deps.tasks.map(list => list.map(t => t.id -> t.name).toMap).map(names => id => names.getOrElse(id, Formats.shortId(id)))

    def taskNameNow(id: UUID): String = tasksById.now().get(id.toString).fold(Formats.shortId(id))(_.name)

    def contextOf(contexts: Map[UUID, MaterialDrafts.Context], rows: List[MfgRow])(mfgId: UUID): MaterialDrafts.Context =
      contexts.getOrElse(mfgId, MaterialDrafts.Context.of(Nil, rows.find(_.id == mfgId).fold(Nil)(_.server.materials)))

    val changes: Signal[List[Change]] =
      draft.signal.combineWith(taskNames).map((d, names) => OrderEditDraft.changes(d, names)).distinct

    val issues: Signal[List[OrderEditDraft.Issue]] =
      draft.signal
        .combineWith(taskNames, materialContexts.signal)
        .map((d, names, contexts) => OrderEditDraft.issues(d, names, contextOf(contexts, d.mfgs)))
        .distinct

    def currentIssues(): List[OrderEditDraft.Issue] =
      val d = draft.now()
      OrderEditDraft.issues(d, taskNameNow, contextOf(materialContexts.now(), d.mfgs))

    def rawFor(field: EditField): Signal[Option[String]] =
      issues.map(_.collectFirst { case issue if issue.field == field => issue.message }).distinct

    def spec(labelText: String, field: EditField, required: Boolean): FieldSpec =
      touch.spec(labelText, OrderEditDraft.domId(field), required, rawFor(field))

    def applyResponse(order: OrderResponse): Unit = draft.update(d => OrderEditDraft.rebase(d, order, () => nextKey()))

    def snapshotsOfNew(): List[MfgSnapshot] = newMfgs.now().map(OrderMfgDraftCard.snapshotOf)

    // ---- Stock warnings ----------------------------------------------------------------------------

    val newSnapshots: Signal[List[MfgSnapshot]] = OrderMfgDraftCard.snapshots(newMfgs.signal)

    // Stock of the materials of the order, read without the order itself (its lines, edited and new, are added back), loaded once a material line
    // exists and refreshed on order and warehouse changes.
    val newRows: Signal[List[MaterialDrafts.Row]] = newSnapshots.map(_.flatMap(_.materials))
    val hasMaterials =
      draft.signal.combineWith(newRows).map((d, rows) => (OrderEditDraft.demandRows(d) ++ rows).exists(row => !MaterialDrafts.isBlank(row))).distinct
    val requirements: Signal[List[MaterialRequirementResponse]] =
      hasMaterials.flatMapSwitch { any =>
        if any then
          loadable(AppBus.ordersTicks.combineWith(AppBus.warehouseTicks))(() => ApiClient.materialRequirements(Some(orderId))).map {
            case Some(Right(response)) => response.materials
            case _ => Nil
          }
        else Signal.fromValue(Nil)
      }
    val stockWarnings: Signal[Map[UUID, OrderDrafts.StockWarning]] =
      draft.signal.combineWith(newRows, requirements).map(OrderEditDraft.stockWarnings).distinct
    val stockWarningList: Signal[List[OrderDrafts.StockWarning]] = stockWarnings.map(_.values.toList.sortBy(_.internalCode))
    val stockFootnote = "Avviso non bloccante: le modifiche si possono salvare comunque."

    def isDirty(): Boolean =
      editable && !goneFlag.now() &&
        (OrderEditDraft.isDirty(draft.now()) ||
          snapshotsOfNew().exists(s => !OrderCreateModel.isBlankDraft(s)) ||
          addingTask.now().exists(_.draft.state.now().taskId.trim.nn.nonEmpty))

    def requestLeave(): Unit = if isDirty() then leaveOpen.set(true) else onExit()

    // ---- Navigation between cards and fields -------------------------------------------------------

    def openSaved(mfgId: UUID): Unit =
      newOpen.now().foreach(key => newSeen.update(_ + key))
      newOpen.set(None)
      savedOpen.set(Some(mfgId))

    def toggleSaved(mfgId: UUID): Unit = if savedOpen.now().contains(mfgId) then savedOpen.set(None) else openSaved(mfgId)

    def openNew(key: Int): Unit =
      newOpen.now().filter(_ != key).foreach(previous => newSeen.update(_ + previous))
      savedOpen.set(None)
      newOpen.set(Some(key))

    def toggleNew(key: Int): Unit =
      if newOpen.now().contains(key) then
        newSeen.update(_ + key)
        newOpen.set(None)
      else openNew(key)

    /** Opens the card of `field` and focuses it. */
    def reveal(field: EditField): Unit =
      field.card.foreach(openSaved)
      FormFocus.focus(OrderEditDraft.domId(field))

    def reportCard(key: String, context: String)(err: ApiError): Unit =
      ErrorCenter.report(context, err)
      cardErrors.update(_.updated(key, err))

    def cardError(key: String): Modifier[HtmlElement] =
      child.maybe <-- cardErrors.signal.map(_.get(key)).distinct.map(_.map(err => errorBanner(err, () => cardErrors.update(_ - key))))

    /** Disabled while a request runs or the order no longer exists. */
    val structuralDisabled: Signal[Boolean] = busy.signal.combineWith(gone).map(_ || _)

    // ---- Save --------------------------------------------------------------------------------------

    def save(): Unit =
      if !busy.now() && !goneFlag.now() then
        currentIssues() match
          case first :: _ =>
            touch.attemptSubmit()
            reveal(first.field)
          case Nil =>
            val d = draft.now()
            val ops = OrderEditDraft.effects(d, contextOf(materialContexts.now(), d.mfgs))
            // Nothing to save: leave as "Annulla" would (a new manufacturing not yet added still asks first).
            if ops.isEmpty then requestLeave()
            else
              busy.set(true)
              saving.set(true)
              submitError.set(None)
              runSequential(orderId, ops).foreach { run =>
                busy.set(false)
                saving.set(false)
                AppBus.mutatedOrders()
                if run.materialsSaved then AppBus.mutatedWarehouse()
                run.failure match
                  case None =>
                    Notifications.success(s"Ordine $number salvato")
                    onExit()
                  case Some(err) =>
                    run.last.foreach(applyResponse)
                    showError(submitError, "Salvataggio ordine")(
                      err.copy(message = s"Salvataggio interrotto: ${err.message}. Le modifiche ancora da salvare sono elencate nel riepilogo."),
                    )
                    FormFocus.focus(submitErrorId)
              }
            end if
    end save

    // ---- Structural operations ---------------------------------------------------------------------

    /** Runs one immediate structural request: on success merges the response into the draft, reports the mutation and runs `onDone`. */
    def structural(errorKey: String, context: String)(request: => Future[ApiClient.Result[OrderResponse]])(onDone: OrderResponse => Unit): Unit =
      if !busy.now() && !goneFlag.now() then
        busy.set(true)
        cardErrors.update(_ - errorKey)
        request.foreach { result =>
          busy.set(false)
          result match
            case Right(order) =>
              val before = draft.now().order
              applyResponse(order)
              AppBus.mutatedOrders()
              onDone(before)
            case Left(err) => reportCard(errorKey, context)(err)
        }

    def openAddTask(mfgId: UUID): Unit =
      addingTask.set(Some(AddingTask(mfgId, OrderMfgDraftCard.newTask(() => nextKey()), FormTouch())))
      FormFocus.focus(newTaskIds(mfgId).task)

    def closeAddTask(mfgId: UUID): Unit =
      addingTask.set(None)
      FormFocus.focus(addTaskId(mfgId))

    def submitAddTask(adding: AddingTask): Unit =
      val state = adding.draft.state.now()
      val present = draft.now().mfgs.find(_.id == adding.mfgId).fold(Set.empty[UUID])(_.tasks.map(_.templateTaskId).toSet)
      OrderEditDraft.newTaskIssues(state, present) match
        case (field, _) :: _ =>
          adding.touch.attemptSubmit()
          val ids = newTaskIds(adding.mfgId)
          FormFocus.focus(if field == TaskField.Task then ids.task else ids.duration)
        case Nil =>
          OrderEditDraft.addTaskRequest(state).foreach { request =>
            structural(s"s-${adding.mfgId}", "Aggiunta task")(ApiClient.addManufacturingTask(orderId, adding.mfgId, request)) { _ =>
              val code = draft.now().mfgs.find(_.id == adding.mfgId).fold("")(_.code)
              Notifications.success(s"Task «${taskNameNow(request.taskId)}» aggiunto a $code")
              closeAddTask(adding.mfgId)
            }
          }
    end submitAddTask

    def confirmRemoval(removal: PendingRemoval): Unit =
      removal.task match
        case Some((scheduledId, templateId)) =>
          structural(s"s-${removal.mfgId}", "Rimozione task")(ApiClient.removeManufacturingTask(orderId, removal.mfgId, scheduledId)) { _ =>
            Notifications.success(s"Task «${taskNameNow(templateId)}» rimosso da ${removal.mfgCode}")
            FormFocus.focus(addTaskId(removal.mfgId))
          }
        case None =>
          structural(s"s-${removal.mfgId}", "Rimozione lavorazione")(ApiClient.removeManufacturing(orderId, removal.mfgId)) { _ =>
            if savedOpen.now().contains(removal.mfgId) then savedOpen.set(None)
            Notifications.success(s"Lavorazione ${removal.mfgCode} rimossa dall'ordine")
            FormFocus.focus(addMfgId)
          }

    // ---- New manufacturings ------------------------------------------------------------------------

    def touchOf(key: Int): FormTouch = cardTouches.getOrElseUpdate(key, FormTouch())

    val savedCount: Signal[Int] = draft.signal.map(_.mfgs.size).distinct

    val newIssues: Signal[List[OrderDrafts.Issue]] =
      newSnapshots
        .combineWith(catalogs.signal, newMaterialsContext.signal, savedCount)
        .map { (list, templates, context, offset) =>
          list.zipWithIndex
            .flatMap((s, index) => OrderDrafts.manufacturingIssues(s, offset + index, OrderMfgDraftCard.catalogLookup(templates), context))
        }
        .distinct

    def newRawAt(target: Signal[IssueTarget]): Signal[Option[String]] =
      newIssues
        .combineWith(target)
        .map((list, at) => list.collectFirst { case issue if issue.target == at => OrderCreateModel.inlineMessage(issue.message) })
        .distinct

    def newTaskKeyAt(key: Int, index: Int): Option[Int] = newMfgs.now().find(_.key == key).flatMap(_.tasks.now().lift(index)).map(_.key)

    def addNew(mode: String): Unit =
      val m = OrderMfgDraftCard.newDraft(mode, () => nextKey())
      newMfgs.update(_ :+ m)
      openNew(m.key)
      FormFocus.focus(newIds.mfgField(m.key, if mode == "catalog" then MfgField.Catalog else MfgField.Code))

    def removeNew(key: Int): Unit =
      newMfgs.update(_.filterNot(_.key == key))
      if newOpen.now().contains(key) then newOpen.set(None)
      cardErrors.update(_ - s"n-$key")
      FormFocus.focus(addMfgId)

    def requestRemoveNew(key: Int): Unit =
      newMfgs.now().find(_.key == key).foreach { m =>
        if OrderCreateModel.isBlankDraft(OrderMfgDraftCard.snapshotOf(m)) then removeNew(key) else removingNew.set(Some(key))
      }

    /** The work deadline a new manufacturing without its own date falls back to: the header one (typed, else saved), else the delivery day. */
    def fallbackDeadline(): String =
      val d = draft.now()
      List(d.header.deadline, d.baseline.deadline, DayText.dayOf(d.order.deliveryDate)).map(_.trim.nn).find(_.nonEmpty).getOrElse("")

    /** "Aggiungi all'ordine": validates only this card, then adds it; on success the draft turns into the saved card. */
    def addToOrder(m: MfgDraft): Unit =
      val index = newMfgs.now().indexWhere(_.key == m.key)
      val snapshot = OrderMfgDraftCard.snapshotOf(m)
      OrderDrafts.manufacturingIssues(snapshot, draft.now().mfgs.size + index, catalogById, newMaterialsContext.now()) match
        case first :: _ =>
          touchOf(m.key).attemptSubmit()
          openNew(m.key)
          FormFocus.focus(newIds.domId(first.target, newTaskKeyAt, _ => addMfgId, addMfgId))
        case Nil =>
          OrderDrafts.manufacturingFromDraft(snapshot, fallbackDeadline(), catalogById, newMaterialsContext.now(), () => randomUuid()) match
            case Left(err) => reportCard(s"n-${m.key}", "Aggiunta lavorazione")(err)
            case Right(dto) =>
              structural(s"n-${m.key}", "Aggiunta lavorazione")(ApiClient.addManufacturing(orderId, dto)) { before =>
                newMfgs.update(_.filterNot(_.key == m.key))
                if dto.materials.isDefined then AppBus.mutatedWarehouse()
                OrderEditDraft.addedManufacturings(before, draft.now().order).headOption.foreach { added =>
                  openSaved(added.id)
                  Notifications.success(s"Lavorazione ${added.code} aggiunta all'ordine")
                  FormFocus.focus(panelId(added.id))
                }
              }
    end addToOrder

    val cardContext = OrderMfgDraftCard.Context(
      newIds,
      touchOf,
      newRawAt,
      key => newIssues.map(_.count(issue => OrderCreateModel.mfgKeyOf(issue.target).contains(key))).distinct,
      catalogs.signal,
      catalogById,
      deps.tasks,
      id => tasksById.now().get(id),
      deps.employeeOptions,
      draft.signal.map(d => if d.header.deadline.trim.nn.nonEmpty then d.header.deadline else d.baseline.deadline),
      newMaterialsContext.signal,
      newMaterialsContext.writer,
      stockWarnings,
      () => nextKey(),
      newSnapshots,
      savedCount,
      false,
      newOpen.signal,
      newSeen.signal,
      toggleNew,
    )

    def renderNew(m: MfgDraft): HtmlElement =
      OrderMfgDraftCard(
        cardContext,
        m,
        emptyMod,
        div(
          cls := "space-y-3 border-t border-slate-100 pt-3",
          cardError(s"n-${m.key}"),
          div(
            cls := "flex flex-col-reverse gap-2 sm:flex-row sm:items-center sm:justify-end",
            p(cls := "text-xs text-slate-500 sm:mr-auto", "La lavorazione viene aggiunta subito all'ordine salvato."),
            button(tpe := "button", cls := s"$btnGhost min-h-10 justify-center md:min-h-0", "Annulla", onClick --> (_ => requestRemoveNew(m.key))),
            button(
              tpe := "button",
              cls := s"$btnPrimary min-h-10 justify-center md:min-h-0",
              disabled <-- structuralDisabled,
              child.text <-- busy.signal.map(isBusy => if isBusy then "Aggiunta…" else "Aggiungi all'ordine"),
              onClick --> (_ => addToOrder(m)),
            ),
          ),
        ),
      )

    // ---- Saved manufacturing card ------------------------------------------------------------------

    val employeeLabel: Signal[String => String] =
      deps.employeeOptions.map(list => id => list.collectFirst { case (value, labelText) if value == id => labelText }.getOrElse("—"))

    def renderTaskRow(
        mfgId: UUID,
        initialRow: TaskRow,
        taskSignal: Signal[TaskRow],
        siblings: Signal[List[TaskRow]],
        onlyTask: Signal[Boolean],
    ): HtmlElement =
      val taskId = initialRow.id
      val templateId = initialRow.templateTaskId
      def field(part: TaskPart): EditField = EditField.Task(mfgId, taskId, part)
      def set(change: OrderEditDraft.TaskValues => OrderEditDraft.TaskValues): Unit =
        draft.update(d => OrderEditDraft.updateTask(d, mfgId, taskId)(change))
      val dependencyChoices: Signal[List[(String, String)]] =
        siblings.combineWith(taskNames).map { (rows, names) =>
          rows
            .filter(row => row.id != taskId && row.templateTaskId != templateId)
            .map(row => row.templateTaskId.toString -> names(row.templateTaskId))
            .distinctBy(_._1)
        }
      val code = draft.now().mfgs.find(_.id == mfgId).fold("")(_.code)
      div(
        cls := "space-y-2 rounded-md border border-slate-200 bg-slate-50/60 p-3",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "text-sm font-medium text-slate-800", child.text <-- taskNames.map(_(templateId))),
            div(cls := "text-xs text-slate-500", child.text <-- taskSignal.map(row => OrderStatus.label(row.server.status))),
          ),
          button(
            tpe := "button",
            cls := s"$btnDanger min-h-10 shrink-0 justify-center md:min-h-0",
            aria.label <-- taskNames.map(names => s"Rimuovi il task ${names(templateId)}"),
            title := "Rimuovi task",
            disabled <-- onlyTask.combineWith(structuralDisabled).map(_ || _),
            "✕",
            onClick --> (_ => pendingRemoval.set(Some(PendingRemoval(mfgId, code, Some((taskId, templateId)))))),
          ),
        ),
        div(
          cls := "grid grid-cols-2 gap-3 sm:grid-cols-[minmax(0,1fr)_7rem_7rem] sm:items-start",
          div(
            cls := "col-span-2 sm:col-span-1",
            Components.field(FieldSpec("Dipendente", OrderEditDraft.domId(field(TaskPart.Employee))))(bind =>
              selectInput(taskSignal.map(_.current.employeeId), Observer[String](v => set(_.copy(employeeId = v))), deps.employeeOptions)
                .amend(bind),
            ),
          ),
          Components.field(spec("Previste (h:mm)", field(TaskPart.Expected), true))(bind =>
            textInput(taskSignal.map(_.current.expected), Observer[String](v => set(_.copy(expected = v))), "1:35").amend(bind),
          ),
          Components.field(spec("Fatte (h:mm)", field(TaskPart.Completed), true))(bind =>
            textInput(taskSignal.map(_.current.completed), Observer[String](v => set(_.copy(completed = v))), "1:35").amend(bind),
          ),
        ),
        div(
          idAttr := OrderEditDraft.domId(field(TaskPart.Dependencies)),
          checkboxChips(
            "Dipende da",
            dependencyChoices,
            taskSignal.map(_.current.dependsOn),
            (id, checked) => set(s => s.copy(dependsOn = if checked then s.dependsOn + id else s.dependsOn - id)),
          ),
        ),
      )
    end renderTaskRow

    def addTaskRow(adding: AddingTask): HtmlElement =
      val ids = newTaskIds(adding.mfgId)
      val raw = adding.draft.state.signal.combineWith(draft.signal).map { (state, d) =>
        OrderEditDraft.newTaskIssues(state, d.mfgs.find(_.id == adding.mfgId).fold(Set.empty[UUID])(_.tasks.map(_.templateTaskId).toSet))
      }
      def rawOf(field: TaskField): Signal[Option[String]] = raw.map(_.collectFirst { case (f, message) if f == field => message }).distinct
      div(
        cls := "space-y-3 rounded-md border border-sky-200 bg-sky-50/40 p-3",
        subheading("Nuovo task"),
        div(
          cls := "grid grid-cols-1 gap-3 sm:grid-cols-[minmax(0,1fr)_12rem_7rem] sm:items-start",
          OrderMfgDraftCard.taskInputs(
            adding.draft,
            ids,
            adding.touch,
            rawOf(TaskField.Task),
            rawOf(TaskField.Duration),
            cardContext.taskOptions,
            deps.employeeOptions,
            id => tasksById.now().get(id),
            () => draft.now().mfgs.find(_.id == adding.mfgId).fold(1)(_.quantity),
          ),
        ),
        div(
          cls := "flex flex-col-reverse gap-2 sm:flex-row sm:items-center sm:justify-end",
          p(cls := "text-xs text-slate-500 sm:mr-auto", "Il task viene aggiunto subito all'ordine salvato."),
          button(
            tpe := "button",
            cls := s"$btnGhost min-h-10 justify-center md:min-h-0",
            "Annulla",
            onClick --> (_ => closeAddTask(adding.mfgId)),
          ),
          button(
            tpe := "button",
            cls := s"$btnSmall min-h-10 justify-center md:min-h-0",
            disabled <-- structuralDisabled,
            child.text <-- busy.signal.map(isBusy => if isBusy then "Aggiunta…" else "Aggiungi task"),
            onClick --> (_ => submitAddTask(adding)),
          ),
        ),
      )
    end addTaskRow

    def savedSummary(rowSignal: Signal[MfgRow], position: Signal[Int], hasIssues: Signal[Boolean]): HtmlElement =
      div(
        cls := "flex flex-wrap items-center gap-x-3 gap-y-1",
        span(cls := "text-sm font-semibold tabular-nums text-slate-400", child.text <-- position.map(n => s"#$n")),
        span(cls := "min-w-0 truncate text-sm font-medium text-slate-800", child.text <-- rowSignal.map(row => mfgTitle(row.server))),
        child.maybe <-- rowSignal.map(_.quantity).distinct.map(OrderFormView.quantityBadge),
        span(cls := "text-xs tabular-nums text-slate-500", child.text <-- rowSignal.map(row => OrderCreateModel.totalsText(mfgTotals(row.server)))),
        span(
          cls := "text-xs text-slate-500",
          child.text <-- rowSignal.map(row => DayText.short(row.current.deadline)).map(day => if day.isEmpty then "" else s"entro $day"),
        ),
        child <-- rowSignal.map(_.server.status).distinct.map(statusBadge),
        child.maybe <-- rowSignal.map(OrderEditDraft.isMfgDirty).distinct.map(Option.when(_)(dirtyDot)),
        child.maybe <-- hasIssues.map(Option.when(_)(statusIcon(1))),
      )

    def renderSaved(mfgId: UUID, rowSignal: Signal[MfgRow]): HtmlElement =
      def field(part: MfgPart): EditField = EditField.Mfg(mfgId, part)
      def set(change: OrderEditDraft.MfgValues => OrderEditDraft.MfgValues): Unit =
        draft.update(d => OrderEditDraft.updateMfg(d, mfgId)(change))
      val position = draft.signal.map(_.mfgs.indexWhere(_.id == mfgId) + 1).distinct
      val hasIssues = issues
        .map(_.exists(_.field.card.contains(mfgId)))
        .combineWith(touch.submittedSignal)
        .map(_ && _)
        .distinct
      val tasksSignal = rowSignal.map(_.tasks)
      val onlyTask = tasksSignal.map(_.size <= 1).distinct
      val dependencyChoices: Signal[List[(String, String)]] =
        draft.signal.map(_.mfgs.filter(_.id != mfgId).map(other => other.id.toString -> other.code)).distinct
      val codeNow = () => draft.now().mfgs.find(_.id == mfgId).fold("")(_.code)
      accordionCard(
        panelId(mfgId),
        savedOpen.signal.map(_.contains(mfgId)),
        () => toggleSaved(mfgId),
        savedSummary(rowSignal, position, hasIssues),
        emptyMod,
        hasIssues,
      )(
        div(
          cls := "space-y-5",
          div(
            cls := OrderFormView.headerGridCls,
            Components.field(FieldSpec("Descrizione", OrderEditDraft.domId(field(MfgPart.Description))))(bind =>
              textInput(rowSignal.map(_.current.description), Observer[String](v => set(_.copy(description = v))), "Opzionale").amend(bind),
            ),
            Components.field(spec("Deadline lavorazione", field(MfgPart.Deadline), true))(bind =>
              textInput(rowSignal.map(_.current.deadline), Observer[String](v => set(_.copy(deadline = v))), "", "date").amend(bind),
            ),
            OrderMfgDraftCard.labelled(
              "Stato",
              segmented(
                rowSignal.map(_.current.status),
                Observer[String](v => set(_.copy(status = v))),
                mfgStatusOptions,
                List(aria.label := "Stato lavorazione", idAttr := OrderEditDraft.domId(field(MfgPart.Status))),
              ),
            ),
            Components.field(FieldSpec("Dipendente preferito", OrderEditDraft.domId(field(MfgPart.Employee))))(bind =>
              selectInput(rowSignal.map(_.current.employeeId), Observer[String](v => set(_.copy(employeeId = v))), deps.employeeOptions)
                .amend(bind),
            ),
          ),
          div(
            idAttr := OrderEditDraft.domId(field(MfgPart.Dependencies)),
            checkboxChips(
              "Dipende da",
              dependencyChoices,
              rowSignal.map(_.current.dependsOn),
              (id, checked) => set(s => s.copy(dependsOn = if checked then s.dependsOn + id else s.dependsOn - id)),
            ),
          ),
          div(
            cls := "space-y-2",
            subheading("Task"),
            div(
              cls := "space-y-2",
              children <-- tasksSignal.split(_.id)((_, initialRow, taskSignal) => renderTaskRow(mfgId, initialRow, taskSignal, tasksSignal, onlyTask)),
            ),
            child.maybe <-- onlyTask.map(Option.when(_)(hint("Una lavorazione deve avere almeno un task: per toglierlo aggiungine prima un altro."))),
            child <-- addingTask.signal.map(_.filter(_.mfgId == mfgId)).distinctByFn((a, b) => a.map(_.draft.key) == b.map(_.draft.key)).map {
              case Some(adding) => addTaskRow(adding)
              case None =>
                button(
                  idAttr := addTaskId(mfgId),
                  tpe := "button",
                  cls := s"$btnSmall min-h-10 md:min-h-0",
                  disabled <-- gone,
                  "+ Task",
                  onClick --> (_ => openAddTask(mfgId)),
                )
            },
          ),
          MaterialsEditor.savedEditor(
            rowSignal.map(_.server.materials).distinct,
            rowSignal.map(_.materials),
            change => draft.update(d => OrderEditDraft.updateMaterials(d, mfgId)(change)),
            () => nextKey(),
            Observer[MaterialDrafts.Context](context => materialContexts.update(_.updated(mfgId, context))),
            OrderEditDraft.domId(field(MfgPart.Materials)),
            rowSignal.map(_.server.status == "completed"),
            // A completed manufacturing has already unloaded its materials: no warning.
            rowSignal.map(_.needsStock).distinct.flatMapSwitch(needs => if needs then stockWarnings else Signal.fromValue(Map.empty)),
          ),
          cardError(s"s-$mfgId"),
          div(
            cls := "flex flex-col gap-2 border-t border-slate-100 pt-3 sm:flex-row sm:items-center sm:justify-end",
            child <-- draft.signal.map(_.mfgs.size > 1).distinct.map { removable =>
              if removable then
                button(
                  tpe := "button",
                  cls := s"$btnDanger min-h-10 justify-center md:min-h-0",
                  disabled <-- structuralDisabled,
                  "Rimuovi lavorazione",
                  onClick --> (_ => pendingRemoval.set(Some(PendingRemoval(mfgId, codeNow(), None)))),
                )
              else p(cls := "text-xs text-slate-500", "L'ordine deve avere almeno una lavorazione.")
            },
          ),
        ),
      )
    end renderSaved

    // ---- Read-only (Detail) ------------------------------------------------------------------------

    def renderDetailMfg(m: ManufacturingResponse, index: Int, order: OrderResponse): HtmlElement =
      val dependsOn = order.dependencies
        .find(_.manufacturingId == m.id)
        .fold(List.empty[UUID])(_.dependsOn)
        .flatMap(id => order.manufacturings.find(_.id == id).map(_.code))
      accordionCard(
        panelId(m.id),
        savedOpen.signal.map(_.contains(m.id)),
        () => toggleSaved(m.id),
        div(
          cls := "flex flex-wrap items-center gap-x-3 gap-y-1",
          span(cls := "text-sm font-semibold tabular-nums text-slate-400", s"#${index + 1}"),
          span(cls := "min-w-0 truncate text-sm font-medium text-slate-800", mfgTitle(m)),
          OrderFormView.quantityBadge(m.quantity),
          span(cls := "text-xs tabular-nums text-slate-500", OrderCreateModel.totalsText(mfgTotals(m))),
          span(cls := "text-xs text-slate-500", s"entro ${DayText.short(DayText.dayOf(m.completionDate))}"),
          statusBadge(m.status),
        ),
        emptyMod,
        Signal.fromValue(false),
      )(
        div(
          cls := "space-y-5",
          div(
            cls := OrderFormView.headerGridCls,
            readField("Descrizione", orDash(m.description.getOrElse(""))),
            readField("Deadline lavorazione", Formats.date(m.completionDate)),
            readField("Stato", statusBadge(m.status)),
            readField("Dipendente preferito", child.text <-- employeeLabel.map(_(m.preferredEmployeeId.fold("")(_.toString)))),
            readField("Dipende da", if dependsOn.isEmpty then "—" else dependsOn.mkString(", ")),
          ),
          div(
            cls := "space-y-2",
            subheading("Task"),
            div(
              cls := "divide-y divide-slate-100 rounded-md border border-slate-200",
              m.tasks.map { t =>
                div(
                  cls := "flex flex-col gap-1 px-3 py-2 text-sm sm:flex-row sm:items-center sm:justify-between",
                  div(cls := "text-slate-700", child.text <-- taskNames.map(_(t.taskId))),
                  div(
                    cls := "text-xs text-slate-500",
                    child.text <-- employeeLabel.map { names =>
                      val employee = t.preferredEmployeeId.fold("")(id => s" · ${names(id.toString)}")
                      s"${Formats.duration(t.expectedMinutes)}h previste · ${Formats.duration(t.completedMinutes.getOrElse(0))}h fatte · ${OrderStatus
                          .label(t.status)}$employee"
                    },
                  ),
                )
              },
            ),
          ),
          MaterialsEditor.linesView(m),
        ),
      )
    end renderDetailMfg

    // ---- Sections ----------------------------------------------------------------------------------

    val headerIssueCount = issues.map(_.count(_.field.card.isEmpty))
    val mfgIssueCount = issues.map(_.count(_.field.card.isDefined))

    def countStatus(count: Signal[Int]): Signal[SectionStatus] =
      if !editable then Signal.fromValue(SectionStatus.Neutral)
      else
        count.combineWith(touch.submittedSignal).map((n, attempted) => if n > 0 && attempted then SectionStatus.Pending(n) else SectionStatus.Neutral)

    val customerName = OrderFormView.customerName(deps, Signal.fromValue(initial.customerId.toString))
    val deliveryDay = DayText.dayOf(initial.deliveryDate)

    val fixedFields = List(
      readField("Numero ordine", number),
      readField("Cliente", child.text <-- customerName),
      readField("Data creazione", Formats.date(initial.creationDate)),
      readField("Consegna cliente", Formats.date(initial.deliveryDate)),
    )

    def setHeader(change: OrderEditDraft.HeaderValues => OrderEditDraft.HeaderValues): Unit =
      draft.update(d => OrderEditDraft.updateHeader(d)(change))

    val headerSection =
      if editable then
        val deadline = draft.signal.map(_.header.deadline)
        formSection(1, "Testata", "Priorità, deadline e descrizione dell'ordine.", countStatus(headerIssueCount))(
          div(
            cls := OrderFormView.headerGridCls,
            fixedFields,
            p(cls := "text-xs text-slate-500 sm:col-span-2", "Non modificabili dopo la creazione."),
          ),
          div(
            cls := s"${OrderFormView.headerGridCls} border-t border-slate-100 pt-4",
            OrderFormView.deadlineField(
              touch.spec(
                "Deadline fine lavorazione",
                OrderEditDraft.domId(EditField.Header(HeaderPart.Deadline)),
                true,
                s"Proposta: ${OrderDrafts.deadlineLeadDays} giorni prima della consegna",
                rawFor(EditField.Header(HeaderPart.Deadline)),
              ),
              bind => textInput(deadline, Observer[String](v => setHeader(_.copy(deadline = v))), "", "date").amend(bind),
              deadline.map(day => day.trim.nn != OrderDrafts.proposedDeadline(deliveryDay)),
              () => setHeader(_.copy(deadline = OrderDrafts.proposedDeadline(deliveryDay))),
              deadline.map(day => day.trim.nn.nonEmpty && day.trim.nn > deliveryDay),
            ),
            OrderFormView.priorityField(
              draft.signal.map(_.header.priority),
              Observer[String](v => setHeader(_.copy(priority = v))),
              OrderEditDraft.domId(EditField.Header(HeaderPart.Priority)),
            ),
            OrderFormView.descriptionField(
              draft.signal.map(_.header.description),
              Observer[String](v => setHeader(_.copy(description = v))),
              OrderEditDraft.domId(EditField.Header(HeaderPart.Description)),
            ),
          ),
        )
      else
        formSection(1, "Testata", "Dati dell'ordine.", countStatus(headerIssueCount))(
          div(
            cls := OrderFormView.headerGridCls,
            fixedFields,
            readField("Deadline fine lavorazione", initial.promisedDeliveryDate.fold("—")(Formats.date)),
            readField("Priorità", OrderFormView.priorityBadge(initial.priority)),
            readField("Costo materiali", WarehouseFormats.formatEuro(initial.materialsCost)),
            div(cls := "sm:col-span-2", readField("Descrizione ordine", orDash(initial.description.getOrElse("")))),
          ),
        )

    val addButtons =
      div(
        cls := "flex flex-wrap items-center gap-2",
        button(
          idAttr := addMfgId,
          tpe := "button",
          cls := s"$btnGhost min-h-10 md:min-h-0",
          disabled <-- gone,
          "+ Da catalogo",
          onClick --> (_ => addNew("catalog")),
        ),
        button(tpe := "button", cls := s"$btnGhost min-h-10 md:min-h-0", disabled <-- gone, "+ Personalizzata", onClick --> (_ => addNew("custom"))),
      )

    val mfgsSection =
      if editable then
        formSection(
          2,
          "Lavorazioni",
          "Apri una scheda per modificarla. Le modifiche ai valori si salvano con «Salva modifiche»; aggiunte e rimozioni si applicano subito.",
          countStatus(mfgIssueCount),
        )(
          div(cls := "space-y-3", children <-- draft.signal.map(_.mfgs).split(_.id)((id, _, rowSignal) => renderSaved(id, rowSignal))),
          div(cls := "space-y-3 empty:hidden", children <-- newMfgs.signal.split(_.key)((_, initialDraft, _) => renderNew(initialDraft))),
          addButtons,
        )
      else
        formSection(2, "Lavorazioni", "Apri una scheda per vederne task e materiali.", countStatus(mfgIssueCount))(
          div(cls := "space-y-3", initial.manufacturings.zipWithIndex.map((m, index) => renderDetailMfg(m, index, initial))),
        )

    val recap =
      div(
        child <-- draft.signal.map(_.order).distinct.map { order =>
          OrderFormView.recapList(
            order.manufacturings.map(m =>
              OrderFormView.RecapItem(
                mfgTitle(m),
                OrderCreateModel.totalsText(mfgTotals(m)),
                DayText.short(DayText.dayOf(m.completionDate)),
                m.quantity,
              ),
            ),
          )
        },
        OrderFormView.recapTotal(draft.signal.map { d =>
          val each = d.order.manufacturings.map(mfgTotals)
          OrderCreateModel.totalsText(OrderDrafts.MfgTotals(each.map(_.tasks).sum, each.map(_.minutes).sum, d.order.materialsCost))
        }),
      )

    val emphasized: Signal[OrderEditDraft.Issue => Boolean] = touch.submittedSignal.map(attempted => _ => attempted)

    def changesBlock: HtmlElement =
      div(
        cls := "space-y-2",
        div(
          cls := subheadingCls,
          child.text <-- changes.map(list => if list.isEmpty then "Modifiche da salvare" else s"Modifiche da salvare (${list.size})"),
        ),
        changeList(changes, _.label, change => reveal(change.field)),
      )

    def issuesBlock: Modifier[HtmlElement] =
      child.maybe <-- issues.map(_.nonEmpty).distinct.map { any =>
        Option.when(any)(
          div(
            cls := "space-y-2",
            div(cls := subheadingCls, child.text <-- issues.map(list => s"Da correggere (${list.size})")),
            issueList(issues, _.text, emphasized, issue => reveal(issue.field)),
          ),
        )
      }

    val confirmSection =
      if editable then
        formSection(3, "Riepilogo e conferma", "Controlla le modifiche e salvale.", countStatus(issues.map(_.size)))(
          recap,
          child.maybe <-- stockWarningList.map(list => OrderFormView.stockWarningsBox(list, stockFootnote).map(box => div(cls := "mt-3", box))),
          div(cls := "space-y-4 lg:hidden", changesBlock, issuesBlock),
          child.maybe <-- submitError.signal.map(_.map(err => div(idAttr := submitErrorId, errorBanner(err)))),
          div(
            cls := "flex justify-end border-t border-slate-100 pt-3 lg:hidden",
            button(tpe := "button", cls := s"$btnGhost min-h-10 justify-center", "Annulla", onClick --> (_ => requestLeave())),
          ),
        )
      else formSection(3, "Riepilogo", "Lavorazioni e totali dell'ordine.", countStatus(Signal.fromValue(0)))(recap)

    // ---- Sidebar & mobile bar ----------------------------------------------------------------------

    val summary = OrderFormView.Summary(
      Signal.fromValue(number),
      customerName,
      Signal.fromValue(orDash(DayText.long(deliveryDay))),
      draft.signal.map(d => orDash(DayText.long(d.header.deadline))),
      draft.signal.map(d => OrderFormView.priorityLabel(d.header.priority)),
      draft.signal.map(_.order.manufacturings.size),
      draft.signal.map(_.order.manufacturings.flatMap(_.tasks).map(_.expectedMinutes).sum),
      draft.signal.map(_.order.materialsCost),
    )

    val goneReason =
      s"L'ordine «$number» non esiste più: potrebbe essere stato eliminato da un altro utente. Le modifiche non possono essere salvate."

    val blocked: Signal[Option[String]] =
      gone.combineWith(busy.signal, saving.signal).map { (isGone, isBusy, isSaving) =>
        if isGone then Some("L'ordine non esiste più.")
        else Option.when(isBusy && !isSaving)("Attendi il completamento dell'operazione in corso.")
      }

    val sidebar =
      if editable then
        OrderFormView.sidebar(summary)(
          child.maybe <-- stockWarningList.map(list =>
            OrderFormView.stockWarningsBox(list, stockFootnote).map(box => div(cls := "border-t border-slate-100 pt-3", box)),
          ),
          div(cls := "space-y-4 border-t border-slate-100 pt-3", changesBlock, issuesBlock),
          formActions("Salva modifiche", "Salvataggio…", saving.signal, blocked, Some(() => requestLeave()), () => save()),
        )
      else OrderFormView.sidebar(summary)()

    val mobileBar =
      mobileSubmitBar(
        child <-- changes.map(_.headOption).combineWith(changes.map(_.size)).distinct.map { (first, count) =>
          first match
            case None => span(cls := "text-slate-500", "Nessuna modifica")
            case Some(change) =>
              button(
                tpe := "button",
                cls := "min-h-10 truncate text-left font-medium text-amber-700 underline-offset-2 hover:underline",
                aria.label := s"$count modifiche da salvare: vai alla prima",
                if count == 1 then "● 1 modifica" else s"● $count modifiche",
                onClick --> (_ => reveal(change.field)),
              )
        },
        issueBadge(issues.map(_.size).distinct, () => currentIssues().headOption.foreach(issue => reveal(issue.field))),
        "Salva modifiche",
        "Salvataggio…",
        saving.signal,
        saving.signal.combineWith(blocked).map((isSaving, reason) => isSaving || reason.isDefined),
        () => save(),
      )

    // ---- Dialogs -----------------------------------------------------------------------------------

    val leaveDialog =
      confirmDialog(
        leaveOpen.signal,
        Signal.fromValue(DraftGuard.discardTitle(Some(number), "")),
        Signal.fromValue("Le modifiche non salvate andranno perse. Le lavorazioni e i task già aggiunti o rimossi restano applicati all'ordine."),
        "Abbandona",
        "Continua a modificare",
        DialogTone.Danger,
        () => leaveOpen.set(false),
        () =>
          leaveOpen.set(false)
          onExit(),
      )

    val removalDialog =
      confirmDialog(
        pendingRemoval.signal.map(_.isDefined),
        pendingRemoval.signal.combineWith(taskNames).map {
          case (Some(PendingRemoval(_, code, Some((_, templateId)))), names) => s"Rimuovere il task «${names(templateId)}» da «$code»?"
          case (Some(removal), _) => s"Rimuovere la lavorazione «${removal.mfgCode}»?"
          case (None, _) => ""
        },
        Signal.fromValue("La rimozione viene applicata subito all'ordine salvato e non è reversibile."),
        "Rimuovi",
        DialogTone.Danger,
        () => pendingRemoval.set(None),
        () =>
          val removal = pendingRemoval.now()
          pendingRemoval.set(None)
          removal.foreach(confirmRemoval),
      )

    val discardNewDialog =
      confirmDialog(
        removingNew.signal.map(_.isDefined),
        removingNew.signal.combineWith(newSnapshots, catalogs.signal).map { (pending, list, templates) =>
          val label = pending
            .flatMap(key => list.find(_.key == key))
            .map(s => OrderCreateModel.mfgLabel(s.core, OrderMfgDraftCard.catalogLookup(templates)))
            .getOrElse("")
          if label.isEmpty then "Scartare la nuova lavorazione?" else s"Scartare la nuova lavorazione «$label»?"
        },
        Signal.fromValue("La lavorazione non è stata aggiunta all'ordine: i dati inseriti andranno persi."),
        "Scarta",
        "Continua a modificare",
        DialogTone.Danger,
        () => removingNew.set(None),
        () =>
          val pending = removingNew.now()
          removingNew.set(None)
          pending.foreach(removeNew),
      )

    val main = div(cls := "space-y-6", headerSection, mfgsSection, confirmSection)
    val notes: Modifier[HtmlElement] =
      if editable then child.maybe <-- gone.map(Option.when(_)(warningNote(goneReason)))
      else infoNote(readOnlyReason(initial))

    formPage(
      PageHeader(
        OrderFormView.backLabel,
        () => requestLeave(),
        Signal.fromValue(s"${if editable then "Modifica" else "Dettaglio"} ordine $number"),
        context = List("Ordini", number),
        status = List(
          child <-- draft.signal.map(_.order.status).distinct.map(statusBadge),
          child <-- draft.signal.map(_.order.priority).distinct.map(OrderFormView.priorityBadge),
        ),
        notes = notes,
        requiredLegend = editable,
        initialFocus = PageFocus.Title,
      ),
    )(
      DraftGuard.trackGone(deps.knownOrderIds.map(_.exists(ids => !ids.contains(orderId))), goneFlag)(() => ()),
      deps.catalogs --> catalogs.writer,
      deps.tasks --> (list => tasksById.set(list.map(task => task.id.toString -> task).toMap)),
      if editable then
        List(
          NavigationGuard.guard(DraftGuard.discardTitle(Some(number), ""), "Le modifiche non salvate all'ordine andranno perse.", () => isDirty()),
          summaryLayout(main, sidebar, mobileBar),
          leaveDialog,
          removalDialog,
          discardNewDialog,
        )
      else List(summaryLayout(main, sidebar)),
    )
  end apply
end OrderEditView
