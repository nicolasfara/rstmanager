package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.{ AuthService, Role }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/**
 * Orders: filterable list with lifecycle transitions, the full-page create form ([[OrderCreateView]]), and a full edit modal covering order data
 * (priority, work deadline, description), per-manufacturing description/deadline/status, add/remove of manufacturings and tasks, and the dependency
 * graphs (between manufacturings and between the tasks of a manufacturing). Completing or delivering an order asks for an explicit acknowledgement
 * first.
 */
object OrdersPage:

  private val priorityOptions = List("normal" -> "Normale", "urgent" -> "Urgente")

  private val manufacturingModeOptions = List("catalog" -> "Da catalogo", "custom" -> "Personalizzata")

  /** Manufacturing lifecycle statuses selectable when editing: backend value -> label. */
  private val mfgStatusOptions =
    List("not_started" -> "Non iniziata", "in_progress" -> "In corso", "paused" -> "In pausa", "completed" -> "Completata")

  /** Status filter chips: filter value -> label. An empty value means "all". */
  private val statusFilters = List(
    "" -> "Tutti",
    "in_progress" -> "In corso",
    "suspended" -> "Sospesi",
    "completed" -> "Completati",
    "delivered" -> "Consegnati",
    "cancelled" -> "Annullati",
  )

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** `java.util.UUID.randomUUID` is unavailable on Scala.js (needs SecureRandom); use the Web Crypto API. */
  private def randomUuid(): UUID = UUID.fromString(js.Dynamic.global.crypto.randomUUID().toString).nn

  /** Trims free text and collapses blank strings to `None` (an empty value clears the field). */
  private def normalizeStr(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  private val chipBase = "rounded-full px-3 py-1 text-xs font-medium border transition-colors"
  private def chipCls(active: Boolean): String =
    if active then s"$chipBase bg-slate-900 text-white border-slate-900"
    else s"$chipBase bg-white text-slate-600 border-slate-300 hover:bg-slate-100"

  private def priorityBadge(priority: String): HtmlElement = priority match
    case "urgent" => badge("Urgente", "bg-rose-50 text-rose-700 border-rose-200")
    case _ => badge("Normale", "bg-slate-50 text-slate-600 border-slate-200")

  /** State for the inline add-manufacturing form. `taskEmployees` mirrors the create-form per-task employee overrides for catalog mode. */
  private case class AddMfgState(
      mode: String,
      catalogId: String,
      code: String,
      date: String,
      description: String,
      taskId: String,
      hours: String,
      employee: String,
      taskEmployees: Map[String, String],
  )
  private object AddMfgState:
    val empty: AddMfgState = AddMfgState("custom", "", "", "", "", "", "8:00", "", Map.empty)

  /** State for the inline add-task form fields. */
  private case class AddTaskState(taskId: String, hours: String, employee: String)

  /** Editable header fields for the edit modal (priority, promised delivery date, description). */
  private case class EditHeaderState(priority: String, promised: String, description: String) derives CanEqual

  /** Consolidated mutable state for one editable scheduled-task row inside the edit modal. */
  private case class TaskEditState(expected: String, completed: String, dependsOn: Set[String], employee: String) derives CanEqual

  /** One editable scheduled-task row. `dependsOn` holds template-task id strings. */
  private final case class TaskEditRow(
      manufacturingId: UUID,
      taskId: UUID,
      templateTaskId: UUID,
      tracker: DirtyTracker[TaskEditState],
  )

  /** Consolidated mutable state for one editable manufacturing row inside the edit modal. */
  private case class MfgEditState(description: String, completionDate: String, status: String, dependsOn: Set[String], employee: String)
      derives CanEqual

  /** One editable manufacturing row inside the edit modal. `dependsOn` holds manufacturing id strings. */
  private final case class MfgEditRow(
      id: UUID,
      tracker: DirtyTracker[MfgEditState],
  )

  /** What the page shows: the list (with the edit modal), or the full-page create form in its place. */
  private enum View derives CanEqual:
    case List, Create

  /** A lifecycle transition awaiting an explicit user acknowledgement. */
  private final case class ConfirmData(orderId: UUID, action: String, title: String, message: String)

  /** A removal from a saved order awaiting confirmation: a whole manufacturing, or one task of it (`task` = scheduled task id, template task id). */
  private final case class PendingRemoval(orderId: UUID, mfgId: UUID, mfgCode: String, task: Option[(UUID, UUID)])

  def apply(): HtmlElement =
    val pageError = Var(Option.empty[ApiError])
    val view = Var(View.List)
    val statusFilter = Var("")
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val ordersData = loadable(AppBus.ordersTicks)(() => ApiClient.listOrders())
    val customersData = loadable(AppBus.customersTicks)(() => ApiClient.listCustomers())
    val tasksData = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())
    val manufacturingCatalogData = loadable(AppBus.manufacturingsTicks)(() => ApiClient.listManufacturingCatalog())
    val employeesData = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val manufacturingCatalogs = Var(List.empty[ManufacturingCatalogResponse])
    // Synchronous view of each catalog task's default employee, used to propose it when a task is picked in custom mode.
    val taskDefaultEmployees = Var(Map.empty[String, UUID])
    def proposedEmployee(taskId: String): String = taskDefaultEmployees.now().get(taskId).map(_.toString).getOrElse("")

    val customersMap: Signal[Map[UUID, String]] = customersData.map {
      case Some(Right(list)) => list.map(c => c.id -> c.businessName.getOrElse(s"${c.name} ${c.surname}")).toMap
      case _ => Map.empty
    }
    val tasksMap: Signal[Map[UUID, String]] = tasksData.map {
      case Some(Right(list)) => list.map(t => t.id -> t.name).toMap
      case _ => Map.empty
    }
    val employeesMap: Signal[Map[UUID, String]] = employeesData.map {
      case Some(Right(list)) => list.map(e => e.id -> s"${e.name} ${e.surname}").toMap
      case _ => Map.empty
    }
    val customerOptions: Signal[List[(String, String)]] = customersData.map {
      case Some(Right(list)) => list.map(c => c.id.toString -> c.businessName.getOrElse(s"${c.name} ${c.surname}"))
      case _ => Nil
    }
    val taskOptions: Signal[List[(String, String)]] = tasksData.map {
      case Some(Right(list)) => ("" -> "— task —") :: list.map(t => t.id.toString -> s"${t.name} (${Formats.duration(t.requiredMinutes)}h)")
      case _ => List("" -> "—")
    }
    val manufacturingCatalogOptions: Signal[List[(String, String)]] = manufacturingCatalogData.map {
      case Some(Right(list)) =>
        ("" -> "— lavorazione —") :: list.map(m => m.id.toString -> s"${m.code} · ${m.name} (${Formats.duration(m.totalRequiredMinutes)}h)")
      case _ => List("" -> "—")
    }
    val employeeOptions: Signal[List[(String, String)]] = employeesData.map {
      case Some(Right(list)) => ("" -> "— auto (pianificazione sceglie) —") :: list.map(e => e.id.toString -> s"${e.name} ${e.surname}")
      case _ => List("" -> "—")
    }

    def catalogById(rawId: String): Option[ManufacturingCatalogResponse] =
      parseUuid(rawId).flatMap(id => manufacturingCatalogs.now().find(_.id == id))

    /** Per-task employee prefill for a catalog template: the defaults configured on the catalog manufacturing. */
    def defaultTaskEmployees(rawCatalogId: String): Map[String, String] =
      catalogById(rawCatalogId).map(_.defaultEmployees.map(d => d.taskId.toString -> d.employeeId.toString).toMap).getOrElse(Map.empty)

    // The create form gets the shared loaded data; mounted only while creating, so each opening starts from a fresh draft.
    val createDeps = OrderCreateView.Deps(
      customerOptions,
      tasksData.map {
        case Some(Right(list)) => list
        case _ => Nil
      },
      manufacturingCatalogs.signal,
      employeeOptions,
      ordersData.map {
        case Some(Right(list)) => list.map(_.number)
        case _ => Nil
      },
    )

    // ---- Edit modal state ------------------------------------------------------------------------
    val editing = Var(Option.empty[OrderResponse])
    val editHeader = Var(EditHeaderState("normal", "", ""))
    val editHeaderTracker = Var(DirtyTracker(EditHeaderState("normal", "", ""), editHeader))
    val editTasks = Var(List.empty[TaskEditRow])
    val editMfgs = Var(List.empty[MfgEditRow])
    val editError = Var(Option.empty[ApiError])

    // Inline "add manufacturing" form
    val showAddMfg = Var(false)
    val addMfgState = Var(AddMfgState.empty)

    def resetAddMfg(): Unit =
      showAddMfg.set(false)
      addMfgState.set(AddMfgState.empty)

    // Inline "add task" form (targets the manufacturing whose id is held here)
    val addTaskMfgId = Var(Option.empty[UUID])
    val addTaskState = Var(AddTaskState("", "8:00", ""))

    def resetAddTask(): Unit =
      addTaskMfgId.set(None)
      addTaskState.set(AddTaskState("", "8:00", ""))

    def openEdit(order: OrderResponse): Unit =
      val orderDepsByMfg: Map[UUID, Set[String]] =
        order.dependencies.map(d => d.manufacturingId -> d.dependsOn.map(_.toString).toSet).toMap
      val headerState = EditHeaderState(
        priority = order.priority,
        promised = order.promisedDeliveryDate.map(DayText.dayOf).getOrElse(""),
        description = order.description.getOrElse(""),
      )
      editHeader.set(headerState)
      editHeaderTracker.set(DirtyTracker(headerState, editHeader))
      editTasks.set(order.manufacturings.flatMap { m =>
        val taskDepsByTemplate: Map[UUID, Set[String]] =
          m.dependencies.map(d => d.taskId -> d.dependsOn.map(_.toString).toSet).toMap
        m.tasks.map { t =>
          val completed = t.completedMinutes.getOrElse(0)
          val deps = taskDepsByTemplate.getOrElse(t.taskId, Set.empty)
          val state =
            TaskEditState(Formats.duration(t.expectedMinutes), Formats.duration(completed), deps, t.preferredEmployeeId.map(_.toString).getOrElse(""))
          TaskEditRow(m.id, t.id, t.taskId, DirtyTracker(state, Var(state)))
        }
      })
      editMfgs.set(order.manufacturings.map { m =>
        val desc = m.description.getOrElse("")
        val completionDate = DayText.dayOf(m.completionDate)
        val deps = orderDepsByMfg.getOrElse(m.id, Set.empty)
        val state = MfgEditState(desc, completionDate, m.status, deps, m.preferredEmployeeId.map(_.toString).getOrElse(""))
        MfgEditRow(m.id, DirtyTracker(state, Var(state)))
      })
      editError.set(None)
      resetAddMfg()
      resetAddTask()
      editing.set(Some(order))
    end openEdit

    /** Runs the update effects one after another, stopping at the first failure (mirrors the backend). */
    def runSequential(effects: List[() => Future[ApiClient.Result[Unit]]]): Future[ApiClient.Result[Unit]] =
      effects.foldLeft(Future.successful[ApiClient.Result[Unit]](Right(()))) { (acc, effect) =>
        acc.flatMap {
          case Left(err) => Future.successful(Left(err))
          case Right(_) => effect()
        }
      }

    /** Applies an immediate structural change; on success refreshes the modal with the returned order and the table. */
    def applyStructural(effect: => Future[ApiClient.Result[OrderResponse]]): Unit =
      effect.foreach {
        case Right(updated) => openEdit(updated); AppBus.mutatedOrders()
        case Left(err) => showError(editError, "Modifica ordine")(err)
      }

    def saveEdit(): Unit = editing.now().foreach { order =>
      val h = editHeader.now()
      val headerTracker = editHeaderTracker.now()
      val priorityChanged = headerTracker.changed(_.priority)
      val promisedChanged = h.promised.nonEmpty && headerTracker.changed(_.promised)
      val descriptionChanged = headerTracker.changed(_.description.trim.nn)
      val orderUpdate: List[() => Future[ApiClient.Result[Unit]]] =
        if priorityChanged || promisedChanged || descriptionChanged then
          val request = OrderUpdateRequest(
            if priorityChanged then Some(h.priority) else None,
            if promisedChanged then Some(Formats.toIso(h.promised)) else None,
            if descriptionChanged then Some(h.description) else None,
          )
          List(() => ApiClient.updateOrder(order.id, request).map(_.map(_ => ())))
        else Nil

      val mfgUpdates: List[() => Future[ApiClient.Result[Unit]]] = editMfgs.now().flatMap { row =>
        val es = row.tracker.current.now()
        val descChanged = row.tracker.changed(_.description.trim.nn)
        val completionDateChanged = es.completionDate.nonEmpty && row.tracker.changed(_.completionDate)
        val statusChanged = row.tracker.changed(_.status)
        if descChanged || completionDateChanged || statusChanged then
          val request = ManufacturingUpdateRequest(
            if descChanged then Some(es.description) else None,
            if completionDateChanged then Some(Formats.toIso(es.completionDate)) else None,
            if statusChanged then Some(es.status) else None,
            None,
          )
          List(() => ApiClient.updateManufacturing(order.id, row.id, request).map(_.map(_ => ())))
        else Nil
      }

      // The preferred employee travels on its own endpoint (both levels), so changed selections become dedicated effects.
      val mfgEmployeeUpdates: List[() => Future[ApiClient.Result[Unit]]] = editMfgs.now().flatMap { row =>
        if row.tracker.changed(_.employee) then
          val employeeId = parseUuid(row.tracker.current.now().employee)
          List(() => ApiClient.setPreferredEmployee(order.id, row.id, employeeId).map(_.map(_ => ())))
        else Nil
      }

      val taskUpdates: List[() => Future[ApiClient.Result[Unit]]] = editTasks.now().flatMap { row =>
        val es = row.tracker.current.now()
        val newExpected = Formats.parseDuration(es.expected)
        val newCompleted = Formats.parseDuration(es.completed)
        val expectedChanged = newExpected.exists(_ => row.tracker.changed(s => Formats.parseDuration(s.expected)))
        val completedChanged = newCompleted.exists(_ => row.tracker.changed(s => Formats.parseDuration(s.completed)))
        if expectedChanged || completedChanged then
          val request = TaskProgressUpdateRequest(if completedChanged then newCompleted else None, if expectedChanged then newExpected else None)
          List(() => ApiClient.updateScheduledTask(order.id, row.manufacturingId, row.taskId, request).map(_.map(_ => ())))
        else Nil
      }

      val taskEmployeeUpdates: List[() => Future[ApiClient.Result[Unit]]] = editTasks.now().flatMap { row =>
        if row.tracker.changed(_.employee) then
          val employeeId = parseUuid(row.tracker.current.now().employee)
          List(() => ApiClient.setTaskPreferredEmployee(order.id, row.manufacturingId, row.taskId, employeeId).map(_.map(_ => ())))
        else Nil
      }

      // Any change to a "depends on" selection replaces the whole graph, rebuilt from every row.
      val orderDepsUpdate: List[() => Future[ApiClient.Result[Unit]]] =
        if editMfgs.now().exists(_.tracker.changed(_.dependsOn)) then
          val request = OrderDependenciesUpdateRequest(
            editMfgs.now().flatMap { row =>
              val dependsOn = row.tracker.current.now().dependsOn.toList.flatMap(parseUuid).filter(_ != row.id)
              if dependsOn.isEmpty then None else Some(ManufacturingDependencyDto(row.id, dependsOn))
            },
          )
          List(() => ApiClient.updateOrderDependencies(order.id, request).map(_.map(_ => ())))
        else Nil

      val taskDepsUpdates: List[() => Future[ApiClient.Result[Unit]]] =
        editTasks.now().groupBy(_.manufacturingId).toList.flatMap { case (mfgId, rows) =>
          if rows.exists(_.tracker.changed(_.dependsOn)) then
            val request = TaskDependenciesUpdateRequest(
              rows.flatMap { row =>
                val dependsOn = row.tracker.current.now().dependsOn.toList.flatMap(parseUuid).filter(_ != row.templateTaskId)
                if dependsOn.isEmpty then None else Some(TaskDependencyDto(row.templateTaskId, dependsOn))
              }
                .distinctBy(_.taskId),
            )
            List(() => ApiClient.updateTaskDependencies(order.id, mfgId, request).map(_.map(_ => ())))
          else Nil
        }

      val effects = orderUpdate ++ mfgUpdates ++ mfgEmployeeUpdates ++ taskUpdates ++ taskEmployeeUpdates ++ orderDepsUpdate ++ taskDepsUpdates
      if effects.isEmpty then editing.set(None)
      else
        runSequential(effects).foreach {
          case Right(_) => editing.set(None); AppBus.mutatedOrders()
          case Left(err) => showError(editError, "Salvataggio ordine")(err)
        }
    }

    def submitAddMfg(): Unit = editing.now().foreach { order =>
      val s = addMfgState.now()
      if s.mode == "catalog" then
        catalogById(s.catalogId) match
          case None => showError(editError, "Modifica ordine")(ApiError("invalid-form", "Seleziona una lavorazione a catalogo valida.", Nil))
          case Some(template) =>
            applyStructural(
              ApiClient.addManufacturing(order.id, OrderDrafts.fromCatalog(template, s.date, s.employee, s.taskEmployees, () => randomUuid())),
            )
      else
        parseUuid(s.taskId) match
          case None => showError(editError, "Modifica ordine")(ApiError("invalid-form", "Seleziona un task valido per la nuova lavorazione.", Nil))
          case Some(taskId) =>
            val task = ScheduledTaskDto(randomUuid(), taskId, "pending", Formats.parseDuration(s.hours).getOrElse(0), Some(0), None)
            val dto = ManufacturingDto(
              s.code.trim.nn,
              Formats.toIso(s.date),
              "not_started",
              List(task),
              Nil,
              None,
              None,
              None,
              None,
              normalizeStr(s.description),
              parseUuid(s.employee),
            )
            applyStructural(ApiClient.addManufacturing(order.id, dto))
      end if
    }

    def submitAddTask(mfgId: UUID): Unit = editing.now().foreach { order =>
      val s = addTaskState.now()
      parseUuid(s.taskId) match
        case None => showError(editError, "Modifica ordine")(ApiError("invalid-form", "Seleziona un task valido.", Nil))
        case Some(taskId) =>
          applyStructural(
            ApiClient
              .addManufacturingTask(order.id, mfgId, AddTaskRequest(taskId, Formats.parseDuration(s.hours).getOrElse(0), Nil, parseUuid(s.employee))),
          )
    }

    // ---- Row actions -----------------------------------------------------------------------------
    val confirm = Var(Option.empty[ConfirmData])
    // Removals from the edit modal hit the saved order at once, so they are confirmed first.
    val pendingRemoval = Var(Option.empty[PendingRemoval])

    def transition(id: UUID, action: String): Unit =
      ApiClient.orderTransition(id, TransitionRequest(action, None)).foreach {
        case Right(_) => AppBus.mutatedOrders()
        case Left(err) => showError(pageError, "Transizione ordine")(err)
      }

    def cancelOrder(id: UUID): Unit =
      ApiClient.deleteOrder(id, None).foreach {
        case Right(_) => AppBus.mutatedOrders()
        case Left(err) => showError(pageError, "Annullamento ordine")(err)
      }

    /** Completing/delivering/cancelling changes the order lifecycle, so it goes through an acknowledgement modal first. */
    def requestTransition(order: OrderResponse, action: String): Unit = action match
      case "complete" =>
        confirm.set(
          Some(
            ConfirmData(
              order.id,
              action,
              "Completare l'ordine?",
              s"Stai per segnare l'ordine ${order.number} come completato. Verifica che tutte le lavorazioni siano concluse.",
            ),
          ),
        )
      case "deliver" =>
        confirm.set(
          Some(
            ConfirmData(
              order.id,
              action,
              "Confermare la consegna?",
              s"Stai per segnare l'ordine ${order.number} come consegnato. L'operazione è definitiva.",
            ),
          ),
        )
      case "cancel" =>
        confirm.set(
          Some(
            ConfirmData(
              order.id,
              action,
              "Annullare l'ordine?",
              s"Stai per annullare l'ordine ${order.number}: verrà tolto dalla pianificazione. Potrai riaprirlo in seguito con \"Riapri\".",
            ),
          ),
        )
      case _ => transition(order.id, action)

    /** Runs an acknowledged action: cancellation goes through the delete endpoint, everything else is a lifecycle transition. */
    def runConfirmed(data: ConfirmData): Unit =
      if data.action == "cancel" then cancelOrder(data.orderId) else transition(data.orderId, data.action)

    def transitionButtons(order: OrderResponse): List[HtmlElement] =
      val actions = order.status match
        case "in_progress" => List("suspend" -> "Sospendi", "complete" -> "Completa")
        case "suspended" => List("reactivate" -> "Riattiva", "complete" -> "Completa")
        case "completed" => List("deliver" -> "Consegna", "reopen" -> "Riapri")
        case "cancelled" => List("reopen" -> "Riapri")
        case _ => Nil
      actions.map { case (action, label) =>
        button(tpe := "button", cls := btnSmall, label, onClick --> (_ => requestTransition(order, action)))
      }

    // ---- Edit modal rendering --------------------------------------------------------------------
    def editContent(order: OrderResponse): HtmlElement =
      // Viewers open the same modal in read-only mode ("Dettaglio"); mutations require the operator role.
      val editable = OrderStatus.isEditable(order.status) && AuthService.currentHasRole(Role.Operator)
      val rowsByTask: Map[UUID, TaskEditRow] = editTasks.now().map(row => row.taskId -> row).toMap
      val mfgById: Map[UUID, MfgEditRow] = editMfgs.now().map(row => row.id -> row).toMap

      /** Selectable prerequisites for a scheduled task: the other template tasks of the same manufacturing. */
      def taskEditDependencyChoices(m: ManufacturingResponse, row: TaskEditRow): Signal[List[(String, String)]] =
        tasksMap.map { names =>
          m.tasks
            .filter(t => t.id != row.taskId && t.taskId != row.templateTaskId)
            .map(t => t.taskId.toString -> names.getOrElse(t.taskId, Formats.shortId(t.taskId)))
            .distinctBy(_._1)
        }

      def renderTaskEdit(m: ManufacturingResponse, t: ScheduledTaskDto): HtmlElement =
        val nameNode = child.text <-- tasksMap.map(_.getOrElse(t.taskId, Formats.shortId(t.taskId)))
        val employeeSuffix: Signal[String] =
          employeesMap.map(names => t.preferredEmployeeId.fold("")(empId => s" · ${names.getOrElse(empId, Formats.shortId(empId))}"))
        rowsByTask.get(t.id) match
          case Some(row) if editable =>
            div(
              cls := "border-t border-slate-100 pt-2 first:border-0 first:pt-0",
              div(
                cls := "grid grid-cols-1 gap-2 sm:grid-cols-[minmax(0,1fr)_11rem_5rem_5rem_auto] sm:items-end",
                div(
                  cls := "flex-1",
                  div(cls := "text-sm text-slate-700", nameNode),
                  div(cls := "text-xs text-slate-400", OrderStatus.label(t.status)),
                ),
                field(
                  "Dipendente",
                  selectInput(
                    row.tracker.current.signal.map(_.employee),
                    Observer[String](v => row.tracker.current.update(_.copy(employee = v))),
                    employeeOptions,
                  ),
                ),
                field(
                  "Previste (h:mm)",
                  textInput(
                    row.tracker.current.signal.map(_.expected),
                    Observer[String](v => row.tracker.current.update(_.copy(expected = v))),
                    "1:35",
                  ),
                ),
                field(
                  "Fatte (h:mm)",
                  textInput(
                    row.tracker.current.signal.map(_.completed),
                    Observer[String](v => row.tracker.current.update(_.copy(completed = v))),
                    "1:35",
                  ),
                ),
                button(
                  tpe := "button",
                  cls := s"$btnDanger w-full justify-center sm:mb-0.5 sm:w-auto",
                  disabled := m.tasks.size <= 1,
                  aria.label := "Rimuovi task",
                  title := "Rimuovi task",
                  "✕",
                  onClick --> (_ => if m.tasks.size > 1 then pendingRemoval.set(Some(PendingRemoval(order.id, m.id, m.code, Some((t.id, t.taskId)))))),
                ),
              ),
              checkboxChips(
                "Dipende da",
                taskEditDependencyChoices(m, row),
                row.tracker.current.signal.map(_.dependsOn),
                (id, checked) => row.tracker.current.update(s => s.copy(dependsOn = if checked then s.dependsOn + id else s.dependsOn - id)),
              ),
            )
          case _ =>
            div(
              cls := "flex flex-col gap-1 border-t border-slate-100 pt-2 text-sm first:border-0 first:pt-0 sm:flex-row sm:items-center sm:justify-between",
              div(cls := "text-slate-700", nameNode),
              div(
                cls := "text-xs text-slate-500",
                child.text <-- employeeSuffix.map(suffix =>
                  s"${Formats.duration(t.expectedMinutes)}h previste · ${Formats.duration(t.completedMinutes.getOrElse(0))}h fatte · ${OrderStatus.label(t.status)}$suffix",
                ),
              ),
            )
        end match
      end renderTaskEdit

      def addTaskForm(m: ManufacturingResponse): HtmlElement =
        div(
          cls := "mt-2",
          child <-- addTaskMfgId.signal.map { target =>
            if target.contains(m.id) then
              div(
                cls := "grid grid-cols-1 gap-2 rounded-md border border-slate-200 bg-slate-50 p-2 sm:grid-cols-[minmax(0,1fr)_11rem_5rem_auto_auto] sm:items-end",
                div(
                  cls := "flex-1",
                  field(
                    "Task",
                    selectInput(
                      addTaskState.signal.map(_.taskId),
                      Observer[String](v => addTaskState.update(_.copy(taskId = v, employee = proposedEmployee(v)))),
                      taskOptions,
                    ),
                  ),
                ),
                field(
                  "Dipendente",
                  selectInput(addTaskState.signal.map(_.employee), Observer[String](v => addTaskState.update(_.copy(employee = v))), employeeOptions),
                ),
                field(
                  "Durata (h:mm)",
                  textInput(addTaskState.signal.map(_.hours), Observer[String](v => addTaskState.update(_.copy(hours = v))), "1:35"),
                ),
                button(
                  tpe := "button",
                  cls := s"$btnSmall w-full justify-center sm:mb-0.5 sm:w-auto",
                  "Aggiungi",
                  onClick --> (_ => submitAddTask(m.id)),
                ),
                button(tpe := "button", cls := s"$btnGhost w-full justify-center sm:mb-0.5 sm:w-auto", "Annulla", onClick --> (_ => resetAddTask())),
              )
            else
              button(
                tpe := "button",
                cls := btnSmall,
                "+ Task",
                onClick --> (_ =>
                  resetAddTask(); addTaskMfgId.set(Some(m.id))
                ),
              )
          },
        )

      /** Selectable prerequisites for a manufacturing: the other manufacturings of the order, labelled by code. */
      def mfgEditDependencyChoices(current: UUID): Signal[List[(String, String)]] =
        Signal.fromValue(order.manufacturings.filter(_.id != current).map(other => other.id.toString -> other.code))

      /** Codes of the manufacturings the given one depends on, for the read-only view. */
      def dependencyCodes(m: ManufacturingResponse): List[String] =
        order.dependencies
          .find(_.manufacturingId == m.id)
          .map(_.dependsOn)
          .getOrElse(Nil)
          .flatMap(depId => order.manufacturings.find(_.id == depId).map(_.code))

      def renderMfgEdit(m: ManufacturingResponse): HtmlElement =
        div(
          cls := "rounded-lg border border-slate-200 p-3",
          div(
            cls := "mb-2 flex flex-col gap-1 sm:flex-row sm:items-center sm:justify-between sm:gap-2",
            div(
              cls := "flex items-center gap-2",
              span(cls := "font-medium text-slate-800", m.code),
              OrderCreateView.quantityBadge(m.quantity),
            ),
            div(
              cls := "flex items-center gap-2",
              span(cls := "text-xs text-slate-400", Formats.date(m.completionDate)),
              if editable then emptyNode else statusBadge(m.status),
            ),
          ),
          if editable then
            mfgById.get(m.id) match
              case Some(row) =>
                div(
                  cls := "mb-2",
                  div(
                    cls := "grid grid-cols-1 gap-2 sm:grid-cols-2",
                    field(
                      "Descrizione",
                      textInput(
                        row.tracker.current.signal.map(_.description),
                        Observer[String](v => row.tracker.current.update(_.copy(description = v))),
                        "Opzionale",
                      ),
                    ),
                    field(
                      "Deadline lavorazione",
                      textInput(
                        row.tracker.current.signal.map(_.completionDate),
                        Observer[String](v => row.tracker.current.update(_.copy(completionDate = v))),
                        "",
                        "date",
                      ),
                    ),
                    field(
                      "Stato",
                      staticSelect(
                        row.tracker.current.signal.map(_.status),
                        Observer[String](v => row.tracker.current.update(_.copy(status = v))),
                        mfgStatusOptions,
                      ),
                    ),
                    field(
                      "Dipendente preferito",
                      selectInput(
                        row.tracker.current.signal.map(_.employee),
                        Observer[String](v => row.tracker.current.update(_.copy(employee = v))),
                        employeeOptions,
                      ),
                    ),
                  ),
                  checkboxChips(
                    "Dipende da",
                    mfgEditDependencyChoices(m.id),
                    row.tracker.current.signal.map(_.dependsOn),
                    (id, checked) => row.tracker.current.update(s => s.copy(dependsOn = if checked then s.dependsOn + id else s.dependsOn - id)),
                  ),
                )
              case None => emptyNode
          else
            val depsLine = dependencyCodes(m) match
              case Nil => emptyNode
              case codes => div(cls := "mb-2 text-xs text-slate-500", s"Dipende da: ${codes.mkString(", ")}")
            div(
              m.description.map(d => div(cls := "mb-2 text-sm text-slate-600", d)).getOrElse(emptyNode),
              depsLine,
            )
          ,
          div(cls := "space-y-2", m.tasks.map(t => renderTaskEdit(m, t))),
          if editable then addTaskForm(m) else emptyNode,
          // Keeps the unsaved edits of the modal: only the order snapshot is replaced (materials do not touch the other fields).
          MaterialsEditor.section(order, m, updated => editing.set(Some(updated))),
          if editable && order.manufacturings.size > 1 then
            div(
              cls := "mt-2 flex justify-end",
              button(
                tpe := "button",
                cls := s"$btnDanger w-full justify-center sm:w-auto",
                "Rimuovi lavorazione",
                onClick --> (_ => pendingRemoval.set(Some(PendingRemoval(order.id, m.id, m.code, None)))),
              ),
            )
          else emptyNode,
        )

      def addMfgForm: HtmlElement =
        div(
          cls := "mt-2",
          child <-- showAddMfg.signal.map { open =>
            if open then
              div(
                cls := "space-y-2 rounded-lg border border-slate-200 bg-slate-50 p-3",
                div(
                  cls := "grid grid-cols-1 gap-2 sm:grid-cols-2",
                  field(
                    "Tipo",
                    staticSelect(
                      addMfgState.signal.map(_.mode),
                      Observer[String](v => addMfgState.update(_.copy(mode = v))),
                      manufacturingModeOptions,
                    ),
                  ),
                  field(
                    "Completamento",
                    textInput(addMfgState.signal.map(_.date), Observer[String](v => addMfgState.update(_.copy(date = v))), "", "date"),
                  ),
                ),
                field(
                  "Dipendente preferito",
                  selectInput(addMfgState.signal.map(_.employee), Observer[String](v => addMfgState.update(_.copy(employee = v))), employeeOptions),
                ),
                // `.distinct` avoids recreating the subtree (and losing input focus) on every keystroke.
                child <-- addMfgState.signal.map(_.mode).distinct.map {
                  case "catalog" =>
                    div(
                      cls := "space-y-2",
                      field(
                        "Lavorazione catalogo",
                        selectInput(
                          addMfgState.signal.map(_.catalogId),
                          // Selecting a template also prefills the per-task employees from the catalog defaults.
                          Observer[String](v => addMfgState.update(_.copy(catalogId = v, taskEmployees = defaultTaskEmployees(v)))),
                          manufacturingCatalogOptions,
                        ),
                      ),
                      OrderCreateView.catalogPreview(addMfgState.signal.map(_.catalogId), manufacturingCatalogs.signal),
                      OrderCreateView.catalogTaskEmployeeOverrides(
                        addMfgState.signal.map(_.catalogId),
                        manufacturingCatalogs.signal,
                        employeeOptions,
                        addMfgState.signal.map(_.taskEmployees),
                        (taskId, employeeId) => addMfgState.update(s => s.copy(taskEmployees = s.taskEmployees.updated(taskId, employeeId))),
                      ),
                    )
                  case _ =>
                    div(
                      cls := "space-y-2",
                      field(
                        "Codice lavorazione",
                        textInput(addMfgState.signal.map(_.code), Observer[String](v => addMfgState.update(_.copy(code = v))), "MFG-2026-002"),
                      ),
                      field(
                        "Descrizione",
                        textInput(
                          addMfgState.signal.map(_.description),
                          Observer[String](v => addMfgState.update(_.copy(description = v))),
                          "Opzionale",
                        ),
                      ),
                      div(
                        cls := "grid grid-cols-1 gap-2 sm:grid-cols-[minmax(0,1fr)_5rem]",
                        field(
                          "Primo task",
                          selectInput(
                            addMfgState.signal.map(_.taskId),
                            Observer[String](v => addMfgState.update(_.copy(taskId = v, employee = proposedEmployee(v)))),
                            taskOptions,
                          ),
                        ),
                        field(
                          "Durata (h:mm)",
                          textInput(addMfgState.signal.map(_.hours), Observer[String](v => addMfgState.update(_.copy(hours = v))), "1:35"),
                        ),
                      ),
                    )
                },
                div(
                  cls := "flex flex-col-reverse gap-2 sm:flex-row sm:justify-end",
                  button(
                    tpe := "button",
                    cls := s"$btnGhost justify-center",
                    "Annulla",
                    onClick --> (_ => resetAddMfg()),
                  ),
                  button(tpe := "button", cls := s"$btnSmall justify-center", "Aggiungi lavorazione", onClick --> (_ => submitAddMfg())),
                ),
              )
            else
              button(
                tpe := "button",
                cls := btnGhost,
                "+ Lavorazione",
                onClick --> (_ =>
                  resetAddMfg(); showAddMfg.set(true)
                ),
              )
          },
        )

      div(
        cls := "space-y-4",
        div(
          cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
          readField("Numero", order.number),
          readField("Stato", statusBadge(order.status)),
          readField("Cliente", child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId)))),
          readField("Consegna cliente", Formats.date(order.deliveryDate)),
          readField("Costo materiali", WarehouseFormats.formatEuro(order.materialsCost)),
        ),
        if editable then
          div(
            cls := "space-y-3",
            div(
              cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
              field(
                "Priorità",
                staticSelect(editHeader.signal.map(_.priority), Observer[String](v => editHeader.update(_.copy(priority = v))), priorityOptions),
              ),
              field(
                "Deadline fine lavorazione",
                textInput(editHeader.signal.map(_.promised), Observer[String](v => editHeader.update(_.copy(promised = v))), "", "date"),
              ),
            ),
            field(
              "Descrizione ordine",
              textInput(editHeader.signal.map(_.description), Observer[String](v => editHeader.update(_.copy(description = v))), "Opzionale"),
            ),
          )
        else
          div(
            cls := "space-y-2",
            order.description.map(d => readField("Descrizione", d)).getOrElse(emptyNode),
            div(
              cls := "rounded-md border border-slate-200 bg-slate-50 p-3 text-xs text-slate-500",
              "Questo ordine non è in uno stato modificabile: solo gli ordini in corso o sospesi possono essere modificati.",
            ),
          )
        ,
        div(
          cls := "space-y-2",
          div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Lavorazioni e task"),
          div(cls := "space-y-2", order.manufacturings.map(renderMfgEdit)),
          if editable then addMfgForm else emptyNode,
        ),
        child.maybe <-- editError.signal.map(_.map(errorBanner)),
        div(
          cls := "flex flex-col-reverse gap-2 border-t border-slate-100 pt-3 sm:flex-row sm:justify-end",
          button(tpe := "button", cls := s"$btnGhost justify-center", "Chiudi", onClick --> (_ => editing.set(None))),
          if editable then button(tpe := "button", cls := s"$btnPrimary justify-center", "Salva modifiche", onClick --> (_ => saveEdit()))
          else emptyNode,
        ),
      )
    end editContent

    val editModal =
      modal(
        editing.signal.map(_.isDefined),
        editing.signal.map(
          _.map(o => s"${if OrderStatus.isEditable(o.status) then "Modifica" else "Dettaglio"} ordine ${o.number}").getOrElse(""),
        ),
        "max-w-2xl",
        () => editing.set(None),
      )(div(child <-- editing.signal.map { case Some(o) => editContent(o); case None => emptyNode }))

    val removalDialog =
      confirmDialog(
        isOpen = pendingRemoval.signal.map(_.isDefined),
        titleText = pendingRemoval.signal.combineWith(tasksMap).map {
          case (Some(PendingRemoval(_, _, code, Some((_, templateId)))), names) =>
            s"Rimuovere il task «${names.getOrElse(templateId, Formats.shortId(templateId))}» da «$code»?"
          case (Some(removal), _) => s"Rimuovere la lavorazione «${removal.mfgCode}»?"
          case (None, _) => ""
        },
        message = Signal.fromValue("La rimozione viene applicata subito all'ordine salvato e non è reversibile."),
        confirmLabel = "Rimuovi",
        tone = DialogTone.Danger,
        onCancel = () => pendingRemoval.set(None),
        onConfirm = () =>
          pendingRemoval.now().foreach { removal =>
            pendingRemoval.set(None)
            removal.task match
              case Some((scheduledId, _)) => applyStructural(ApiClient.removeManufacturingTask(removal.orderId, removal.mfgId, scheduledId))
              case None => applyStructural(ApiClient.removeManufacturing(removal.orderId, removal.mfgId))
          },
      )

    // ---- Confirmation (ACK) modal ----------------------------------------------------------------
    val confirmModal =
      confirmDialog(
        isOpen = confirm.signal.map(_.isDefined),
        titleText = confirm.signal.map(_.fold("")(_.title)),
        message = confirm.signal.map(_.fold("")(_.message)),
        confirmLabel = "Conferma",
        tone = DialogTone.Neutral,
        onCancel = () => confirm.set(None),
        onConfirm = () => confirm.now().foreach { c => runConfirmed(c); confirm.set(None) },
      )

    // ---- Row / table rendering -------------------------------------------------------------------
    def orderActionButtons(order: OrderResponse): List[HtmlElement] =
      val canOperate = AuthService.currentHasRole(Role.Operator)
      val detailsButton = button(
        tpe := "button",
        cls := btnSmall,
        if OrderStatus.isEditable(order.status) && canOperate then "Modifica" else "Dettagli",
        onClick --> (_ => openEdit(order)),
      )
      // Cancelling removes the order (DELETE), which the server restricts to admins.
      val cancelButton =
        if order.status == "cancelled" || !AuthService.currentHasRole(Role.Admin) then Nil
        else List(button(tpe := "button", cls := btnDanger, "Annulla ordine", onClick --> (_ => requestTransition(order, "cancel"))))
      detailsButton :: (if canOperate then transitionButtons(order) else Nil) ++ cancelButton

    def renderOrderCard(order: OrderResponse): HtmlElement =
      val taskCount = order.manufacturings.map(_.tasks.size).sum
      div(
        cls := "space-y-3 p-4",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "break-words font-medium text-slate-800", order.number),
            div(
              cls := "mt-1 text-sm text-slate-600",
              child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId))),
            ),
            order.description.map(d => div(cls := "mt-1 break-words text-xs text-slate-400", d)).getOrElse(emptyNode),
          ),
          div(cls := "shrink-0", statusBadge(order.status)),
        ),
        div(
          cls := "grid grid-cols-2 gap-3 text-sm",
          readField("Priorità", priorityBadge(order.priority)),
          readField("Consegna", Formats.date(order.deliveryDate)),
          readField("Lavorazioni", s"${order.manufacturings.size} lav."),
          readField("Task", s"$taskCount task"),
        ),
        div(
          cls := cardActionsCls,
          orderActionButtons(order),
        ),
      )
    end renderOrderCard

    def renderRow(order: OrderResponse): HtmlElement =
      val taskCount = order.manufacturings.map(_.tasks.size).sum
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(
          cls := "px-4 py-2",
          div(cls := "font-medium text-slate-800", order.number),
          order.description.map(d => div(cls := "text-xs text-slate-400", d)).getOrElse(emptyNode),
        ),
        td(cls := "px-4 py-2 text-slate-600", child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId)))),
        td(cls := "px-4 py-2", statusBadge(order.status)),
        td(cls := "px-4 py-2", priorityBadge(order.priority)),
        td(cls := "px-4 py-2 text-slate-500", Formats.date(order.deliveryDate)),
        td(cls := "px-4 py-2 text-slate-500", s"${order.manufacturings.size} lav. · $taskCount task"),
        td(
          cls := "px-4 py-2",
          div(
            cls := "flex flex-wrap justify-end gap-2",
            orderActionButtons(order),
          ),
        ),
      )
    end renderRow

    // Status in lifecycle order, priority urgent first: sorting by them groups the rows the way the filter chips list them.
    val statusRank = statusFilters.map(_._1).filter(_.nonEmpty).zipWithIndex.toMap
    val sort = TableSort[OrderResponse](
      customersMap.map { customers =>
        Map(
          "number" -> Column.text(_.number),
          "customer" -> Column.optionalText(o => customers.get(o.customerId)),
          "status" -> Column.by(o => statusRank.getOrElse(o.status, statusRank.size)),
          "priority" -> Column.by(o => if o.priority == "urgent" then 0 else 1),
          "delivery" -> Column.text(_.deliveryDate),
          "manufacturings" -> Column.by(_.manufacturings.size),
        )
      },
      pager,
    )

    val filterBar =
      div(
        cls := "mb-4 flex flex-wrap items-center gap-2",
        statusFilters.map { case (value, labelText) =>
          button(
            tpe := "button",
            cls <-- statusFilter.signal.map(current => chipCls(current == value)),
            labelText,
            onClick --> { _ => statusFilter.set(value); pager.reset() },
          )
        },
        div(cls := "w-full sm:ml-auto sm:w-80", searchBox(query, "Cerca per numero, cliente, lavorazione…", pager)),
        div(
          cls := "w-full md:hidden",
          sort.picker(
            List(
              "number" -> "Ordine",
              "customer" -> "Cliente",
              "status" -> "Stato",
              "priority" -> "Priorità",
              "delivery" -> "Consegna",
              "manufacturings" -> "Lavorazioni",
            ),
          ),
        ),
      )

    val searchFields: Signal[OrderResponse => List[String]] =
      customersMap.map(customers => order => ListSearch.order(order, customers.get(order.customerId)))

    def ordersTable(orders: List[OrderResponse]): HtmlElement =
      val filtered = sort.sorted(orders).combineWith(statusFilter.signal, query.signal, searchFields).map { (sorted, filter, typed, fieldsOf) =>
        sorted.filter(o => (filter.isEmpty || o.status == filter) && SearchTerms.matchesAny(typed, fieldsOf(o)))
      }
      pager.view(filtered) { visible =>
        if visible.isEmpty then div(cls := "p-4", emptyState("Nessun ordine corrisponde ai filtri."))
        else
          div(
            div(cls := "divide-y divide-slate-100 md:hidden", visible.map(renderOrderCard)),
            div(
              cls := "hidden overflow-x-auto md:block",
              table(
                cls := "w-full text-sm",
                thead(
                  cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                  tr(
                    sort.header("number", "Ordine", "px-4 py-2"),
                    sort.header("customer", "Cliente", "px-4 py-2"),
                    sort.header("status", "Stato", "px-4 py-2"),
                    sort.header("priority", "Priorità", "px-4 py-2"),
                    sort.header("delivery", "Consegna", "px-4 py-2"),
                    sort.header("manufacturings", "Lavorazioni", "px-4 py-2"),
                    th(cls := "px-4 py-2"),
                  ),
                ),
                tbody(visible.map(renderRow)),
              ),
            ),
          )
        end if
      }
    end ordersTable

    // ---- Page layout -----------------------------------------------------------------------------
    div(
      manufacturingCatalogData --> {
        case Some(Right(list)) => manufacturingCatalogs.set(list)
        case _ => ()
      },
      tasksData --> {
        case Some(Right(list)) => taskDefaultEmployees.set(list.flatMap(t => t.defaultEmployeeId.map(t.id.toString -> _)).toMap)
        case _ => ()
      },
      div(
        cls <-- view.signal.map(current => if current == View.List then "" else "hidden"),
        div(
          cls := "mb-4 flex items-center justify-between",
          sectionTitle("Ordini"),
          roleGated(Role.Operator)(
            button(
              tpe := "button",
              cls := btnPrimary,
              "+ Nuovo ordine",
              onClick --> (_ => view.set(View.Create)),
            ),
          ),
        ),
        child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mb-4", errorBanner(e)))),
        filterBar,
        card(
          cls := "overflow-hidden",
          renderResult(ordersData) { orders =>
            if orders.isEmpty then div(cls := "p-4", emptyState("Nessun ordine. Crea il primo ordine."))
            else ordersTable(orders)
          },
        ),
      ),
      child.maybe <-- view.signal.distinct.map(current =>
        Option.when(current == View.Create)(OrderCreateView(createDeps, () => view.set(View.List))),
      ),
      editModal,
      // After the edit modal, so they stack above it; as its siblings, their Escape never reaches it.
      removalDialog,
      confirmModal,
    )
  end apply
end OrdersPage
