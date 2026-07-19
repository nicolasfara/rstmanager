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

/**
 * Orders: filterable list with lifecycle transitions, a nested create form, and a full edit modal covering order data (priority, work deadline,
 * description), per-manufacturing description/deadline/status, add/remove of manufacturings and tasks, and the dependency graphs (between
 * manufacturings and between the tasks of a manufacturing). Completing or delivering an order asks for an explicit acknowledgement first.
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

  /** HTML date inputs yield `yyyy-MM-dd`; the API expects a full ISO-8601 instant. */
  private def toIso(day: String): String = if day.isEmpty then "" else s"${day}T00:00:00.000Z"

  /** Extracts the `yyyy-MM-dd` day part of an ISO-8601 instant for a date input's initial value. */
  private def dayOf(iso: String): String = iso.take(10)

  private def pad2(value: Int): String = if value < 10 then s"0$value" else value.toString

  private def formatDay(date: js.Date): String =
    s"${date.getFullYear().toInt}-${pad2(date.getMonth().toInt + 1)}-${pad2(date.getDate().toInt)}"

  private def parseDay(day: String): Option[js.Date] =
    day.split("-").nn.toList.map(_.nn) match
      case yearRaw :: monthRaw :: dayRaw :: Nil =>
        for
          year <- yearRaw.toIntOption
          month <- monthRaw.toIntOption
          dayOfMonth <- dayRaw.toIntOption
        yield new js.Date(year, month - 1, dayOfMonth)
      case _ => None

  private def daysBefore(day: String, days: Int): String =
    parseDay(day).map { date =>
      val shifted = new js.Date(date.getTime())
      shifted.setDate(date.getDate() - days.toDouble)
      formatDay(shifted)
    }
      .getOrElse("")

  private def todayDay(): String =
    formatDay(new js.Date())

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** `java.util.UUID.randomUUID` is unavailable on Scala.js (needs SecureRandom); use the Web Crypto API. */
  private def randomUuid(): UUID = UUID.fromString(js.Dynamic.global.crypto.randomUUID().toString).nn

  /** Only in-progress and suspended orders accept data/task edits (see the domain `Order` aggregate). */
  private def isEditable(status: String): Boolean = status == "in_progress" || status == "suspended"

  /** Trims free text and collapses blank strings to `None` (an empty value clears the field). */
  private def normalizeStr(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  private def statusLabel(status: String): String = status match
    case "pending" => "In attesa"
    case "in_progress" => "In corso"
    case "completed" => "Completato"
    case "not_started" => "Non iniziata"
    case "paused" => "In pausa"
    case "suspended" => "Sospeso"
    case "delivered" => "Consegnato"
    case "cancelled" => "Annullato"
    case other => other

  private val chipBase = "rounded-full px-3 py-1 text-xs font-medium border transition-colors"
  private def chipCls(active: Boolean): String =
    if active then s"$chipBase bg-slate-900 text-white border-slate-900"
    else s"$chipBase bg-white text-slate-600 border-slate-300 hover:bg-slate-100"

  private def priorityBadge(priority: String): HtmlElement = priority match
    case "urgent" => badge("Urgente", "bg-rose-50 text-rose-700 border-rose-200")
    case _ => badge("Normale", "bg-slate-50 text-slate-600 border-slate-200")

  private def readField(labelText: String, valueNode: Modifier[HtmlElement]): HtmlElement =
    div(
      cls := "space-y-0.5",
      div(cls := "text-xs font-medium text-slate-500", labelText),
      div(cls := "text-sm text-slate-800", valueNode),
    )

  // ---- State case classes ----------------------------------------------------------------------

  /** Immutable state for a task draft in the create form; held in a single `Var`. */
  private case class TaskState(taskId: String, hours: String, dependsOn: Set[String], employeeId: String)

  /** Mutable draft holder: stable key for `split`, all mutable fields consolidated in one `Var`. `dependsOn` holds template-task id strings. */
  private final case class TaskDraft(key: Int, state: Var[TaskState])

  /**
   * Immutable state for the non-list fields of a manufacturing draft. `taskEmployees` holds, for catalog mode, the per-task preferred employee
   * (template-task id -> employee id), prefilled from the catalog defaults and overridable before submit.
   */
  private case class MfgCoreState(
      mode: String,
      catalogId: String,
      code: String,
      completionDate: String,
      description: String,
      employeeId: String,
      dependsOn: Set[String],
      taskEmployees: Map[String, String],
  )

  /** `tasks` is kept as a separate `Var` because it undergoes its own add/remove mutations. `dependsOn` holds sibling draft key strings. */
  private final case class MfgDraft(key: Int, state: Var[MfgCoreState], tasks: Var[List[TaskDraft]])

  /** Consolidated create-form state; reset is atomic via a single `Var.set`. */
  private case class CreateOrderState(
      number: String,
      numberManuallyEdited: Boolean,
      customerId: String,
      creationDate: String,
      deliveryDate: String,
      workDeadline: String,
      workDeadlineManuallyEdited: Boolean,
      priority: String,
      description: String,
  )

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
    val empty: AddMfgState = AddMfgState("custom", "", "", "", "", "", "8", "", Map.empty)

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

  /**
   * "Depends on" checkbox chips. `currentSet` drives the checked state; `toggle(id, checked)` is called on every checkbox change. Hidden when
   * `choices` is empty.
   */
  private def dependsOnChips(
      choices: Signal[List[(String, String)]],
      currentSet: Signal[Set[String]],
      toggle: (String, Boolean) => Unit,
  ): HtmlElement =
    div(
      child <-- choices.map { list =>
        if list.isEmpty then emptyNode
        else
          div(
            cls := "mt-2 flex flex-wrap items-center gap-2 text-xs text-slate-600",
            span(cls := "font-medium text-slate-500", "Dipende da"),
            list.map { case (id, labelText) =>
              label(
                cls := "inline-flex items-center gap-1 rounded-md border border-slate-200 px-2 py-1",
                input(
                  typ := "checkbox",
                  checked <-- currentSet.map(_.contains(id)),
                  onChange.mapToChecked --> { isChecked => toggle(id, isChecked) },
                ),
                labelText,
              )
            },
          )
      },
    )

  /** A lifecycle transition awaiting an explicit user acknowledgement. */
  private final case class ConfirmData(orderId: UUID, action: String, title: String, message: String)

  def apply(): HtmlElement =
    val pageError = Var(Option.empty[ApiError])
    val showCreate = Var(false)
    val statusFilter = Var("")
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1; keyCounter

    val ordersData = loadable(AppBus.ordersTicks)(() => ApiClient.listOrders())
    val customersData = loadable(AppBus.customersTicks)(() => ApiClient.listCustomers())
    val tasksData = loadable(AppBus.tasksTicks)(() => ApiClient.listTasks())
    val manufacturingCatalogData = loadable(AppBus.manufacturingsTicks)(() => ApiClient.listManufacturingCatalog())
    val employeesData = loadable(AppBus.employeesTicks)(() => ApiClient.listEmployees())
    val ordersSnapshot = Var(List.empty[OrderResponse])
    val manufacturingCatalogs = Var(List.empty[ManufacturingCatalogResponse])

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
      case Some(Right(list)) => ("" -> "— task —") :: list.map(t => t.id.toString -> s"${t.name} (${t.requiredHours}h)")
      case _ => List("" -> "—")
    }
    val manufacturingCatalogOptions: Signal[List[(String, String)]] = manufacturingCatalogData.map {
      case Some(Right(list)) => ("" -> "— lavorazione —") :: list.map(m => m.id.toString -> s"${m.code} · ${m.name} (${m.totalRequiredHours}h)")
      case _ => List("" -> "—")
    }
    val employeeOptions: Signal[List[(String, String)]] = employeesData.map {
      case Some(Right(list)) => ("" -> "— auto (pianificazione sceglie) —") :: list.map(e => e.id.toString -> s"${e.name} ${e.surname}")
      case _ => List("" -> "—")
    }

    // ---- Create form state -----------------------------------------------------------------------
    val createState = Var(
      CreateOrderState(
        number = GeneratedCodes.next("ORD", Nil),
        numberManuallyEdited = false,
        customerId = "",
        creationDate = todayDay(),
        deliveryDate = "",
        workDeadline = "",
        workDeadlineManuallyEdited = false,
        priority = "normal",
        description = "",
      ),
    )

    def newTask(): TaskDraft = TaskDraft(nextKey(), Var(TaskState("", "8", Set.empty, "")))
    def newMfg(): MfgDraft =
      MfgDraft(nextKey(), Var(MfgCoreState("custom", "", "", "", "", "", Set.empty, Map.empty)), Var(List(newTask())))

    val mfgs = Var(List(newMfg()))

    def resetCreate(): Unit =
      createState.set(
        CreateOrderState(
          number = GeneratedCodes.next("ORD", ordersSnapshot.now().map(_.number)),
          numberManuallyEdited = false,
          customerId = "",
          creationDate = todayDay(),
          deliveryDate = "",
          workDeadline = "",
          workDeadlineManuallyEdited = false,
          priority = "normal",
          description = "",
        ),
      )
      mfgs.set(List(newMfg()))
      pageError.set(None)

    def catalogById(rawId: String): Option[ManufacturingCatalogResponse] =
      parseUuid(rawId).flatMap(id => manufacturingCatalogs.now().find(_.id == id))

    /** Per-task employee prefill for a catalog template: the defaults configured on the catalog manufacturing. */
    def defaultTaskEmployees(rawCatalogId: String): Map[String, String] =
      catalogById(rawCatalogId).map(_.defaultEmployees.map(d => d.taskId.toString -> d.employeeId.toString).toMap).getOrElse(Map.empty)

    def fromCatalog(
        template: ManufacturingCatalogResponse,
        completionDate: String,
        employeeId: String,
        taskEmployees: Map[String, String],
    ): ManufacturingDto =
      ManufacturingDto(
        template.code,
        toIso(completionDate),
        "not_started",
        template.tasks.map { task =>
          ScheduledTaskDto(
            randomUuid(),
            task.id,
            "pending",
            task.requiredHours,
            Some(0),
            None,
            taskEmployees.get(task.id.toString).flatMap(parseUuid),
          )
        },
        template.dependencies.map(dependency => TaskDependencyDto(dependency.taskId, dependency.dependsOn)),
        None,
        None,
        None,
        None,
        template.description,
        parseUuid(employeeId),
      )

    def fromCustom(m: MfgDraft, completionDate: String): ManufacturingDto =
      val ms = m.state.now()
      val tasks = m.tasks.now().flatMap { t =>
        val ts = t.state.now()
        parseUuid(ts.taskId).map { taskId =>
          ScheduledTaskDto(randomUuid(), taskId, "pending", ts.hours.toIntOption.getOrElse(0), Some(0), None, parseUuid(ts.employeeId))
        }
      }
      val selectedIds = tasks.map(_.taskId.toString).toSet
      val dependencies = m.tasks.now().flatMap { t =>
        val ts = t.state.now()
        parseUuid(ts.taskId).flatMap { taskId =>
          val dependsOn = ts.dependsOn.toList.filter(dep => selectedIds.contains(dep) && dep != taskId.toString).flatMap(parseUuid)
          if dependsOn.isEmpty then None else Some(TaskDependencyDto(taskId, dependsOn))
        }
      }
      ManufacturingDto(
        ms.code.trim.nn,
        toIso(completionDate),
        "not_started",
        tasks,
        dependencies,
        None,
        None,
        None,
        None,
        normalizeStr(ms.description),
        parseUuid(ms.employeeId),
      )
    end fromCustom

    def manufacturingFromDraft(m: MfgDraft): Either[ApiError, ManufacturingDto] =
      val ms = m.state.now()
      // Empty completion date falls back to the order-level work deadline (validated non-empty before this runs).
      val completionDate = Some(ms.completionDate.trim.nn).filter(_.nonEmpty).getOrElse(createState.now().workDeadline)
      if ms.mode == "catalog" then
        catalogById(ms.catalogId).toRight(ApiError("invalid-form", "Seleziona una lavorazione a catalogo valida.", Nil)).map { template =>
          fromCatalog(template, completionDate, ms.employeeId, ms.taskEmployees)
        }
      else Right(fromCustom(m, completionDate))

    def collectManufacturings(): Either[ApiError, List[ManufacturingDto]] =
      mfgs.now().foldLeft(Right(List.empty): Either[ApiError, List[ManufacturingDto]]) { (acc, draft) =>
        acc.flatMap(list => manufacturingFromDraft(draft).map(list :+ _))
      }

    /** Maps the draft-key based "depends on" selections to positional indexes for the creation request. */
    def collectDependencies(): Option[List[ManufacturingDependencyByIndexDto]] =
      val drafts = mfgs.now()
      val indexByKey = drafts.zipWithIndex.map((draft, index) => draft.key.toString -> index).toMap
      val entries = drafts.zipWithIndex.flatMap { (draft, index) =>
        val dependsOnIndexes = draft.state.now().dependsOn.toList.flatMap(indexByKey.get).filter(_ != index).sorted
        if dependsOnIndexes.isEmpty then None else Some(ManufacturingDependencyByIndexDto(index, dependsOnIndexes))
      }
      if entries.isEmpty then None else Some(entries)

    val createFormErrors: Signal[List[String]] = createState.signal.map { cs =>
      List(
        Option.when(cs.number.trim.nn.isEmpty)("Numero ordine obbligatorio"),
        Option.when(cs.customerId.isEmpty)("Cliente obbligatorio"),
        Option.when(cs.creationDate.trim.nn.isEmpty)("Data creazione obbligatoria"),
        Option.when(cs.deliveryDate.trim.nn.isEmpty)("Data consegna obbligatoria"),
        Option.when(cs.workDeadline.trim.nn.isEmpty)("Scadenza lavoro obbligatoria"),
      ).flatten
    }

    def submitCreate(): Unit =
      val cs = createState.now()
      parseUuid(cs.customerId) match
        case None => showError(pageError, "Creazione ordine")(ApiError("invalid-form", "Seleziona un cliente valido.", Nil))
        case Some(cId) =>
          if cs.deliveryDate.isEmpty then showError(pageError, "Creazione ordine")(ApiError("invalid-form", "Inserisci la consegna cliente.", Nil))
          else if cs.workDeadline.isEmpty then
            showError(pageError, "Creazione ordine")(ApiError("invalid-form", "Inserisci la deadline di fine lavorazione.", Nil))
          else
            collectManufacturings() match
              case Left(err) => showError(pageError, "Creazione ordine")(err)
              case Right(manufacturings) =>
                val request = OrderRequest(
                  cs.number.trim.nn,
                  cId,
                  toIso(cs.creationDate),
                  toIso(cs.deliveryDate),
                  toIso(cs.workDeadline),
                  cs.priority,
                  manufacturings,
                  normalizeStr(cs.description),
                  collectDependencies(),
                )
                ApiClient.createOrder(request).foreach {
                  case Right(_) => resetCreate(); showCreate.set(false); AppBus.mutatedOrders()
                  case Left(err) => showError(pageError, "Creazione ordine")(err)
                }
      end match
    end submitCreate

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
    val addTaskState = Var(AddTaskState("", "8", ""))

    def resetAddTask(): Unit =
      addTaskMfgId.set(None)
      addTaskState.set(AddTaskState("", "8", ""))

    def openEdit(order: OrderResponse): Unit =
      val orderDepsByMfg: Map[UUID, Set[String]] =
        order.dependencies.map(d => d.manufacturingId -> d.dependsOn.map(_.toString).toSet).toMap
      val headerState = EditHeaderState(
        priority = order.priority,
        promised = order.promisedDeliveryDate.map(dayOf).getOrElse(""),
        description = order.description.getOrElse(""),
      )
      editHeader.set(headerState)
      editHeaderTracker.set(DirtyTracker(headerState, editHeader))
      editTasks.set(order.manufacturings.flatMap { m =>
        val taskDepsByTemplate: Map[UUID, Set[String]] =
          m.dependencies.map(d => d.taskId -> d.dependsOn.map(_.toString).toSet).toMap
        m.tasks.map { t =>
          val completed = t.completedHours.getOrElse(0)
          val deps = taskDepsByTemplate.getOrElse(t.taskId, Set.empty)
          val state = TaskEditState(t.expectedHours.toString, completed.toString, deps, t.preferredEmployeeId.map(_.toString).getOrElse(""))
          TaskEditRow(m.id, t.id, t.taskId, DirtyTracker(state, Var(state)))
        }
      })
      editMfgs.set(order.manufacturings.map { m =>
        val desc = m.description.getOrElse("")
        val completionDate = dayOf(m.completionDate)
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
            if promisedChanged then Some(toIso(h.promised)) else None,
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
            if completionDateChanged then Some(toIso(es.completionDate)) else None,
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
        val newExpected = es.expected.toIntOption
        val newCompleted = es.completed.toIntOption
        val expectedChanged = newExpected.exists(_ => row.tracker.changed(_.expected.toIntOption))
        val completedChanged = newCompleted.exists(_ => row.tracker.changed(_.completed.toIntOption))
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
          case Some(template) => applyStructural(ApiClient.addManufacturing(order.id, fromCatalog(template, s.date, s.employee, s.taskEmployees)))
      else
        parseUuid(s.taskId) match
          case None => showError(editError, "Modifica ordine")(ApiError("invalid-form", "Seleziona un task valido per la nuova lavorazione.", Nil))
          case Some(taskId) =>
            val task = ScheduledTaskDto(randomUuid(), taskId, "pending", s.hours.toIntOption.getOrElse(0), Some(0), None)
            val dto = ManufacturingDto(
              s.code.trim.nn,
              toIso(s.date),
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
            ApiClient.addManufacturingTask(order.id, mfgId, AddTaskRequest(taskId, s.hours.toIntOption.getOrElse(0), Nil, parseUuid(s.employee))),
          )
    }

    // ---- Row actions -----------------------------------------------------------------------------
    val confirm = Var(Option.empty[ConfirmData])

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

    // ---- Create form rendering -------------------------------------------------------------------

    /** Selectable prerequisites for a task draft: the (valid) template tasks chosen by the sibling drafts. */
    def taskDependencyChoices(m: MfgDraft, t: TaskDraft): Signal[List[(String, String)]] =
      m.tasks.signal.combineWith(taskOptions).map { case (rows, options) =>
        val labels = options.toMap
        rows
          .filter(_.key != t.key)
          .flatMap { other =>
            val id = other.state.now().taskId
            if id.isEmpty || id == t.state.now().taskId then None else labels.get(id).map(labelText => id -> labelText)
          }
          .distinctBy(_._1)
      }

    def renderTask(m: MfgDraft, t: TaskDraft): HtmlElement =
      div(
        div(
          cls := "grid grid-cols-1 gap-2 sm:grid-cols-[minmax(0,1fr)_11rem_5rem_auto] sm:items-end",
          div(
            cls := "flex-1",
            field(
              "Task",
              // When the taskId changes, clear the dependsOn for that slot and force the tasks signal to re-propagate.
              selectInput(
                t.state.signal.map(_.taskId),
                Observer[String] { next =>
                  t.state.update(ts => ts.copy(taskId = next, dependsOn = ts.dependsOn - next))
                  m.tasks.update(_.map(identity))
                },
                taskOptions,
              ),
            ),
          ),
          field(
            "Dipendente",
            selectInput(t.state.signal.map(_.employeeId), Observer[String](v => t.state.update(_.copy(employeeId = v))), employeeOptions),
          ),
          field(
            "Ore",
            textInput(t.state.signal.map(_.hours), Observer[String](v => t.state.update(_.copy(hours = v))), "", "number"),
          ),
          button(
            tpe := "button",
            cls := s"$btnDanger w-full justify-center sm:mb-0.5 sm:w-auto",
            "✕",
            onClick --> (_ => m.tasks.update(_.filterNot(_.key == t.key))),
          ),
        ),
        dependsOnChips(
          taskDependencyChoices(m, t),
          t.state.signal.map(_.dependsOn),
          (id, checked) => t.state.update(s => s.copy(dependsOn = if checked then s.dependsOn + id else s.dependsOn - id)),
        ),
      )

    /**
     * Per-task employee selects for a catalog-mode manufacturing: one row per template task, prefilled with the catalog default and overridable
     * before submit. Hidden until a valid template is selected.
     */
    def catalogTaskEmployeeOverrides(
        catalogIdSignal: Signal[String],
        currentMap: Signal[Map[String, String]],
        setEmployee: (String, String) => Unit,
    ): HtmlElement =
      div(
        child <-- catalogIdSignal.combineWith(manufacturingCatalogs.signal).map { case (rawId, catalogs) =>
          parseUuid(rawId).flatMap(id => catalogs.find(_.id == id)) match
            case None => emptyNode
            case Some(template) =>
              div(
                cls := "space-y-1 rounded-md border border-slate-200 p-2",
                div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Dipendente per task"),
                template.tasks.map { task =>
                  div(
                    cls := "grid grid-cols-1 gap-1 sm:grid-cols-[minmax(0,1fr)_minmax(0,14rem)] sm:items-center",
                    div(cls := "text-sm text-slate-700", s"${task.name} (${task.requiredHours}h)"),
                    selectInput(
                      currentMap.map(_.getOrElse(task.id.toString, "")),
                      Observer[String](v => setEmployee(task.id.toString, v)),
                      employeeOptions,
                    ),
                  )
                },
              )
        },
      )

    def catalogPreview(catalogIdSignal: Signal[String]): HtmlElement =
      div(
        child <-- catalogIdSignal.combineWith(manufacturingCatalogs.signal).map { case (rawId, catalogs) =>
          val selected = parseUuid(rawId).flatMap(id => catalogs.find(_.id == id))
          selected match
            case None => emptyNode
            case Some(template) =>
              div(
                cls := "rounded-md border border-slate-200 bg-slate-50 p-2 text-xs text-slate-600",
                div(cls := "font-medium text-slate-700", s"${template.code} · ${template.name}"),
                div(cls := "mt-1", template.tasks.map(task => s"${task.name} (${task.requiredHours}h)").mkString(", ")),
                div(
                  cls := "mt-1 text-slate-400",
                  s"${template.totalRequiredHours}h totali · ${template.dependencies.map(_.dependsOn.size).sum} dipendenze",
                ),
              )
        },
      )

    /** Selectable prerequisites for a manufacturing draft: the sibling drafts, labelled by their position and code/catalog name. */
    def mfgDependencyChoices(m: MfgDraft): Signal[List[(String, String)]] =
      mfgs.signal.combineWith(manufacturingCatalogs.signal).map { case (drafts, catalogs) =>
        drafts.zipWithIndex.filter((other, _) => other.key != m.key).map { (other, index) =>
          val ms = other.state.now()
          val labelText =
            if ms.mode == "catalog" then
              parseUuid(ms.catalogId)
                .flatMap(id => catalogs.find(_.id == id))
                .map(_.code)
                .getOrElse("lavorazione")
            else Some(ms.code.trim.nn).filter(_.nonEmpty).getOrElse("lavorazione")
          other.key.toString -> s"${index + 1}. $labelText"
        }
      }

    def renderMfg(m: MfgDraft): HtmlElement =
      val catalogIdSignal = m.state.signal.map(_.catalogId)
      // Shows the order-level work deadline as the effective value while the field is untouched;
      // clearing the field snaps back to the deadline, matching the submit-time fallback.
      val completionDisplay = m.state.signal
        .map(_.completionDate)
        .combineWith(createState.signal.map(_.workDeadline))
        .map((completion, deadline) => if completion.nonEmpty then completion else deadline)
      div(
        cls := "rounded-lg border border-slate-200 p-3",
        div(
          cls := "grid grid-cols-1 gap-2 sm:grid-cols-[11rem_10rem_auto] sm:items-end",
          field("Tipo", staticSelect(m.state.signal.map(_.mode), Observer[String](v => m.state.update(_.copy(mode = v))), manufacturingModeOptions)),
          field(
            "Completamento (default: deadline)",
            textInput(completionDisplay, Observer[String](v => m.state.update(_.copy(completionDate = v))), "", "date"),
          ),
          button(
            tpe := "button",
            cls := s"$btnDanger w-full justify-center sm:mb-0.5 sm:w-auto",
            "Rimuovi",
            onClick --> (_ => mfgs.update(_.filterNot(_.key == m.key))),
          ),
        ),
        div(
          cls := "mt-2",
          field(
            "Dipendente preferito",
            selectInput(m.state.signal.map(_.employeeId), Observer[String](v => m.state.update(_.copy(employeeId = v))), employeeOptions),
          ),
        ),
        dependsOnChips(
          mfgDependencyChoices(m),
          m.state.signal.map(_.dependsOn),
          (id, checked) => m.state.update(s => s.copy(dependsOn = if checked then s.dependsOn + id else s.dependsOn - id)),
        ),
        // `.distinct` is required: without it every keystroke into the fields below re-emits the
        // state Var, recreating this whole subtree and stealing focus from the active input.
        child <-- m.state.signal.map(_.mode).distinct.map {
          case "catalog" =>
            div(
              cls := "mt-2 space-y-2",
              field(
                "Lavorazione catalogo",
                selectInput(
                  catalogIdSignal,
                  // Selecting a template also prefills the per-task employees from the catalog defaults.
                  Observer[String] { v =>
                    m.state.update(_.copy(catalogId = v, taskEmployees = defaultTaskEmployees(v)))
                    mfgs.update(_.map(identity))
                  },
                  manufacturingCatalogOptions,
                ),
              ),
              catalogPreview(catalogIdSignal),
              catalogTaskEmployeeOverrides(
                catalogIdSignal,
                m.state.signal.map(_.taskEmployees),
                (taskId, employeeId) => m.state.update(s => s.copy(taskEmployees = s.taskEmployees.updated(taskId, employeeId))),
              ),
            )
          case _ =>
            div(
              cls := "mt-2 space-y-2",
              field(
                "Codice lavorazione",
                textInput(m.state.signal.map(_.code), Observer[String](v => m.state.update(_.copy(code = v))), "MFG-2026-001").amend(
                  onInput.mapToValue --> (_ => mfgs.update(_.map(identity))),
                ),
              ),
              field(
                "Descrizione lavorazione",
                textInput(m.state.signal.map(_.description), Observer[String](v => m.state.update(_.copy(description = v))), "Opzionale"),
              ),
              div(cls := "space-y-2", children <-- m.tasks.signal.split(_.key)((_, initial, _) => renderTask(m, initial))),
              button(tpe := "button", cls := btnSmall, "+ Task", onClick --> (_ => m.tasks.update(_ :+ newTask()))),
            )
        },
      )
    end renderMfg

    val createForm =
      div(
        cls := "space-y-4",
        div(
          cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
          field(
            "Numero ordine",
            textInput(createState.signal.map(_.number), Observer[String](v => createState.update(_.copy(number = v))), "ORD-2026-001").amend(
              onInput.mapToValue --> (_ => createState.update(_.copy(numberManuallyEdited = true))),
            ),
          ),
          field(
            "Cliente",
            searchableSelect(
              createState.signal.map(_.customerId),
              Observer[String](v => createState.update(_.copy(customerId = v))),
              customerOptions,
              "Cerca cliente…",
              maxResults = 20,
            ),
          ),
          field(
            "Creazione",
            textInput(createState.signal.map(_.creationDate), Observer[String](v => createState.update(_.copy(creationDate = v))), "", "date"),
          ),
          field(
            "Consegna cliente",
            textInput(createState.signal.map(_.deliveryDate), Observer[String](v => createState.update(_.copy(deliveryDate = v))), "", "date").amend(
              onInput.mapToValue --> { value =>
                if !createState.now().workDeadlineManuallyEdited then createState.update(_.copy(workDeadline = daysBefore(value, 5)))
              },
            ),
          ),
          field(
            "Deadline fine lavorazione",
            textInput(createState.signal.map(_.workDeadline), Observer[String](v => createState.update(_.copy(workDeadline = v))), "", "date").amend(
              onInput.mapToValue --> (_ => createState.update(_.copy(workDeadlineManuallyEdited = true))),
            ),
          ),
          field(
            "Priorità",
            staticSelect(createState.signal.map(_.priority), Observer[String](v => createState.update(_.copy(priority = v))), priorityOptions),
          ),
        ),
        field(
          "Descrizione ordine",
          textInput(createState.signal.map(_.description), Observer[String](v => createState.update(_.copy(description = v))), "Opzionale"),
        ),
        div(
          cls := "space-y-2",
          div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Lavorazioni"),
          children <-- mfgs.signal.split(_.key)((_, initial, _) => renderMfg(initial)),
          button(tpe := "button", cls := btnGhost, "+ Lavorazione", onClick --> (_ => mfgs.update(_ :+ newMfg()))),
        ),
        child.maybe <-- pageError.signal.map(_.map(errorBanner)),
        div(
          cls := "flex flex-col-reverse gap-2 border-t border-slate-100 pt-3 sm:flex-row sm:justify-end",
          button(tpe := "button", cls := s"$btnGhost justify-center", "Annulla", onClick --> (_ => showCreate.set(false))),
          button(
            tpe := "button",
            cls := s"$btnPrimary justify-center",
            "Crea ordine",
            disabled <-- createFormErrors.map(_.nonEmpty),
            onClick --> (_ => submitCreate()),
          ),
        ),
      )

    // ---- Edit modal rendering --------------------------------------------------------------------
    def editContent(order: OrderResponse): HtmlElement =
      // Viewers open the same modal in read-only mode ("Dettaglio"); mutations require the operator role.
      val editable = isEditable(order.status) && AuthService.currentHasRole(Role.Operator)
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
                  div(cls := "text-xs text-slate-400", statusLabel(t.status)),
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
                  "Previste",
                  textInput(
                    row.tracker.current.signal.map(_.expected),
                    Observer[String](v => row.tracker.current.update(_.copy(expected = v))),
                    "",
                    "number",
                  ),
                ),
                field(
                  "Fatte",
                  textInput(
                    row.tracker.current.signal.map(_.completed),
                    Observer[String](v => row.tracker.current.update(_.copy(completed = v))),
                    "",
                    "number",
                  ),
                ),
                button(
                  tpe := "button",
                  cls := s"$btnDanger w-full justify-center sm:mb-0.5 sm:w-auto",
                  disabled := m.tasks.size <= 1,
                  "✕",
                  onClick --> (_ => if m.tasks.size > 1 then applyStructural(ApiClient.removeManufacturingTask(order.id, m.id, t.id))),
                ),
              ),
              dependsOnChips(
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
                  s"${t.expectedHours}h previste · ${t.completedHours.getOrElse(0)}h fatte · ${statusLabel(t.status)}$suffix",
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
                    selectInput(addTaskState.signal.map(_.taskId), Observer[String](v => addTaskState.update(_.copy(taskId = v))), taskOptions),
                  ),
                ),
                field(
                  "Dipendente",
                  selectInput(addTaskState.signal.map(_.employee), Observer[String](v => addTaskState.update(_.copy(employee = v))), employeeOptions),
                ),
                field(
                  "Ore",
                  textInput(addTaskState.signal.map(_.hours), Observer[String](v => addTaskState.update(_.copy(hours = v))), "", "number"),
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
            div(cls := "font-medium text-slate-800", m.code),
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
                  dependsOnChips(
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
          if editable && order.manufacturings.size > 1 then
            div(
              cls := "mt-2 flex justify-end",
              button(
                tpe := "button",
                cls := s"$btnDanger w-full justify-center sm:w-auto",
                "Rimuovi lavorazione",
                onClick --> (_ => applyStructural(ApiClient.removeManufacturing(order.id, m.id))),
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
                      catalogPreview(addMfgState.signal.map(_.catalogId)),
                      catalogTaskEmployeeOverrides(
                        addMfgState.signal.map(_.catalogId),
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
                          selectInput(addMfgState.signal.map(_.taskId), Observer[String](v => addMfgState.update(_.copy(taskId = v))), taskOptions),
                        ),
                        field(
                          "Ore",
                          textInput(addMfgState.signal.map(_.hours), Observer[String](v => addMfgState.update(_.copy(hours = v))), "", "number"),
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
      div(
        cls := "fixed inset-0 z-40 items-start justify-center overflow-y-auto bg-slate-900/40 p-2 sm:p-4",
        cls <-- editing.signal.map(o => if o.isDefined then "flex" else "hidden"),
        div(
          cls := "mt-2 w-full max-w-2xl sm:mt-8",
          card(
            div(
              cls := "flex items-center justify-between border-b border-slate-100 px-4 py-3",
              h2(
                cls := "text-sm font-semibold text-slate-800",
                child.text <-- editing.signal.map(
                  _.map(o => s"${if isEditable(o.status) then "Modifica" else "Dettaglio"} ordine ${o.number}").getOrElse(""),
                ),
              ),
              button(cls := "text-slate-400 hover:text-slate-700", "✕", onClick --> (_ => editing.set(None))),
            ),
            div(cls := "p-4", child <-- editing.signal.map { case Some(o) => editContent(o); case None => emptyNode }),
          ),
        ),
      )

    // ---- Confirmation (ACK) modal ----------------------------------------------------------------
    val confirmModal =
      div(
        cls := "fixed inset-0 z-50 items-start justify-center overflow-y-auto bg-slate-900/50 p-2 sm:p-4",
        cls <-- confirm.signal.map(c => if c.isDefined then "flex" else "hidden"),
        div(
          cls := "mt-12 w-full max-w-md sm:mt-24",
          card(
            div(
              cls := "border-b border-slate-100 px-4 py-3",
              h2(cls := "text-sm font-semibold text-slate-800", child.text <-- confirm.signal.map(_.map(_.title).getOrElse(""))),
            ),
            div(
              cls := "space-y-4 p-4",
              p(cls := "text-sm text-slate-600", child.text <-- confirm.signal.map(_.map(_.message).getOrElse(""))),
              div(
                cls := "flex flex-col-reverse gap-2 sm:flex-row sm:justify-end",
                button(tpe := "button", cls := s"$btnGhost justify-center", "Annulla", onClick --> (_ => confirm.set(None))),
                button(
                  tpe := "button",
                  cls := s"$btnPrimary justify-center",
                  "Conferma",
                  onClick --> (_ => confirm.now().foreach { c => runConfirmed(c); confirm.set(None) }),
                ),
              ),
            ),
          ),
        ),
      )

    // ---- Row / table rendering -------------------------------------------------------------------
    def orderActionButtons(order: OrderResponse): List[HtmlElement] =
      val canOperate = AuthService.currentHasRole(Role.Operator)
      val detailsButton = button(
        tpe := "button",
        cls := btnSmall,
        if isEditable(order.status) && canOperate then "Modifica" else "Dettagli",
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
          cls := "flex flex-wrap gap-2 border-t border-slate-100 pt-3",
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

    val filterBar =
      div(
        cls := "mb-4 flex flex-wrap gap-2",
        statusFilters.map { case (value, labelText) =>
          button(
            tpe := "button",
            cls <-- statusFilter.signal.map(current => chipCls(current == value)),
            labelText,
            onClick --> (_ => statusFilter.set(value)),
          )
        },
      )

    def ordersTable(orders: List[OrderResponse]): HtmlElement =
      div(
        child <-- statusFilter.signal.map { filter =>
          val visible = orders.filter(o => filter.isEmpty || o.status == filter)
          if visible.isEmpty then emptyState("Nessun ordine con questo stato.")
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
                      th(cls := "px-4 py-2", "Ordine"),
                      th(cls := "px-4 py-2", "Cliente"),
                      th(cls := "px-4 py-2", "Stato"),
                      th(cls := "px-4 py-2", "Priorità"),
                      th(cls := "px-4 py-2", "Consegna"),
                      th(cls := "px-4 py-2", "Lavorazioni"),
                      th(cls := "px-4 py-2"),
                    ),
                  ),
                  tbody(visible.map(renderRow)),
                ),
              ),
            )
          end if
        },
      )

    // ---- Page layout -----------------------------------------------------------------------------
    div(
      manufacturingCatalogData --> {
        case Some(Right(list)) => manufacturingCatalogs.set(list)
        case _ => ()
      },
      ordersData --> {
        case Some(Right(list)) =>
          ordersSnapshot.set(list)
          if showCreate.now() && !createState.now().numberManuallyEdited then
            createState.update(_.copy(number = GeneratedCodes.next("ORD", list.map(_.number))))
        case _ => ()
      },
      div(
        cls := "mb-4 flex items-center justify-between",
        sectionTitle("Ordini"),
        roleGated(Role.Operator)(
          button(
            tpe := "button",
            cls := btnPrimary,
            "+ Nuovo ordine",
            onClick --> (_ =>
              resetCreate(); showCreate.set(true)
            ),
          ),
        ),
      ),
      child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mb-4", errorBanner(e)))),
      filterBar,
      card(
        cls := "overflow-hidden",
        renderResult(ordersData) { orders =>
          if orders.isEmpty then emptyState("Nessun ordine. Crea il primo ordine.")
          else ordersTable(orders)
        },
      ),
      modal(showCreate, "Nuovo ordine")(createForm),
      editModal,
      confirmModal,
    )
  end apply
end OrdersPage
