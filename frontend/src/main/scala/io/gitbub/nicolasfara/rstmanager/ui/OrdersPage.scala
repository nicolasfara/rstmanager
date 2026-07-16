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
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/**
 * Orders: filterable list with lifecycle transitions, a nested create form, and a full edit modal covering order data (priority, work deadline,
 * description), per-manufacturing description/deadline/status and add/remove of manufacturings and tasks. Completing or delivering an order asks for
 * an explicit acknowledgement first.
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
    parseDay(day)
      .map { date =>
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

  /** Mutable draft holders backed by `Var`s so their input elements stay stable under `split`. */
  private final case class TaskDraft(key: Int, taskId: Var[String], hours: Var[String])
  private final case class MfgDraft(
      key: Int,
      mode: Var[String],
      catalogId: Var[String],
      code: Var[String],
      completionDate: Var[String],
      description: Var[String],
      tasks: Var[List[TaskDraft]],
      employeeId: Var[String],
  )

  /** One editable scheduled-task row inside the edit modal; tracks original values to detect changes. */
  private final case class TaskEditRow(
      manufacturingId: UUID,
      taskId: UUID,
      originalExpected: Int,
      originalCompleted: Int,
      expected: Var[String],
      completed: Var[String],
  )

  /** One editable manufacturing row inside the edit modal; tracks original values to detect changes. */
  private final case class MfgEditRow(
      id: UUID,
      originalDescription: String,
      originalCompletionDate: String,
      originalStatus: String,
      description: Var[String],
      completionDate: Var[String],
      status: Var[String],
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

    val ordersData = loadable(AppBus.ticks)(() => ApiClient.listOrders())
    val customersData = loadable(AppBus.ticks)(() => ApiClient.listCustomers())
    val tasksData = loadable(AppBus.ticks)(() => ApiClient.listTasks())
    val manufacturingCatalogData = loadable(AppBus.ticks)(() => ApiClient.listManufacturingCatalog())
    val employeesData = loadable(AppBus.ticks)(() => ApiClient.listEmployees())
    val ordersSnapshot = Var(List.empty[OrderResponse])
    val manufacturingCatalogs = Var(List.empty[ManufacturingCatalogResponse])

    val customersMap: Signal[Map[UUID, String]] = customersData.map {
      case Some(Right(list)) => list.map(c => c.id -> s"${c.name} ${c.surname}").toMap
      case _ => Map.empty
    }
    val tasksMap: Signal[Map[UUID, String]] = tasksData.map {
      case Some(Right(list)) => list.map(t => t.id -> t.name).toMap
      case _ => Map.empty
    }
    val customerOptions: Signal[List[(String, String)]] = customersData.map {
      case Some(Right(list)) => ("" -> "— seleziona cliente —") :: list.map(c => c.id.toString -> s"${c.name} ${c.surname}")
      case _ => List("" -> "—")
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
    val number = Var(GeneratedCodes.next("ORD", Nil))
    val numberManuallyEdited = Var(false)
    val customerId = Var("")
    val creationDate = Var(todayDay())
    val deliveryDate = Var("")
    val workDeadline = Var("")
    val workDeadlineManuallyEdited = Var(false)
    val priority = Var("normal")
    val orderDescription = Var("")

    def newTask(): TaskDraft = TaskDraft(nextKey(), Var(""), Var("8"))
    def newMfg(): MfgDraft = MfgDraft(nextKey(), Var("custom"), Var(""), Var(""), Var(""), Var(""), Var(List(newTask())), Var(""))
    val mfgs = Var(List(newMfg()))

    def resetCreate(): Unit =
      numberManuallyEdited.set(false)
      workDeadlineManuallyEdited.set(false)
      number.set(GeneratedCodes.next("ORD", ordersSnapshot.now().map(_.number)))
      customerId.set(""); creationDate.set(todayDay()); deliveryDate.set(""); workDeadline.set(""); priority.set("normal")
      orderDescription.set(""); mfgs.set(List(newMfg())); pageError.set(None)

    def catalogById(rawId: String): Option[ManufacturingCatalogResponse] =
      parseUuid(rawId).flatMap(id => manufacturingCatalogs.now().find(_.id == id))

    def fromCatalog(template: ManufacturingCatalogResponse, completionDate: String, employeeId: String): ManufacturingDto =
      ManufacturingDto(
        template.code,
        toIso(completionDate),
        "not_started",
        template.tasks.map(task => ScheduledTaskDto(randomUuid(), task.id, "pending", task.requiredHours, Some(0), None)),
        template.dependencies.map(dependency => TaskDependencyDto(dependency.taskId, dependency.dependsOn)),
        None,
        None,
        None,
        None,
        template.description,
        parseUuid(employeeId),
      )

    def fromCustom(m: MfgDraft): ManufacturingDto =
      val tasks = m.tasks.now().flatMap { t =>
        parseUuid(t.taskId.now()).map { taskId =>
          ScheduledTaskDto(randomUuid(), taskId, "pending", t.hours.now().toIntOption.getOrElse(0), Some(0), None)
        }
      }
      ManufacturingDto(
        m.code.now().trim.nn,
        toIso(m.completionDate.now()),
        "not_started",
        tasks,
        Nil,
        None,
        None,
        None,
        None,
        normalizeStr(m.description.now()),
        parseUuid(m.employeeId.now()),
      )

    def manufacturingFromDraft(m: MfgDraft): Either[ApiError, ManufacturingDto] =
      if m.mode.now() == "catalog" then
        catalogById(m.catalogId.now()).toRight(ApiError("invalid-form", "Seleziona una lavorazione a catalogo valida.", Nil)).map { template =>
          fromCatalog(template, m.completionDate.now(), m.employeeId.now())
        }
      else Right(fromCustom(m))

    def collectManufacturings(): Either[ApiError, List[ManufacturingDto]] =
      mfgs.now().foldLeft(Right(List.empty): Either[ApiError, List[ManufacturingDto]]) { (acc, draft) =>
        acc.flatMap(list => manufacturingFromDraft(draft).map(list :+ _))
      }

    def submitCreate(): Unit =
      parseUuid(customerId.now()) match
        case None => pageError.set(Some(ApiError("invalid-form", "Seleziona un cliente valido.", Nil)))
        case Some(cId) =>
          if deliveryDate.now().isEmpty then pageError.set(Some(ApiError("invalid-form", "Inserisci la consegna cliente.", Nil)))
          else if workDeadline.now().isEmpty then pageError.set(Some(ApiError("invalid-form", "Inserisci la deadline di fine lavorazione.", Nil)))
          else
            collectManufacturings() match
              case Left(err) => pageError.set(Some(err))
              case Right(manufacturings) =>
                val customerDeliveryDate = toIso(deliveryDate.now())
                val workDeadlineDate = toIso(workDeadline.now())
                val request = OrderRequest(
                  number.now().trim.nn,
                  cId,
                  toIso(creationDate.now()),
                  customerDeliveryDate,
                  workDeadlineDate,
                  priority.now(),
                  manufacturings,
                  normalizeStr(orderDescription.now()),
                )
                ApiClient.createOrder(request).foreach {
                  case Right(_) => resetCreate(); showCreate.set(false); AppBus.mutated()
                  case Left(err) => pageError.set(Some(err))
                }

    // ---- Edit modal state ------------------------------------------------------------------------
    val editing = Var(Option.empty[OrderResponse])
    val editPriority = Var("normal")
    val editPromised = Var("")
    val editDescription = Var("")
    val editTasks = Var(List.empty[TaskEditRow])
    val editMfgs = Var(List.empty[MfgEditRow])
    val editError = Var(Option.empty[ApiError])

    // Inline "add manufacturing" form
    val showAddMfg = Var(false)
    val addMfgMode = Var("custom")
    val addMfgCatalogId = Var("")
    val addMfgCode = Var("")
    val addMfgDate = Var("")
    val addMfgDescription = Var("")
    val addMfgTaskId = Var("")
    val addMfgHours = Var("8")
    val addMfgEmployee = Var("")
    def resetAddMfg(): Unit =
      addMfgMode.set("custom"); addMfgCatalogId.set(""); addMfgCode.set(""); addMfgDate.set(""); addMfgDescription.set(""); addMfgTaskId.set("")
      addMfgHours.set("8"); addMfgEmployee.set("")

    // Inline "add task" form (targets the manufacturing whose id is held here)
    val addTaskMfgId = Var(Option.empty[UUID])
    val addTaskId = Var("")
    val addTaskHours = Var("8")
    def resetAddTask(): Unit =
      addTaskMfgId.set(None); addTaskId.set(""); addTaskHours.set("8")

    def openEdit(order: OrderResponse): Unit =
      editPriority.set(order.priority)
      editPromised.set(order.promisedDeliveryDate.map(dayOf).getOrElse(""))
      editDescription.set(order.description.getOrElse(""))
      editTasks.set(order.manufacturings.flatMap { m =>
        m.tasks.map { t =>
          val completed = t.completedHours.getOrElse(0)
          TaskEditRow(m.id, t.id, t.expectedHours, completed, Var(t.expectedHours.toString), Var(completed.toString))
        }
      })
      editMfgs.set(order.manufacturings.map { m =>
        val desc = m.description.getOrElse("")
        val completionDate = dayOf(m.completionDate)
        MfgEditRow(m.id, desc, completionDate, m.status, Var(desc), Var(completionDate), Var(m.status))
      })
      editError.set(None)
      showAddMfg.set(false)
      resetAddMfg()
      resetAddTask()
      editing.set(Some(order))

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
        case Right(updated) => openEdit(updated); AppBus.mutated()
        case Left(err) => editError.set(Some(err))
      }

    def saveEdit(): Unit = editing.now().foreach { order =>
      val newPriority = editPriority.now()
      val newPromisedDay = editPromised.now()
      val newDescription = editDescription.now()
      val originalPromisedDay = order.promisedDeliveryDate.map(dayOf).getOrElse("")
      val priorityChanged = newPriority != order.priority
      val promisedChanged = newPromisedDay.nonEmpty && newPromisedDay != originalPromisedDay
      val descriptionChanged = newDescription.trim.nn != order.description.getOrElse("")
      val orderUpdate: List[() => Future[ApiClient.Result[Unit]]] =
        if priorityChanged || promisedChanged || descriptionChanged then
          val request = OrderUpdateRequest(
            if priorityChanged then Some(newPriority) else None,
            if promisedChanged then Some(toIso(newPromisedDay)) else None,
            if descriptionChanged then Some(newDescription) else None,
          )
          List(() => ApiClient.updateOrder(order.id, request).map(_.map(_ => ())))
        else Nil

      val mfgUpdates: List[() => Future[ApiClient.Result[Unit]]] = editMfgs.now().flatMap { row =>
        val newMfgDescription = row.description.now()
        val newCompletionDay = row.completionDate.now()
        val newStatus = row.status.now()
        val descChanged = newMfgDescription.trim.nn != row.originalDescription
        val completionDateChanged = newCompletionDay.nonEmpty && newCompletionDay != row.originalCompletionDate
        val statusChanged = newStatus != row.originalStatus
        if descChanged || completionDateChanged || statusChanged then
          val request = ManufacturingUpdateRequest(
            if descChanged then Some(newMfgDescription) else None,
            if completionDateChanged then Some(toIso(newCompletionDay)) else None,
            if statusChanged then Some(newStatus) else None,
            None,
          )
          List(() => ApiClient.updateManufacturing(order.id, row.id, request).map(_.map(_ => ())))
        else Nil
      }

      val taskUpdates: List[() => Future[ApiClient.Result[Unit]]] = editTasks.now().flatMap { row =>
        val newExpected = row.expected.now().toIntOption
        val newCompleted = row.completed.now().toIntOption
        val expectedChanged = newExpected.exists(_ != row.originalExpected)
        val completedChanged = newCompleted.exists(_ != row.originalCompleted)
        if expectedChanged || completedChanged then
          val request = TaskProgressUpdateRequest(if completedChanged then newCompleted else None, if expectedChanged then newExpected else None)
          List(() => ApiClient.updateScheduledTask(order.id, row.manufacturingId, row.taskId, request).map(_.map(_ => ())))
        else Nil
      }

      val effects = orderUpdate ++ mfgUpdates ++ taskUpdates
      if effects.isEmpty then editing.set(None)
      else
        runSequential(effects).foreach {
          case Right(_) => editing.set(None); AppBus.mutated()
          case Left(err) => editError.set(Some(err))
        }
    }

    def submitAddMfg(): Unit = editing.now().foreach { order =>
      if addMfgMode.now() == "catalog" then
        catalogById(addMfgCatalogId.now()) match
          case None => editError.set(Some(ApiError("invalid-form", "Seleziona una lavorazione a catalogo valida.", Nil)))
          case Some(template) => applyStructural(ApiClient.addManufacturing(order.id, fromCatalog(template, addMfgDate.now(), addMfgEmployee.now())))
      else
        parseUuid(addMfgTaskId.now()) match
          case None => editError.set(Some(ApiError("invalid-form", "Seleziona un task valido per la nuova lavorazione.", Nil)))
          case Some(taskId) =>
            val task = ScheduledTaskDto(randomUuid(), taskId, "pending", addMfgHours.now().toIntOption.getOrElse(0), Some(0), None)
            val dto = ManufacturingDto(
              addMfgCode.now().trim.nn,
              toIso(addMfgDate.now()),
              "not_started",
              List(task),
              Nil,
              None,
              None,
              None,
              None,
              normalizeStr(addMfgDescription.now()),
              parseUuid(addMfgEmployee.now()),
            )
            applyStructural(ApiClient.addManufacturing(order.id, dto))
    }

    def submitAddTask(mfgId: UUID): Unit = editing.now().foreach { order =>
      parseUuid(addTaskId.now()) match
        case None => editError.set(Some(ApiError("invalid-form", "Seleziona un task valido.", Nil)))
        case Some(taskId) =>
          applyStructural(ApiClient.addManufacturingTask(order.id, mfgId, AddTaskRequest(taskId, addTaskHours.now().toIntOption.getOrElse(0), Nil)))
    }

    // ---- Row actions -----------------------------------------------------------------------------
    val confirm = Var(Option.empty[ConfirmData])

    def transition(id: UUID, action: String): Unit =
      ApiClient.orderTransition(id, TransitionRequest(action, None)).foreach {
        case Right(_) => AppBus.mutated()
        case Left(err) => pageError.set(Some(err))
      }

    def cancelOrder(id: UUID): Unit =
      ApiClient.deleteOrder(id, None).foreach {
        case Right(_) => AppBus.mutated()
        case Left(err) => pageError.set(Some(err))
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
        case "completed" => List("deliver" -> "Consegna")
        case "cancelled" => List("reopen" -> "Riapri")
        case _ => Nil
      actions.map { case (action, label) =>
        button(tpe := "button", cls := btnSmall, label, onClick --> (_ => requestTransition(order, action)))
      }

    // ---- Create form rendering -------------------------------------------------------------------
    def renderTask(m: MfgDraft, t: TaskDraft): HtmlElement =
      div(
        cls := "grid grid-cols-1 gap-2 sm:grid-cols-[minmax(0,1fr)_5rem_auto] sm:items-end",
        div(cls := "flex-1", field("Task", selectInput(t.taskId, taskOptions))),
        field("Ore", textInput(t.hours, "", "number")),
        button(
          tpe := "button",
          cls := s"$btnDanger w-full justify-center sm:mb-0.5 sm:w-auto",
          "✕",
          onClick --> (_ => m.tasks.update(_.filterNot(_.key == t.key))),
        ),
      )

    def catalogPreview(catalogId: Var[String]): HtmlElement =
      div(
        child <-- catalogId.signal.combineWith(manufacturingCatalogs.signal).map { case (rawId, catalogs) =>
          val selected = parseUuid(rawId).flatMap(id => catalogs.find(_.id == id))
          selected match
            case None => emptyNode
            case Some(template) =>
              div(
                cls := "rounded-md border border-slate-200 bg-slate-50 p-2 text-xs text-slate-600",
                div(cls := "font-medium text-slate-700", s"${template.code} · ${template.name}"),
                div(cls := "mt-1", template.tasks.map(task => s"${task.name} (${task.requiredHours}h)").mkString(", ")),
                div(cls := "mt-1 text-slate-400", s"${template.totalRequiredHours}h totali · ${template.dependencies.map(_.dependsOn.size).sum} dipendenze"),
              )
        },
      )

    def renderMfg(m: MfgDraft): HtmlElement =
      div(
        cls := "rounded-lg border border-slate-200 p-3",
        div(
          cls := "grid grid-cols-1 gap-2 sm:grid-cols-[11rem_10rem_auto] sm:items-end",
          field("Tipo", staticSelect(m.mode, manufacturingModeOptions)),
          field("Completamento", textInput(m.completionDate, "", "date")),
          button(
            tpe := "button",
            cls := s"$btnDanger w-full justify-center sm:mb-0.5 sm:w-auto",
            "Rimuovi",
            onClick --> (_ => mfgs.update(_.filterNot(_.key == m.key))),
          ),
        ),
        div(cls := "mt-2", field("Dipendente preferito", selectInput(m.employeeId, employeeOptions))),
        child <-- m.mode.signal.map {
          case "catalog" =>
            div(
              cls := "mt-2 space-y-2",
              field("Lavorazione catalogo", selectInput(m.catalogId, manufacturingCatalogOptions)),
              catalogPreview(m.catalogId),
            )
          case _ =>
            div(
              cls := "mt-2 space-y-2",
              field("Codice lavorazione", textInput(m.code, "MFG-2026-001")),
              field("Descrizione lavorazione", textInput(m.description, "Opzionale")),
              div(cls := "space-y-2", children <-- m.tasks.signal.split(_.key)((_, initial, _) => renderTask(m, initial))),
              button(tpe := "button", cls := btnSmall, "+ Task", onClick --> (_ => m.tasks.update(_ :+ newTask()))),
            )
        },
      )

    val createForm =
      div(
        cls := "space-y-4",
        div(
          cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
          field("Numero ordine", textInput(number, "ORD-2026-001").amend(onInput.mapToValue --> (_ => numberManuallyEdited.set(true)))),
          field("Cliente", selectInput(customerId, customerOptions)),
          field("Creazione", textInput(creationDate, "", "date")),
          field(
            "Consegna cliente",
            textInput(deliveryDate, "", "date").amend(
              onInput.mapToValue --> { value =>
                if !workDeadlineManuallyEdited.now() then workDeadline.set(daysBefore(value, 5))
              },
            ),
          ),
          field(
            "Deadline fine lavorazione",
            textInput(workDeadline, "", "date").amend(onInput.mapToValue --> (_ => workDeadlineManuallyEdited.set(true))),
          ),
          field("Priorità", staticSelect(priority, priorityOptions)),
        ),
        field("Descrizione ordine", textInput(orderDescription, "Opzionale")),
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
          button(tpe := "button", cls := s"$btnPrimary justify-center", "Crea ordine", onClick --> (_ => submitCreate())),
        ),
      )

    // ---- Edit modal rendering --------------------------------------------------------------------
    def editContent(order: OrderResponse): HtmlElement =
      val editable = isEditable(order.status)
      val rowsByTask: Map[UUID, TaskEditRow] = editTasks.now().map(row => row.taskId -> row).toMap
      val mfgById: Map[UUID, MfgEditRow] = editMfgs.now().map(row => row.id -> row).toMap

      def renderTaskEdit(m: ManufacturingResponse, t: ScheduledTaskDto): HtmlElement =
        val nameNode = child.text <-- tasksMap.map(_.getOrElse(t.taskId, Formats.shortId(t.taskId)))
        rowsByTask.get(t.id) match
          case Some(row) if editable =>
            div(
              cls := "grid grid-cols-1 gap-2 border-t border-slate-100 pt-2 first:border-0 first:pt-0 sm:grid-cols-[minmax(0,1fr)_5rem_5rem_auto] sm:items-end",
              div(
                cls := "flex-1",
                div(cls := "text-sm text-slate-700", nameNode),
                div(cls := "text-xs text-slate-400", statusLabel(t.status)),
              ),
              field("Previste", textInput(row.expected, "", "number")),
              field("Fatte", textInput(row.completed, "", "number")),
              button(
                tpe := "button",
                cls := s"$btnDanger w-full justify-center sm:mb-0.5 sm:w-auto",
                disabled := m.tasks.size <= 1,
                "✕",
                onClick --> (_ => if m.tasks.size > 1 then applyStructural(ApiClient.removeManufacturingTask(order.id, m.id, t.id))),
              ),
            )
          case _ =>
            div(
              cls := "flex flex-col gap-1 border-t border-slate-100 pt-2 text-sm first:border-0 first:pt-0 sm:flex-row sm:items-center sm:justify-between",
              div(cls := "text-slate-700", nameNode),
              div(
                cls := "text-xs text-slate-500",
                s"${t.expectedHours}h previste · ${t.completedHours.getOrElse(0)}h fatte · ${statusLabel(t.status)}",
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
                cls := "grid grid-cols-1 gap-2 rounded-md border border-slate-200 bg-slate-50 p-2 sm:grid-cols-[minmax(0,1fr)_5rem_auto_auto] sm:items-end",
                div(cls := "flex-1", field("Task", selectInput(addTaskId, taskOptions))),
                field("Ore", textInput(addTaskHours, "", "number")),
                button(tpe := "button", cls := s"$btnSmall w-full justify-center sm:mb-0.5 sm:w-auto", "Aggiungi", onClick --> (_ => submitAddTask(m.id))),
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
                  cls := "mb-2 grid grid-cols-1 gap-2 sm:grid-cols-3",
                  field("Descrizione", textInput(row.description, "Opzionale")),
                  field("Deadline lavorazione", textInput(row.completionDate, "", "date")),
                  field("Stato", staticSelect(row.status, mfgStatusOptions)),
                )
              case None => emptyNode
          else m.description.map(d => div(cls := "mb-2 text-sm text-slate-600", d)).getOrElse(emptyNode),
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
                  field("Tipo", staticSelect(addMfgMode, manufacturingModeOptions)),
                  field("Completamento", textInput(addMfgDate, "", "date")),
                ),
                field("Dipendente preferito", selectInput(addMfgEmployee, employeeOptions)),
                child <-- addMfgMode.signal.map {
                  case "catalog" =>
                    div(
                      cls := "space-y-2",
                      field("Lavorazione catalogo", selectInput(addMfgCatalogId, manufacturingCatalogOptions)),
                      catalogPreview(addMfgCatalogId),
                    )
                  case _ =>
                    div(
                      cls := "space-y-2",
                      field("Codice lavorazione", textInput(addMfgCode, "MFG-2026-002")),
                      field("Descrizione", textInput(addMfgDescription, "Opzionale")),
                      div(
                        cls := "grid grid-cols-1 gap-2 sm:grid-cols-[minmax(0,1fr)_5rem]",
                        field("Primo task", selectInput(addMfgTaskId, taskOptions)),
                        field("Ore", textInput(addMfgHours, "", "number")),
                      ),
                    )
                },
                div(
                  cls := "flex flex-col-reverse gap-2 sm:flex-row sm:justify-end",
                  button(
                    tpe := "button",
                    cls := s"$btnGhost justify-center",
                    "Annulla",
                    onClick --> (_ =>
                      showAddMfg.set(false); resetAddMfg()
                    ),
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
              field("Priorità", staticSelect(editPriority, priorityOptions)),
              field("Deadline fine lavorazione", textInput(editPromised, "", "date")),
            ),
            field("Descrizione ordine", textInput(editDescription, "Opzionale")),
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
          if editable then button(tpe := "button", cls := s"$btnPrimary justify-center", "Salva modifiche", onClick --> (_ => saveEdit())) else emptyNode,
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
      val detailsButton = button(
        tpe := "button",
        cls := btnSmall,
        if isEditable(order.status) then "Modifica" else "Dettagli",
        onClick --> (_ => openEdit(order)),
      )
      val cancelButton =
        if order.status == "cancelled" then Nil
        else List(button(tpe := "button", cls := btnDanger, "Annulla ordine", onClick --> (_ => requestTransition(order, "cancel"))))
      detailsButton :: transitionButtons(order) ++ cancelButton

    def renderOrderCard(order: OrderResponse): HtmlElement =
      val taskCount = order.manufacturings.map(_.tasks.size).sum
      div(
        cls := "space-y-3 p-4",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "break-words font-medium text-slate-800", order.number),
            div(cls := "mt-1 text-sm text-slate-600", child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId)))),
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
              )
            )
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
          if showCreate.now() && !numberManuallyEdited.now() then number.set(GeneratedCodes.next("ORD", list.map(_.number)))
        case _ => ()
      },
      div(
        cls := "mb-4 flex items-center justify-between",
        sectionTitle("Ordini"),
        button(
          tpe := "button",
          cls := btnPrimary,
          "+ Nuovo ordine",
          onClick --> (_ =>
            resetCreate(); showCreate.set(true)
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
