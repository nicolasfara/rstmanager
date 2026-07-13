package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Orders: filterable list with lifecycle transitions, a nested create form, and a full edit modal covering
  * order data (priority, promised date, description), per-manufacturing description/status and add/remove of
  * manufacturings and tasks. Completing or delivering an order asks for an explicit acknowledgement first.
  */
object OrdersPage:

  private val priorityOptions = List("normal" -> "Normale", "urgent" -> "Urgente")

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

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** `java.util.UUID.randomUUID` is unavailable on Scala.js (needs SecureRandom); use the Web Crypto API. */
  private def randomUuid(): UUID = UUID.fromString(js.Dynamic.global.crypto.randomUUID().asInstanceOf[String]).nn

  /** Only in-progress and suspended orders accept data/task edits (see the domain `Order` aggregate). */
  private def isEditable(status: String): Boolean = status == "in_progress" || status == "suspended"

  /** Trims free text and collapses blank strings to `None` (an empty value clears the field). */
  private def normalizeStr(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  private def statusLabel(status: String): String = status match
    case "pending"     => "In attesa"
    case "in_progress" => "In corso"
    case "completed"   => "Completato"
    case "not_started" => "Non iniziata"
    case "paused"      => "In pausa"
    case "suspended"   => "Sospeso"
    case "delivered"   => "Consegnato"
    case "cancelled"   => "Annullato"
    case other         => other

  private val chipBase = "rounded-full px-3 py-1 text-xs font-medium border transition-colors"
  private def chipCls(active: Boolean): String =
    if active then s"$chipBase bg-slate-900 text-white border-slate-900"
    else s"$chipBase bg-white text-slate-600 border-slate-300 hover:bg-slate-100"

  private def priorityBadge(priority: String): HtmlElement = priority match
    case "urgent" => badge("Urgente", "bg-rose-50 text-rose-700 border-rose-200")
    case _        => badge("Normale", "bg-slate-50 text-slate-600 border-slate-200")

  private def readField(labelText: String, valueNode: Modifier[HtmlElement]): HtmlElement =
    div(
      cls := "space-y-0.5",
      div(cls := "text-xs font-medium text-slate-500", labelText),
      div(cls := "text-sm text-slate-800", valueNode),
    )

  /** Mutable draft holders backed by `Var`s so their input elements stay stable under `split`. */
  private final case class TaskDraft(key: Int, taskId: Var[String], hours: Var[String])
  private final case class MfgDraft(key: Int, code: Var[String], completionDate: Var[String], description: Var[String], tasks: Var[List[TaskDraft]])

  /** One editable scheduled-task row inside the edit modal; tracks original values to detect changes. */
  private final case class TaskEditRow(
      manufacturingId: UUID,
      taskId: UUID,
      originalExpected: Int,
      originalCompleted: Int,
      expected: Var[String],
      completed: Var[String],
  )

  /** One editable manufacturing row inside the edit modal; tracks original description/status to detect changes. */
  private final case class MfgEditRow(
      id: UUID,
      originalDescription: String,
      originalStatus: String,
      description: Var[String],
      status: Var[String],
  )

  /** A lifecycle transition awaiting an explicit user acknowledgement. */
  private final case class ConfirmData(orderId: UUID, action: String, title: String, message: String)

  def apply(): HtmlElement =
    val tick = Var(0)
    val pageError = Var(Option.empty[ApiError])
    val showCreate = Var(false)
    val statusFilter = Var("")
    var keyCounter = 0
    def nextKey(): Int = { keyCounter += 1; keyCounter }

    val ordersData = loadable(tick.signal)(() => ApiClient.listOrders())
    val customersData = loadable(tick.signal)(() => ApiClient.listCustomers())
    val tasksData = loadable(tick.signal)(() => ApiClient.listTasks())

    val customersMap: Signal[Map[UUID, String]] = customersData.map {
      case Some(Right(list)) => list.map(c => c.id -> s"${c.name} ${c.surname}").toMap
      case _                 => Map.empty
    }
    val tasksMap: Signal[Map[UUID, String]] = tasksData.map {
      case Some(Right(list)) => list.map(t => t.id -> t.name).toMap
      case _                 => Map.empty
    }
    val customerOptions: Signal[List[(String, String)]] = customersData.map {
      case Some(Right(list)) => ("" -> "— seleziona cliente —") :: list.map(c => c.id.toString -> s"${c.name} ${c.surname}")
      case _                 => List("" -> "—")
    }
    val taskOptions: Signal[List[(String, String)]] = tasksData.map {
      case Some(Right(list)) => ("" -> "— task —") :: list.map(t => t.id.toString -> s"${t.name} (${t.requiredHours}h)")
      case _                 => List("" -> "—")
    }

    // ---- Create form state -----------------------------------------------------------------------
    val number = Var("")
    val customerId = Var("")
    val creationDate = Var("")
    val deliveryDate = Var("")
    val promisedDate = Var("")
    val priority = Var("normal")
    val orderDescription = Var("")

    def newTask(): TaskDraft = TaskDraft(nextKey(), Var(""), Var("8"))
    def newMfg(): MfgDraft = MfgDraft(nextKey(), Var(""), Var(""), Var(""), Var(List(newTask())))
    val mfgs = Var(List(newMfg()))

    def resetCreate(): Unit =
      number.set(""); customerId.set(""); creationDate.set(""); deliveryDate.set(""); promisedDate.set(""); priority.set("normal")
      orderDescription.set(""); mfgs.set(List(newMfg())); pageError.set(None)

    def submitCreate(): Unit =
      parseUuid(customerId.now()) match
        case None => pageError.set(Some(ApiError("invalid-form", "Seleziona un cliente valido.", Nil)))
        case Some(cId) =>
          val manufacturings = mfgs.now().map { m =>
            val tasks = m.tasks.now().flatMap { t =>
              parseUuid(t.taskId.now()).map { taskId =>
                ScheduledTaskDto(randomUuid(), taskId, "pending", t.hours.now().toIntOption.getOrElse(0), Some(0), None)
              }
            }
            ManufacturingDto(m.code.now().trim.nn, toIso(m.completionDate.now()), "not_started", tasks, Nil, None, None, None, None, normalizeStr(m.description.now()))
          }
          val request = OrderRequest(
            number.now().trim.nn,
            cId,
            toIso(creationDate.now()),
            toIso(deliveryDate.now()),
            toIso(promisedDate.now()),
            priority.now(),
            manufacturings,
            normalizeStr(orderDescription.now()),
          )
          ApiClient.createOrder(request).foreach {
            case Right(_)  => resetCreate(); showCreate.set(false); tick.update(_ + 1)
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
    val addMfgCode = Var("")
    val addMfgDate = Var("")
    val addMfgDescription = Var("")
    val addMfgTaskId = Var("")
    val addMfgHours = Var("8")
    def resetAddMfg(): Unit =
      addMfgCode.set(""); addMfgDate.set(""); addMfgDescription.set(""); addMfgTaskId.set(""); addMfgHours.set("8")

    // Inline "add task" form (targets the manufacturing whose id is held here)
    val addTaskMfgId = Var(Option.empty[UUID])
    val addTaskId = Var("")
    val addTaskHours = Var("8")
    def resetAddTask(): Unit = { addTaskMfgId.set(None); addTaskId.set(""); addTaskHours.set("8") }

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
        MfgEditRow(m.id, desc, m.status, Var(desc), Var(m.status))
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
          case Right(_)  => effect()
        }
      }

    /** Applies an immediate structural change; on success refreshes the modal with the returned order and the table. */
    def applyStructural(effect: => Future[ApiClient.Result[OrderResponse]]): Unit =
      effect.foreach {
        case Right(updated) => openEdit(updated); tick.update(_ + 1)
        case Left(err)      => editError.set(Some(err))
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
        val newStatus = row.status.now()
        val descChanged = newMfgDescription.trim.nn != row.originalDescription
        val statusChanged = newStatus != row.originalStatus
        if descChanged || statusChanged then
          val request = ManufacturingUpdateRequest(
            if descChanged then Some(newMfgDescription) else None,
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
          case Right(_)  => editing.set(None); tick.update(_ + 1)
          case Left(err) => editError.set(Some(err))
        }
    }

    def submitAddMfg(): Unit = editing.now().foreach { order =>
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
        case Right(_)  => tick.update(_ + 1)
        case Left(err) => pageError.set(Some(err))
      }

    /** Completing/delivering is irreversible, so it goes through an acknowledgement modal first. */
    def requestTransition(order: OrderResponse, action: String): Unit = action match
      case "complete" =>
        confirm.set(Some(ConfirmData(
          order.id, action, "Completare l'ordine?",
          s"Stai per segnare l'ordine ${order.number} come completato. Verifica che tutte le lavorazioni siano concluse.",
        )))
      case "deliver" =>
        confirm.set(Some(ConfirmData(
          order.id, action, "Confermare la consegna?",
          s"Stai per segnare l'ordine ${order.number} come consegnato. L'operazione è definitiva.",
        )))
      case _ => transition(order.id, action)

    def delete(id: UUID): Unit =
      ApiClient.deleteOrder(id, None).foreach {
        case Right(_)  => tick.update(_ + 1)
        case Left(err) => pageError.set(Some(err))
      }

    def transitionButtons(order: OrderResponse): List[HtmlElement] =
      val actions = order.status match
        case "in_progress" => List("suspend" -> "Sospendi", "complete" -> "Completa")
        case "suspended"   => List("reactivate" -> "Riattiva", "complete" -> "Completa")
        case "completed"   => List("deliver" -> "Consegna")
        case _             => Nil
      actions.map { case (action, label) =>
        button(tpe := "button", cls := btnSmall, label, onClick --> (_ => requestTransition(order, action)))
      }

    // ---- Create form rendering -------------------------------------------------------------------
    def renderTask(m: MfgDraft, t: TaskDraft): HtmlElement =
      div(
        cls := "flex items-end gap-2",
        div(cls := "flex-1", field("Task", selectInput(t.taskId, taskOptions))),
        div(cls := "w-20", field("Ore", textInput(t.hours, inputType = "number"))),
        button(tpe := "button", cls := s"$btnDanger mb-0.5", "✕", onClick --> (_ => m.tasks.update(_.filterNot(_.key == t.key)))),
      )

    def renderMfg(m: MfgDraft): HtmlElement =
      div(
        cls := "rounded-lg border border-slate-200 p-3",
        div(
          cls := "flex items-end gap-2",
          div(cls := "flex-1", field("Codice lavorazione", textInput(m.code, "MFG-2026-001"))),
          div(cls := "w-40", field("Completamento", textInput(m.completionDate, inputType = "date"))),
          button(tpe := "button", cls := s"$btnDanger mb-0.5", "Rimuovi", onClick --> (_ => mfgs.update(_.filterNot(_.key == m.key)))),
        ),
        div(cls := "mt-2", field("Descrizione lavorazione", textInput(m.description, "Opzionale"))),
        div(cls := "mt-2 space-y-2", children <-- m.tasks.signal.split(_.key)((_, initial, _) => renderTask(m, initial))),
        button(tpe := "button", cls := s"$btnSmall mt-2", "+ Task", onClick --> (_ => m.tasks.update(_ :+ newTask()))),
      )

    val createForm =
      div(
        cls := "space-y-4",
        div(
          cls := "grid grid-cols-2 gap-3",
          field("Numero ordine", textInput(number, "ORD-2026-001")),
          field("Cliente", selectInput(customerId, customerOptions)),
          field("Creazione", textInput(creationDate, inputType = "date")),
          field("Consegna prevista", textInput(deliveryDate, inputType = "date")),
          field("Consegna promessa", textInput(promisedDate, inputType = "date")),
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
          cls := "flex justify-end gap-2 border-t border-slate-100 pt-3",
          button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => showCreate.set(false))),
          button(tpe := "button", cls := btnPrimary, "Crea ordine", onClick --> (_ => submitCreate())),
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
              cls := "flex items-end gap-2 border-t border-slate-100 pt-2 first:border-0 first:pt-0",
              div(
                cls := "flex-1",
                div(cls := "text-sm text-slate-700", nameNode),
                div(cls := "text-xs text-slate-400", statusLabel(t.status)),
              ),
              div(cls := "w-20", field("Previste", textInput(row.expected, inputType = "number"))),
              div(cls := "w-20", field("Fatte", textInput(row.completed, inputType = "number"))),
              button(
                tpe := "button",
                cls := s"$btnDanger mb-0.5",
                disabled := m.tasks.size <= 1,
                "✕",
                onClick --> (_ => if m.tasks.size > 1 then applyStructural(ApiClient.removeManufacturingTask(order.id, m.id, t.id))),
              ),
            )
          case _ =>
            div(
              cls := "flex items-center justify-between border-t border-slate-100 pt-2 text-sm first:border-0 first:pt-0",
              div(cls := "text-slate-700", nameNode),
              div(
                cls := "text-xs text-slate-500",
                s"${t.expectedHours}h previste · ${t.completedHours.getOrElse(0)}h fatte · ${statusLabel(t.status)}",
              ),
            )

      def addTaskForm(m: ManufacturingResponse): HtmlElement =
        div(
          cls := "mt-2",
          child <-- addTaskMfgId.signal.map { target =>
            if target.contains(m.id) then
              div(
                cls := "flex items-end gap-2 rounded-md border border-slate-200 bg-slate-50 p-2",
                div(cls := "flex-1", field("Task", selectInput(addTaskId, taskOptions))),
                div(cls := "w-20", field("Ore", textInput(addTaskHours, inputType = "number"))),
                button(tpe := "button", cls := s"$btnSmall mb-0.5", "Aggiungi", onClick --> (_ => submitAddTask(m.id))),
                button(tpe := "button", cls := s"$btnGhost mb-0.5", "Annulla", onClick --> (_ => resetAddTask())),
              )
            else button(tpe := "button", cls := btnSmall, "+ Task", onClick --> (_ => { resetAddTask(); addTaskMfgId.set(Some(m.id)) }))
          },
        )

      def renderMfgEdit(m: ManufacturingResponse): HtmlElement =
        div(
          cls := "rounded-lg border border-slate-200 p-3",
          div(
            cls := "mb-2 flex items-center justify-between gap-2",
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
                  cls := "mb-2 grid grid-cols-2 gap-2",
                  field("Descrizione", textInput(row.description, "Opzionale")),
                  field("Stato", staticSelect(row.status, mfgStatusOptions)),
                )
              case None => emptyNode
          else
            m.description.map(d => div(cls := "mb-2 text-sm text-slate-600", d)).getOrElse(emptyNode),
          div(cls := "space-y-2", m.tasks.map(t => renderTaskEdit(m, t))),
          if editable then addTaskForm(m) else emptyNode,
          if editable && order.manufacturings.size > 1 then
            div(
              cls := "mt-2 flex justify-end",
              button(tpe := "button", cls := btnDanger, "Rimuovi lavorazione", onClick --> (_ => applyStructural(ApiClient.removeManufacturing(order.id, m.id)))),
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
                  cls := "grid grid-cols-2 gap-2",
                  field("Codice lavorazione", textInput(addMfgCode, "MFG-2026-002")),
                  field("Completamento", textInput(addMfgDate, inputType = "date")),
                ),
                field("Descrizione", textInput(addMfgDescription, "Opzionale")),
                div(
                  cls := "grid grid-cols-[1fr_5rem] gap-2",
                  field("Primo task", selectInput(addMfgTaskId, taskOptions)),
                  field("Ore", textInput(addMfgHours, inputType = "number")),
                ),
                div(
                  cls := "flex justify-end gap-2",
                  button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => { showAddMfg.set(false); resetAddMfg() })),
                  button(tpe := "button", cls := btnSmall, "Aggiungi lavorazione", onClick --> (_ => submitAddMfg())),
                ),
              )
            else button(tpe := "button", cls := btnGhost, "+ Lavorazione", onClick --> (_ => { resetAddMfg(); showAddMfg.set(true) }))
          },
        )

      div(
        cls := "space-y-4",
        div(
          cls := "grid grid-cols-2 gap-3",
          readField("Numero", order.number),
          readField("Stato", statusBadge(order.status)),
          readField("Cliente", child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId)))),
          readField("Consegna prevista", Formats.date(order.deliveryDate)),
        ),
        if editable then
          div(
            cls := "space-y-3",
            div(
              cls := "grid grid-cols-2 gap-3",
              field("Priorità", staticSelect(editPriority, priorityOptions)),
              field("Consegna promessa", textInput(editPromised, inputType = "date")),
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
          ),
        div(
          cls := "space-y-2",
          div(cls := "text-xs font-semibold uppercase tracking-wide text-slate-500", "Lavorazioni e task"),
          div(cls := "space-y-2", order.manufacturings.map(renderMfgEdit)),
          if editable then addMfgForm else emptyNode,
        ),
        child.maybe <-- editError.signal.map(_.map(errorBanner)),
        div(
          cls := "flex justify-end gap-2 border-t border-slate-100 pt-3",
          button(tpe := "button", cls := btnGhost, "Chiudi", onClick --> (_ => editing.set(None))),
          if editable then button(tpe := "button", cls := btnPrimary, "Salva modifiche", onClick --> (_ => saveEdit())) else emptyNode,
        ),
      )

    val editModal =
      div(
        cls := "fixed inset-0 z-40 items-start justify-center overflow-y-auto bg-slate-900/40 p-4",
        cls <-- editing.signal.map(o => if o.isDefined then "flex" else "hidden"),
        div(
          cls := "mt-8 w-full max-w-2xl",
          card(
            div(
              cls := "flex items-center justify-between border-b border-slate-100 px-4 py-3",
              h2(
                cls := "text-sm font-semibold text-slate-800",
                child.text <-- editing.signal.map(_.map(o => s"${if isEditable(o.status) then "Modifica" else "Dettaglio"} ordine ${o.number}").getOrElse("")),
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
        cls := "fixed inset-0 z-50 items-start justify-center overflow-y-auto bg-slate-900/50 p-4",
        cls <-- confirm.signal.map(c => if c.isDefined then "flex" else "hidden"),
        div(
          cls := "mt-24 w-full max-w-md",
          card(
            div(cls := "border-b border-slate-100 px-4 py-3", h2(cls := "text-sm font-semibold text-slate-800", child.text <-- confirm.signal.map(_.map(_.title).getOrElse("")))),
            div(
              cls := "space-y-4 p-4",
              p(cls := "text-sm text-slate-600", child.text <-- confirm.signal.map(_.map(_.message).getOrElse(""))),
              div(
                cls := "flex justify-end gap-2",
                button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => confirm.set(None))),
                button(
                  tpe := "button",
                  cls := btnPrimary,
                  "Conferma",
                  onClick --> (_ => confirm.now().foreach { c => transition(c.orderId, c.action); confirm.set(None) }),
                ),
              ),
            ),
          ),
        ),
      )

    // ---- Row / table rendering -------------------------------------------------------------------
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
            button(tpe := "button", cls := btnSmall, if isEditable(order.status) then "Modifica" else "Dettagli", onClick --> (_ => openEdit(order))),
            transitionButtons(order),
            button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => delete(order.id))),
          ),
        ),
      )

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
            )
        },
      )

    // ---- Page layout -----------------------------------------------------------------------------
    div(
      div(
        cls := "mb-4 flex items-center justify-between",
        sectionTitle("Ordini"),
        button(tpe := "button", cls := btnPrimary, "+ Nuovo ordine", onClick --> (_ => { resetCreate(); showCreate.set(true) })),
      ),
      child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mb-4", errorBanner(e)))),
      filterBar,
      card(
        cls := "overflow-x-auto",
        renderResult(ordersData) { orders =>
          if orders.isEmpty then emptyState("Nessun ordine. Crea il primo ordine.")
          else ordersTable(orders)
        },
      ),
      modal(showCreate, "Nuovo ordine")(createForm),
      editModal,
      confirmModal,
    )
end OrdersPage
