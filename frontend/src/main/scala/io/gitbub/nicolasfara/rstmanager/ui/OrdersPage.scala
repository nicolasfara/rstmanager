package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Orders: list with lifecycle transitions and inline priority edit, plus a nested create form. */
object OrdersPage:

  private val priorityOptions = List("normal" -> "Normale", "urgent" -> "Urgente")

  private def toIso(day: String): String = if day.isEmpty then "" else s"${day}T00:00:00.000Z"
  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** `java.util.UUID.randomUUID` is unavailable on Scala.js (needs SecureRandom); use the Web Crypto API. */
  private def randomUuid(): UUID = UUID.fromString(js.Dynamic.global.crypto.randomUUID().asInstanceOf[String]).nn

  /** Mutable draft holders backed by `Var`s so their input elements stay stable under `split`. */
  private final case class TaskDraft(key: Int, taskId: Var[String], hours: Var[String])
  private final case class MfgDraft(key: Int, code: Var[String], completionDate: Var[String], tasks: Var[List[TaskDraft]])

  def apply(): HtmlElement =
    val tick = Var(0)
    val pageError = Var(Option.empty[ApiError])
    val showCreate = Var(false)
    var keyCounter = 0
    def nextKey(): Int = { keyCounter += 1; keyCounter }

    val ordersData = loadable(tick.signal)(() => ApiClient.listOrders())
    val customersData = loadable(tick.signal)(() => ApiClient.listCustomers())
    val tasksData = loadable(tick.signal)(() => ApiClient.listTasks())

    val customersMap: Signal[Map[UUID, String]] = customersData.map {
      case Some(Right(list)) => list.map(c => c.id -> s"${c.name} ${c.surname}").toMap
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

    def newTask(): TaskDraft = TaskDraft(nextKey(), Var(""), Var("8"))
    def newMfg(): MfgDraft = MfgDraft(nextKey(), Var(""), Var(""), Var(List(newTask())))
    val mfgs = Var(List(newMfg()))

    def resetCreate(): Unit =
      number.set(""); customerId.set(""); creationDate.set(""); deliveryDate.set(""); promisedDate.set(""); priority.set("normal")
      mfgs.set(List(newMfg())); pageError.set(None)

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
            ManufacturingDto(m.code.now().trim.nn, toIso(m.completionDate.now()), "not_started", tasks, Nil, None, None, None, None)
          }
          val request = OrderRequest(
            number.now().trim.nn, cId, toIso(creationDate.now()), toIso(deliveryDate.now()), toIso(promisedDate.now()), priority.now(), manufacturings,
          )
          ApiClient.createOrder(request).foreach {
            case Right(_)  => resetCreate(); showCreate.set(false); tick.update(_ + 1)
            case Left(err) => pageError.set(Some(err))
          }

    // ---- Row actions -----------------------------------------------------------------------------
    def transition(id: UUID, action: String): Unit =
      ApiClient.orderTransition(id, TransitionRequest(action, None)).foreach {
        case Right(_)  => tick.update(_ + 1)
        case Left(err) => pageError.set(Some(err))
      }

    def changePriority(id: UUID, value: String): Unit =
      ApiClient.updateOrder(id, OrderUpdateRequest(Some(value), None)).foreach {
        case Right(_)  => tick.update(_ + 1)
        case Left(err) => pageError.set(Some(err))
      }

    def delete(id: UUID): Unit =
      ApiClient.deleteOrder(id, None).foreach {
        case Right(_)  => tick.update(_ + 1)
        case Left(err) => pageError.set(Some(err))
      }

    def transitionButtons(order: OrderResponse): List[HtmlElement] =
      val actions = order.status match
        case "in_progress" => List("suspend" -> "Sospendi", "complete" -> "Completa")
        case "suspended"   => List("reactivate" -> "Riattiva")
        case "completed"   => List("deliver" -> "Consegna")
        case _             => Nil
      actions.map { case (action, label) =>
        button(tpe := "button", cls := btnSmall, label, onClick --> (_ => transition(order.id, action)))
      }

    def prioritySelect(order: OrderResponse): HtmlElement =
      select(
        cls := "rounded-md border border-slate-300 px-2 py-1 text-xs",
        value := order.priority,
        priorityOptions.map { case (v, lbl) => option(value := v, lbl) },
        onChange.mapToValue --> (v => changePriority(order.id, v)),
      )

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

    // ---- Page layout -----------------------------------------------------------------------------
    div(
      div(
        cls := "mb-4 flex items-center justify-between",
        sectionTitle("Ordini"),
        button(tpe := "button", cls := btnPrimary, "+ Nuovo ordine", onClick --> (_ => { resetCreate(); showCreate.set(true) })),
      ),
      child.maybe <-- pageError.signal.map(_.map(e => div(cls := "mb-4", errorBanner(e)))),
      card(
        cls := "overflow-x-auto",
        renderResult(ordersData) { orders =>
          if orders.isEmpty then emptyState("Nessun ordine. Crea il primo ordine.")
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
              tbody(
                orders.map { order =>
                  val taskCount = order.manufacturings.map(_.tasks.size).sum
                  tr(
                    cls := "border-b border-slate-100 align-top last:border-0",
                    td(cls := "px-4 py-2 font-medium text-slate-800", order.number),
                    td(cls := "px-4 py-2 text-slate-600", child.text <-- customersMap.map(_.getOrElse(order.customerId, Formats.shortId(order.customerId)))),
                    td(cls := "px-4 py-2", statusBadge(order.status)),
                    td(cls := "px-4 py-2", prioritySelect(order)),
                    td(cls := "px-4 py-2 text-slate-500", Formats.date(order.deliveryDate)),
                    td(cls := "px-4 py-2 text-slate-500", s"${order.manufacturings.size} lav. · $taskCount task"),
                    td(
                      cls := "px-4 py-2",
                      div(
                        cls := "flex flex-wrap justify-end gap-2",
                        transitionButtons(order),
                        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => delete(order.id))),
                      ),
                    ),
                  )
                },
              ),
            )
        },
      ),
      modal(showCreate, "Nuovo ordine")(createForm),
    )
end OrdersPage
