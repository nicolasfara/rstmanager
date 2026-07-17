package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.Dtos.ApiError
import org.scalajs.dom

/** Centralized frontend error reporting: logs API/runtime failures and exposes the latest one to the app shell. */
object ErrorCenter:

  final case class ErrorReport(id: Int, context: String, error: ApiError)

  private var nextId = 0
  private var runtimeHandlersInstalled = false
  private val latest = Var(Option.empty[ErrorReport])

  val latestSignal: Signal[Option[ErrorReport]] = latest.signal

  def report(context: String, error: ApiError): Unit =
    nextId += 1
    val entry = ErrorReport(nextId, context, error)
    latest.set(Some(entry))
    log(entry)

  def reportTo(target: Var[Option[ApiError]], context: String)(error: ApiError): Unit =
    report(context, error)
    target.set(Some(error))

  def dismiss(id: Int): Unit =
    latest.update {
      case Some(entry) if entry.id == id => None
      case current => current
    }

  def installRuntimeHandlers(): Unit =
    if !runtimeHandlersInstalled then
      runtimeHandlersInstalled = true
      dom.window.addEventListener("error", event =>
        val dyn = event.asInstanceOf[js.Dynamic]
        val message = dynText(dyn.message).getOrElse("Errore frontend inatteso")
        val location = List(dynText(dyn.filename), dynText(dyn.lineno), dynText(dyn.colno)).flatten.mkString(":")
        val details = Option.when(location.nonEmpty)(location).toList
        report("Runtime frontend", ApiError("frontend-error", message, details)),
      )
      dom.window.addEventListener("unhandledrejection", event =>
        val dyn = event.asInstanceOf[js.Dynamic]
        val message = dynText(dyn.reason).getOrElse("Errore asincrono non gestito")
        report("Runtime frontend", ApiError("unhandled-rejection", message, Nil)),
      )

  private def log(entry: ErrorReport): Unit =
    val details =
      if entry.error.details.isEmpty then ""
      else s" Dettagli: ${entry.error.details.mkString(" | ")}"
    dom.console.error(s"[RST Manager] ${entry.context}: ${entry.error.code} - ${entry.error.message}$details")

  private def dynText(value: js.Dynamic): Option[String] =
    Option(value.asInstanceOf[Any])
      .map(_.toString)
      .filter(text => text.nonEmpty && text != "undefined" && text != "null")
end ErrorCenter
