package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.ApiError

/** Minimal, reusable Tailwind-styled building blocks shared by all pages. */
object Components:

  // ---- Class tokens ------------------------------------------------------------------------------

  val btnPrimary =
    "inline-flex items-center gap-1.5 rounded-md bg-slate-900 px-3 py-1.5 text-sm font-medium text-white hover:bg-slate-700 disabled:opacity-50"
  val btnGhost =
    "inline-flex items-center gap-1.5 rounded-md border border-slate-300 px-3 py-1.5 text-sm font-medium text-slate-700 hover:bg-slate-100"
  val btnSmall = "inline-flex items-center rounded-md border border-slate-300 px-2 py-1 text-xs font-medium text-slate-600 hover:bg-slate-100"
  val btnDanger = "inline-flex items-center rounded-md border border-rose-300 px-2 py-1 text-xs font-medium text-rose-700 hover:bg-rose-50"
  private val inputCls =
    "w-full rounded-md border border-slate-300 px-2.5 py-1.5 text-sm text-slate-800 focus:border-slate-500 focus:outline-none focus:ring-1 focus:ring-slate-500"
  private val labelCls = "mb-1 block text-xs font-medium text-slate-500"

  // ---- Layout primitives -------------------------------------------------------------------------

  def card(mods: Modifier[HtmlElement]*): HtmlElement =
    div(cls := "rounded-xl border border-slate-200 bg-white shadow-sm", mods)

  def sectionTitle(text: String): HtmlElement =
    h2(cls := "text-base font-semibold text-slate-800", text)

  def field(labelText: String, control: HtmlElement): HtmlElement =
    label(cls := "block", span(cls := labelCls, labelText), control)

  // ---- Form controls -----------------------------------------------------------------------------

  def textInput(state: Var[String]): HtmlElement =
    textInput(state, "", "text")

  def textInput(state: Var[String], placeholderText: String): HtmlElement =
    textInput(state, placeholderText, "text")

  def textInput(state: Var[String], placeholderText: String, inputType: String): HtmlElement =
    input(
      typ := inputType,
      cls := inputCls,
      placeholder := placeholderText,
      controlled(value <-- state.signal, onInput.mapToValue --> state),
    )

  /** Variant of [[textInput]] that accepts a `Signal` for reading and an `Observer` for writing. */
  def textInput(valueSignal: Signal[String], writer: Observer[String], placeholderText: String, inputType: String): HtmlElement =
    input(
      typ := inputType,
      cls := inputCls,
      placeholder := placeholderText,
      controlled(value <-- valueSignal, onInput.mapToValue --> writer),
    )

  def textInput(valueSignal: Signal[String], writer: Observer[String], placeholderText: String): HtmlElement =
    textInput(valueSignal, writer, placeholderText, "text")

  def selectInput(state: Var[String], opts: Signal[List[(String, String)]]): HtmlElement =
    select(
      cls := inputCls,
      controlled(value <-- state.signal, onChange.mapToValue --> state),
      children <-- opts.map(_.map { case (optValue, optLabel) => option(value := optValue, optLabel) }),
    )

  /** Variant of [[selectInput]] that accepts a `Signal` for reading and an `Observer` for writing. */
  def selectInput(valueSignal: Signal[String], writer: Observer[String], opts: Signal[List[(String, String)]]): HtmlElement =
    select(
      cls := inputCls,
      controlled(value <-- valueSignal, onChange.mapToValue --> writer),
      children <-- opts.map(_.map { case (optValue, optLabel) => option(value := optValue, optLabel) }),
    )

  def staticSelect(state: Var[String], opts: List[(String, String)]): HtmlElement =
    selectInput(state, Signal.fromValue(opts))

  /** Variant of [[staticSelect]] that accepts a `Signal` for reading and an `Observer` for writing. */
  def staticSelect(valueSignal: Signal[String], writer: Observer[String], opts: List[(String, String)]): HtmlElement =
    selectInput(valueSignal, writer, Signal.fromValue(opts))

  // ---- Validation --------------------------------------------------------------------------------

  /** Folds a sequence of per-field error signals into a single `Signal[List[String]]`. */
  def formErrorsSignal(checks: Signal[Option[String]]*): Signal[List[String]] =
    checks.foldLeft(Signal.fromValue(List.empty[String]): Signal[List[String]]) { (acc, s) =>
      acc.combineWith(s).map { case (errs, opt) => errs ++ opt.toList }
    }

  // ---- Feedback ----------------------------------------------------------------------------------

  def badge(text: String, colorClasses: String): HtmlElement =
    span(cls := s"inline-flex items-center rounded-full border px-2 py-0.5 text-xs font-medium $colorClasses", text)

  def statusBadge(status: String): HtmlElement =
    val color = status match
      case "completed" => "bg-emerald-50 text-emerald-700 border-emerald-200"
      case "in_progress" => "bg-sky-50 text-sky-700 border-sky-200"
      case "delivered" => "bg-violet-50 text-violet-700 border-violet-200"
      case "suspended" | "paused" => "bg-amber-50 text-amber-700 border-amber-200"
      case "rejected" | "cancelled" => "bg-rose-50 text-rose-700 border-rose-200"
      case _ => "bg-slate-50 text-slate-600 border-slate-200"
    badge(status.replace('_', ' ').nn, color)

  def spinner: HtmlElement =
    div(cls := "flex items-center gap-2 p-6 text-sm text-slate-400", "Caricamento…")

  def emptyState(text: String): HtmlElement =
    div(cls := "rounded-lg border border-dashed border-slate-200 p-6 text-center text-sm text-slate-400", text)

  def errorBanner(err: ApiError): HtmlElement =
    div(
      cls := "rounded-md border border-rose-200 bg-rose-50 p-3 text-sm text-rose-700",
      div(cls := "font-medium", err.message),
      if err.details.nonEmpty then ul(cls := "mt-1 list-disc pl-5 text-xs", err.details.map(d => li(d)))
      else emptyNode,
    )

  def showError(target: Var[Option[ApiError]], context: String)(err: ApiError): Unit =
    ErrorCenter.reportTo(target, context)(err)

  /** Renders a possibly-loading, possibly-failed remote resource. */
  def renderResult[A](signal: Signal[Option[ApiClient.Result[A]]])(render: A => HtmlElement): HtmlElement =
    div(
      child <-- signal.map {
        case None => spinner
        case Some(Left(err)) => errorBanner(err)
        case Some(Right(data)) => render(data)
      },
    )

  // ---- Data loading ------------------------------------------------------------------------------

  /**
   * Turns a `tick` signal + a loading effect into a resource signal that reloads on every tick.
   *
   * Reloads happen **in place**: the previously loaded value stays visible while the new request is in flight (no spinner/empty flash), and
   * consecutive identical results don't re-render (`distinct`). This keeps refreshes — including the automatic ones — visually smooth.
   */
  def loadable[A](tick: Signal[Any])(load: () => Future[ApiClient.Result[A]]): Signal[Option[ApiClient.Result[A]]] =
    tick
      .flatMapSwitch(_ =>
        EventStream.fromFuture(
          load().map {
            case Left(err) =>
              ErrorCenter.report("Caricamento dati", err)
              Left(err)
            case result => result
          },
        ),
      )
      .toWeakSignal
      .distinct

  // ---- Modal -------------------------------------------------------------------------------------

  def modal(isOpen: Var[Boolean], titleText: String)(content: HtmlElement): HtmlElement =
    modal(isOpen, Signal.fromValue(titleText))(content)

  def modal(isOpen: Var[Boolean], titleText: Signal[String])(content: HtmlElement): HtmlElement =
    div(
      cls := "fixed inset-0 z-40 items-start justify-center overflow-y-auto bg-slate-900/40 p-4",
      cls <-- isOpen.signal.map(open => if open then "flex" else "hidden"),
      div(
        cls := "mt-8 w-full max-w-2xl",
        card(
          div(
            cls := "flex items-center justify-between border-b border-slate-100 px-4 py-3",
            h2(cls := "text-sm font-semibold text-slate-800", child.text <-- titleText),
            button(cls := "text-slate-400 hover:text-slate-700", "✕", onClick --> (_ => isOpen.set(false))),
          ),
          div(cls := "p-4", content),
        ),
      ),
    )
end Components
