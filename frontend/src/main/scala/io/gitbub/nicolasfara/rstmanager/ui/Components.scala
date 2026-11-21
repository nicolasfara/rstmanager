package io.gitbub.nicolasfara.rstmanager.ui

import scala.annotation.targetName
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.ApiError
import io.gitbub.nicolasfara.rstmanager.auth.{ AuthService, Role }
import org.scalajs.dom

/** Minimal, reusable Tailwind-styled building blocks shared by all pages. */
object Components:

  // ---- Class tokens ------------------------------------------------------------------------------

  val btnPrimary =
    "inline-flex items-center gap-1.5 rounded-md bg-slate-900 px-3 py-1.5 text-sm font-medium text-white hover:bg-slate-700 disabled:opacity-50"
  val btnGhost =
    "inline-flex items-center gap-1.5 rounded-md border border-slate-300 px-3 py-1.5 text-sm font-medium text-slate-700 hover:bg-slate-100"
  val btnSmall = "inline-flex items-center rounded-md border border-slate-300 px-2 py-1 text-xs font-medium text-slate-600 hover:bg-slate-100"
  val btnDanger = "inline-flex items-center rounded-md border border-rose-300 px-2 py-1 text-xs font-medium text-rose-700 hover:bg-rose-50"
  val btnDangerSolid =
    "inline-flex items-center justify-center gap-1.5 rounded-md bg-rose-600 px-3 py-1.5 text-sm font-medium text-white hover:bg-rose-500 disabled:opacity-50"
  private val inputCls =
    "w-full rounded-md border border-slate-300 px-2.5 py-1.5 text-sm text-slate-800 focus:border-slate-500 focus:outline-none focus:ring-1 focus:ring-slate-500"
  private val labelCls = "mb-1 block text-xs font-medium text-slate-500"

  /** Small uppercase heading of a form block (e.g. "Lavorazioni", "Materiali"). */
  val subheadingCls = "text-xs font-semibold uppercase tracking-wide text-slate-500"

  /** Extra classes of a control whose value is invalid; `!` so they win over the neutral border/ring of the input. */
  val inputInvalidCls = "border-rose-400! focus:border-rose-500! focus:ring-rose-500!"

  /** A text-only button (secondary inline actions such as "↺ Usa proposta" or "← Torna agli ordini"). */
  val btnLink =
    "inline-flex items-center gap-1 rounded-sm text-sm font-medium text-slate-600 underline-offset-2 hover:text-slate-900 hover:underline"

  // ---- Layout primitives -------------------------------------------------------------------------

  def card(mods: Modifier[HtmlElement]*): HtmlElement =
    div(cls := "rounded-xl border border-slate-200 bg-white shadow-sm", mods)

  /** Renders `el` only while the logged-in user satisfies `required`. UI gating is cosmetic: the server enforces authorization. */
  def roleGated(required: Role)(el: => HtmlElement): Modifier[HtmlElement] =
    child.maybe <-- AuthService.hasRoleSignal(required).map(allowed => Option.when(allowed)(el))

  /** Applies the multi-column grid classes only when the role-gated side panel (usually the edit form) is visible. */
  def roleGatedGridCols(required: Role, cols: String): Modifier[HtmlElement] =
    cls <-- AuthService.hasRoleSignal(required).map(allowed => if allowed then cols else "")

  def sectionTitle(text: String): HtmlElement =
    h2(cls := "text-base font-semibold text-slate-800", text)

  def field(labelText: String, control: HtmlElement): HtmlElement =
    label(cls := "block", span(cls := labelCls, labelText), control)

  /**
   * Marks the form card that "Modifica" fills in place. On mobile the form is stacked above a long list, so `reveal()` scrolls it into view when its
   * top is off-screen; `scroll-mt-20` keeps it clear of the sticky header.
   */
  final class FormAnchor:
    private val element = Var(Option.empty[dom.Element])

    val bind: Modifier[HtmlElement] = List(
      cls := "scroll-mt-20",
      onMountUnmountCallback(ctx => element.set(Some(ctx.thisNode.ref)), _ => element.set(None)),
    )

    def reveal(): Unit = element.now().foreach { el =>
      val top = el.getBoundingClientRect().top
      if top < 0 || top > dom.window.innerHeight then el.scrollIntoView(true)
    }
  end FormAnchor

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

  /** Variant of [[textInput]] rendered as a multi-line textarea. */
  def textAreaInput(valueSignal: Signal[String], writer: Observer[String], placeholderText: String): HtmlElement =
    textArea(
      cls := inputCls,
      placeholder := placeholderText,
      rows := 3,
      controlled(value <-- valueSignal, onInput.mapToValue --> writer),
    )

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

  /** Case-insensitive substring match of the typed text on an option label (blank text matches everything). */
  private val labelMatch: (String, String) => Boolean = (typed, labelText) =>
    val needle = typed.trim.nn.toLowerCase.nn
    needle.isEmpty || labelText.toLowerCase.nn.contains(needle)

  /**
   * Searchable single-select ("combobox"): a text input that filters `opts` in real time (case-insensitive substring match on the label); clicking a
   * result selects it. While not editing, the input shows the label of the selected value; blurring without picking anything discards the search text
   * and reverts to that label. At most `maxResults` matches are listed; beyond that, a footer invites the user to refine the search, so a large
   * option list never floods the dropdown.
   */
  def searchableSelect(
      valueSignal: Signal[String],
      writer: Observer[String],
      opts: Signal[List[(String, String)]],
      placeholderText: String,
      maxResults: Int,
  ): HtmlElement =
    combobox(valueSignal, writer, opts.map(_.map((id, labelText) => (id, labelText, labelText))), labelMatch, placeholderText, maxResults, emptyMod)

  /**
   * Variant of [[searchableSelect]] whose `inputMods` are applied to the inner text `<input>` (the root is a wrapper `div`), so a [[FieldSpec]]
   * `bind` (id, ARIA, invalid style) reaches the focusable control.
   */
  def searchableSelect(
      valueSignal: Signal[String],
      writer: Observer[String],
      opts: Signal[List[(String, String)]],
      placeholderText: String,
      maxResults: Int,
      inputMods: Modifier[HtmlElement],
  ): HtmlElement =
    combobox(valueSignal, writer, opts.map(_.map((id, labelText) => (id, labelText, labelText))), labelMatch, placeholderText, maxResults, inputMods)

  /**
   * Variant of [[searchableSelect]] whose options are `(value, label, searchText)`: the label is what is shown, the search text what the typed query
   * is matched against, term by term — every blank-separated term must appear in it, case-insensitively (see [[SearchTerms]]). So one option can be
   * found by any of the words or codes its search text gathers (e.g. a material by internal code, description, or any of its MPNs).
   */
  @targetName("searchableSelectWithSearchText")
  def searchableSelect(
      valueSignal: Signal[String],
      writer: Observer[String],
      opts: Signal[List[(String, String, String)]],
      placeholderText: String,
      maxResults: Int,
  ): HtmlElement =
    combobox(valueSignal, writer, opts, SearchTerms.matches, placeholderText, maxResults, emptyMod)

  /** Variant of the search-text [[searchableSelect]] whose `inputMods` are applied to the inner text `<input>`. */
  @targetName("searchableSelectWithSearchTextAndInputMods")
  def searchableSelect(
      valueSignal: Signal[String],
      writer: Observer[String],
      opts: Signal[List[(String, String, String)]],
      placeholderText: String,
      maxResults: Int,
      inputMods: Modifier[HtmlElement],
  ): HtmlElement =
    combobox(valueSignal, writer, opts, SearchTerms.matches, placeholderText, maxResults, inputMods)

  /** The combobox behind both [[searchableSelect]] variants: `matches(typed, searchText)` decides which options are listed while typing. */
  private def combobox(
      valueSignal: Signal[String],
      writer: Observer[String],
      opts: Signal[List[(String, String, String)]],
      matches: (String, String) => Boolean,
      placeholderText: String,
      maxResults: Int,
      inputMods: Modifier[HtmlElement],
  ): HtmlElement =
    // `None` = not editing (show the selected label); `Some(text)` = the search text being typed.
    val query = Var(Option.empty[String])
    val open = Var(false)
    val displayValue: Signal[String] =
      query.signal.combineWith(valueSignal, opts).map { case (typed, selected, options) =>
        typed.getOrElse(options.collectFirst { case (id, labelText, _) if id == selected => labelText }.getOrElse(""))
      }
    // Visible matches plus how many further matches were cut off by `maxResults`.
    val filtered: Signal[(List[(String, String, String)], Int)] =
      query.signal.combineWith(opts).map { case (typed, options) =>
        val matching = typed.fold(options)(text => options.filter((_, _, searchText) => matches(text, searchText)))
        (matching.take(maxResults), (matching.size - maxResults).max(0))
      }
    def choose(id: String): Unit =
      writer.onNext(id)
      query.set(None)
      open.set(false)
    div(
      cls := "relative",
      input(
        typ := "text",
        cls := inputCls,
        placeholder := placeholderText,
        controlled(value <-- displayValue, onInput.mapToValue --> { text => query.set(Some(text)); open.set(true) }),
        onFocus --> { _ => open.set(true) },
        // Selection happens on the options' mousedown, which fires before this blur.
        onBlur --> { _ => open.set(false); query.set(None) },
        onKeyDown.filter(_.key == "Escape") --> { _ => open.set(false); query.set(None) },
        inputMods,
      ),
      child <-- open.signal.combineWith(filtered).map { case (isOpen, options, hiddenCount) =>
        if !isOpen then emptyNode
        else
          div(
            cls := "absolute z-50 mt-1 max-h-56 w-full overflow-y-auto rounded-md border border-slate-200 bg-white shadow-lg",
            if options.isEmpty then div(cls := "px-2.5 py-2 text-sm text-slate-400", "Nessun risultato")
            else
              options.map { case (id, labelText, _) =>
                div(
                  cls := "cursor-pointer px-2.5 py-1.5 text-sm text-slate-700 hover:bg-slate-100",
                  cls <-- valueSignal.map(selected => if selected == id then "bg-slate-50 font-medium" else ""),
                  labelText,
                  // `mousedown` (not `click`) so it wins over the input's blur; preventDefault keeps the input focused.
                  onMouseDown.preventDefault --> { _ => choose(id) },
                )
              }
            ,
            if hiddenCount > 0 then
              div(
                cls := "border-t border-slate-100 px-2.5 py-1.5 text-xs text-slate-400",
                s"… e altri $hiddenCount risultati — continua a digitare per affinare la ricerca",
              )
            else emptyNode,
          )
      },
    )
  end combobox

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

  // ---- Form building blocks ----------------------------------------------------------------------

  /** Small uppercase heading of a block inside a form (see [[subheadingCls]]). */
  def subheading(text: String): HtmlElement = div(cls := subheadingCls, text)

  /** Muted helper text shown under a control. */
  def hint(text: String): HtmlElement = div(cls := "mt-1 text-xs text-slate-400", text)

  private def errorListElement(errors: List[String]): HtmlElement =
    ul(cls := "mt-3 list-disc pl-5 text-xs text-rose-600", errors.map(error => li(error)))

  /** The validation messages as a red bullet list; nothing while there are none. */
  def errorList(errors: Signal[List[String]]): Modifier[HtmlElement] =
    child.maybe <-- errors.map(list => Option.when(list.nonEmpty)(errorListElement(list)))

  /** Variant of [[errorList]] for messages already resolved (e.g. inside a `child <--` mapping). */
  def errorList(errors: List[String]): Modifier[HtmlElement] =
    if errors.isEmpty then emptyNode else errorListElement(errors)

  /**
   * What [[field]] needs to render an accessible, validated form field: the visible label, the DOM `id` of the control (also the anchor that
   * [[FormFocus.focus]] targets), the required marker, an optional hint, the inline error to show (usually from [[FormTouch.visible]]), and extra
   * modifiers for the wrapper (e.g. the `focusout` touch tracking added by [[FormTouch.spec]], or a dynamic warning under the control).
   */
  final case class FieldSpec(
      labelText: String,
      id: String,
      required: Boolean = false,
      hintText: Option[String] = None,
      error: Signal[Option[String]] = Signal.fromValue(None),
      wrapperMods: Modifier[HtmlElement] = emptyMod,
  ):
    def hintId: String = s"$id-hint"
    def errorId: String = s"$id-error"

  private def requiredMarker(required: Boolean): Modifier[HtmlElement] =
    if required then List(span(aria.hidden := true, cls := "ml-0.5 text-rose-600", "*"), span(cls := "sr-only", " (obbligatorio)"))
    else emptyMod

  /**
   * A labelled field driven by `spec`: `<label for=id>` with the required `*` (hidden from screen readers, which read "(obbligatorio)" instead), the
   * hint and the inline error (a polite live region). `control` receives the `bind` modifier to apply to the actual focusable control: it sets `id`,
   * `aria-required`, `aria-invalid`, `aria-describedby` (hint and error) and the invalid style. For a [[searchableSelect]] pass `bind` as its
   * `inputMods`.
   */
  def field(spec: FieldSpec)(control: Modifier[HtmlElement] => HtmlElement): HtmlElement =
    val invalid = spec.error.map(_.isDefined)
    val bind: Modifier[HtmlElement] = List(
      idAttr := spec.id,
      aria.required := spec.required,
      aria.invalid <-- invalid.map(isInvalid => if isInvalid then "true" else "false"),
      aria.describedBy <-- invalid.map(isInvalid =>
        (spec.hintText.map(_ => spec.hintId).toList ++ Option.when(isInvalid)(spec.errorId)).mkString(" "),
      ),
      cls <-- invalid.map(isInvalid => if isInvalid then inputInvalidCls else ""),
    )
    div(
      cls := "block",
      spec.wrapperMods,
      label(forId := spec.id, cls := labelCls, spec.labelText, requiredMarker(spec.required)),
      control(bind),
      spec.hintText.fold[Modifier[HtmlElement]](emptyNode)(text => div(idAttr := spec.hintId, cls := "mt-1 text-xs text-slate-400", text)),
      div(
        idAttr := spec.errorId,
        aria.live := "polite",
        cls <-- invalid.map(isInvalid => if isInvalid then "mt-1 text-xs font-medium text-rose-600" else ""),
        child.text <-- spec.error.map(_.getOrElse("")),
      ),
    )
  end field

  /** Segmented single choice (a few mutually exclusive options, e.g. priority): `opts` are `(value, label)` pairs. */
  def segmented(valueSignal: Signal[String], writer: Observer[String], opts: List[(String, String)]): HtmlElement =
    segmented(valueSignal, writer, opts, emptyMod)

  /**
   * Variant of [[segmented]] with `groupMods` on the `radiogroup` element (e.g. a [[FieldSpec]] `bind`, or `aria.label`). Each option is a plain
   * `button` (reachable with Tab, activated with Enter/Space) with `role=radio` and `aria-checked`.
   */
  def segmented(valueSignal: Signal[String], writer: Observer[String], opts: List[(String, String)], groupMods: Modifier[HtmlElement]): HtmlElement =
    div(
      role := "radiogroup",
      cls := "inline-flex flex-wrap gap-1 rounded-md border border-slate-300 bg-slate-50 p-0.5",
      groupMods,
      opts.map { (optValue, optLabel) =>
        val active = valueSignal.map(_ == optValue)
        button(
          tpe := "button",
          role := "radio",
          aria.checked <-- active.map(isActive => if isActive then "true" else "false"),
          cls := "rounded px-3 py-1 text-sm font-medium transition-colors focus:outline-none focus-visible:ring-2 focus-visible:ring-slate-500",
          cls <-- active.map(isActive => if isActive then "bg-slate-900 text-white shadow-sm" else "text-slate-600 hover:bg-slate-200"),
          optLabel,
          onClick --> (_ => writer.onNext(optValue)),
        )
      },
    )

  /** Completion state of a [[formSection]]: no indicator, all good, or `count` things still to fix. */
  enum SectionStatus derives CanEqual:
    case Neutral
    case Complete
    case Pending(count: Int)

  /** "✓" when `errorCount` is 0, else "⚠ n" (amber), with screen-reader text. */
  def statusIcon(errorCount: Int): HtmlElement =
    if errorCount <= 0 then
      span(
        cls := "inline-flex items-center gap-1 text-xs font-medium text-emerald-700",
        span(aria.hidden := true, "✓"),
        span(cls := "sr-only", "Completo"),
      )
    else
      span(
        cls := "inline-flex items-center gap-1 rounded-full bg-amber-50 px-2 py-0.5 text-xs font-medium text-amber-700",
        span(aria.hidden := true, s"⚠ $errorCount"),
        span(cls := "sr-only", if errorCount == 1 then "1 elemento da completare" else s"$errorCount elementi da completare"),
      )

  private def sectionHeading(title: String, description: String): HtmlElement =
    div(
      cls := "min-w-0 flex-1",
      h2(cls := "text-base font-semibold text-slate-800", title),
      if description.isEmpty then emptyNode else p(cls := "mt-0.5 text-xs text-slate-500", description),
    )

  /**
   * A numbered form section card: number badge, title, description (empty = none) and the `status` indicator; `content` goes in the body (so an
   * `idAttr` among it anchors the body).
   */
  def formSection(number: Int, title: String, description: String, status: Signal[SectionStatus])(content: Modifier[HtmlElement]*): HtmlElement =
    sectionTag(
      cls := "rounded-xl border border-slate-200 bg-white shadow-sm",
      div(
        cls := "flex items-start gap-3 border-b border-slate-100 px-4 py-3",
        span(
          aria.hidden := true,
          cls := "flex h-7 w-7 shrink-0 items-center justify-center rounded-full text-sm font-semibold",
          cls <-- status.map {
            case SectionStatus.Complete => "bg-emerald-100 text-emerald-700"
            case _ => "bg-slate-100 text-slate-700"
          },
          number.toString,
        ),
        sectionHeading(title, description),
        child <-- status.map {
          case SectionStatus.Neutral => emptyNode
          case SectionStatus.Complete => statusIcon(0)
          case SectionStatus.Pending(count) => statusIcon(count)
        },
      ),
      div(cls := "scroll-mt-20 space-y-4 p-4", content),
    )

  /** Variant of [[formSection]] without number and status. */
  def formSection(title: String, description: String)(content: Modifier[HtmlElement]*): HtmlElement =
    sectionTag(
      cls := "rounded-xl border border-slate-200 bg-white shadow-sm",
      div(cls := "flex items-start gap-3 border-b border-slate-100 px-4 py-3", sectionHeading(title, description)),
      div(cls := "scroll-mt-20 space-y-4 p-4", content),
    )

  /** A group of related fields inside a form, as a `<fieldset>` with its `<legend>` styled as a [[subheading]]. */
  def fieldGroup(legendText: String)(content: Modifier[HtmlElement]*): HtmlElement =
    fieldSet(
      cls := "min-w-0 space-y-3",
      legend(cls := s"$subheadingCls mb-2", legendText),
      content,
    )

  /**
   * Checkbox chips for a multiple choice (e.g. "Dipende da"): `choices` are `(id, label)` pairs, `selected` drives the checked state and
   * `toggle(id, checked)` runs on every change. Hidden while `choices` is empty.
   */
  def checkboxChips(
      labelText: String,
      choices: Signal[List[(String, String)]],
      selected: Signal[Set[String]],
      toggle: (String, Boolean) => Unit,
  ): HtmlElement =
    div(
      child <-- choices.map { list =>
        if list.isEmpty then emptyNode
        else
          div(
            role := "group",
            aria.label := labelText,
            cls := "mt-2 flex flex-wrap items-center gap-2 text-xs text-slate-600",
            span(aria.hidden := true, cls := "font-medium text-slate-500", labelText),
            list.map { case (id, choiceLabel) =>
              label(
                cls := "inline-flex items-center gap-1 rounded-md border border-slate-200 px-2 py-1",
                input(
                  typ := "checkbox",
                  checked <-- selected.map(_.contains(id)),
                  onChange.mapToChecked --> { isChecked => toggle(id, isChecked) },
                ),
                choiceLabel,
              )
            },
          )
      },
    )

  /**
   * A collapsible card. The header is a toggle `button` (`aria-expanded`, `aria-controls=panelId`) showing `summary`; `actions` sit next to it,
   * outside the button (no nested buttons). The `body` is always mounted and only hidden while collapsed, so its state (comboboxes, loaded materials)
   * survives collapsing. `hasIssues` gives the card an amber border.
   */
  def accordionCard(
      panelId: String,
      expanded: Signal[Boolean],
      onToggle: () => Unit,
      summary: Modifier[HtmlElement],
      actions: Modifier[HtmlElement],
      hasIssues: Signal[Boolean],
  )(body: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "rounded-lg border bg-white",
      cls <-- hasIssues.map(issues => if issues then "border-amber-300" else "border-slate-200"),
      div(
        cls := "flex items-center gap-2 px-3 py-2",
        button(
          tpe := "button",
          cls := "flex min-w-0 flex-1 items-center gap-2 rounded-md py-1 text-left focus:outline-none focus-visible:ring-2 focus-visible:ring-slate-500",
          aria.expanded <-- expanded,
          aria.controls := panelId,
          span(
            aria.hidden := true,
            cls := "shrink-0 text-xs text-slate-400 transition-transform",
            cls <-- expanded.map(open => if open then "rotate-90" else ""),
            "▶",
          ),
          div(cls := "min-w-0 flex-1", summary),
          onClick --> (_ => onToggle()),
        ),
        div(cls := "flex shrink-0 items-center gap-2", actions),
      ),
      div(
        idAttr := panelId,
        cls := "border-t border-slate-100 p-3",
        cls <-- expanded.map(open => if open then "" else "hidden"),
        body,
      ),
    )

  /**
   * A form page with a sticky summary: `main` on the left, `sidebar` sticky on the right from `lg` up (below the sticky header), and on smaller
   * screens `mobileBar` fixed at the bottom instead (the main column gets bottom padding so the bar never covers its end).
   */
  def summaryLayout(main: HtmlElement, sidebar: HtmlElement, mobileBar: HtmlElement): HtmlElement =
    div(
      div(
        cls := "grid gap-6 lg:grid-cols-[minmax(0,1fr)_20rem]",
        div(cls := "min-w-0 pb-28 lg:pb-0", main),
        asideTag(cls := "hidden self-start lg:sticky lg:top-20 lg:block", sidebar),
      ),
      div(
        cls := "fixed inset-x-0 bottom-0 z-30 border-t border-slate-200 bg-white/95 px-4 py-3 shadow-lg backdrop-blur lg:hidden",
        mobileBar,
      ),
    )

  /** One `label · value` line of a summary. */
  def summaryRow(labelText: String, value: Signal[String]): HtmlElement =
    div(
      cls := "flex items-baseline justify-between gap-3 text-sm",
      span(cls := "shrink-0 text-slate-500", labelText),
      span(cls := "min-w-0 truncate text-right font-medium text-slate-800", child.text <-- value),
    )

  /**
   * The checklist of what is still missing: each issue is a button that calls `onSelect` (e.g. reveal and focus the field). Neutral (slate/amber)
   * until `emphasize` (typically [[FormTouch.submittedSignal]]), then red; "✓ Pronto" when empty. A polite live region. Re-renders on every emission
   * of `issues`, so pass a `distinct` signal.
   */
  def issueList[A](issues: Signal[List[A]], text: A => String, emphasize: Signal[Boolean], onSelect: A => Unit): HtmlElement =
    div(
      aria.live := "polite",
      child <-- issues.combineWith(emphasize).map { (list, strong) =>
        if list.isEmpty then div(cls := "text-sm font-medium text-emerald-700", "✓ Pronto")
        else
          ul(
            cls := "space-y-1",
            list.map { issue =>
              li(
                button(
                  tpe := "button",
                  cls := "flex w-full items-start gap-1.5 rounded-sm text-left text-xs hover:underline focus:outline-none focus-visible:ring-2 focus-visible:ring-slate-400",
                  cls := (if strong then "text-rose-700" else "text-slate-600"),
                  span(aria.hidden := true, cls := (if strong then "text-rose-500" else "text-amber-500"), "•"),
                  span(text(issue)),
                  onClick --> (_ => onSelect(issue)),
                ),
              )
            },
          )
      },
    )

  /**
   * The cancel / submit pair at the end of a form. The submit button stays enabled (an invalid submit shows the errors) except while `submitting`,
   * when it shows `busyLabel`; `onSubmit` should still ignore calls while a request is in flight. No cancel button when `onCancel` is `None`.
   */
  def formActions(
      submitLabel: String,
      busyLabel: String,
      submitting: Signal[Boolean],
      onCancel: Option[() => Unit],
      onSubmit: () => Unit,
  ): HtmlElement =
    div(
      cls := "flex flex-col-reverse gap-2 border-t border-slate-100 pt-3 sm:flex-row sm:justify-end",
      onCancel.fold[Modifier[HtmlElement]](emptyNode)(cancel =>
        button(tpe := "button", cls := s"$btnGhost justify-center", "Annulla", onClick --> (_ => cancel())),
      ),
      button(
        tpe := "button",
        cls := s"$btnPrimary justify-center",
        disabled <-- submitting,
        aria.busy <-- submitting,
        child.text <-- submitting.map(busy => if busy then busyLabel else submitLabel),
        onClick --> (_ => onSubmit()),
      ),
    )

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

  // ---- Lists -------------------------------------------------------------------------------------

  /** The search field of a client-side filtered list: typing updates `query` and brings `pager` back to the first page. */
  def searchBox(query: Var[String], placeholderText: String, pager: Pagination): HtmlElement =
    textInput(query.signal, Observer[String] { text => query.set(text); pager.reset() }, placeholderText, "search")

  /**
   * A loaded list searched and paginated on screen (`all` is a signal so that a [[TableSort]] can reorder it): keeps the rows of `all` whose `fields`
   * match `query` term by term (see [[SearchTerms]]), renders the current page with `render` and the pager below it; `noMatch` is shown when the
   * query matches nothing.
   */
  def searchedPages[A](all: Signal[List[A]], query: Signal[String], fields: A => List[String], pager: Pagination, noMatch: String)(
      render: List[A] => HtmlElement,
  ): HtmlElement =
    searchedPages(all, query, Signal.fromValue(fields), pager, noMatch)(render)

  /** Variant of [[searchedPages]] whose search fields depend on other loaded data (e.g. the name of a referenced customer). */
  @targetName("searchedPagesWithFieldsSignal")
  def searchedPages[A](all: Signal[List[A]], query: Signal[String], fields: Signal[A => List[String]], pager: Pagination, noMatch: String)(
      render: List[A] => HtmlElement,
  ): HtmlElement =
    pager.view(all.combineWith(query, fields).map((rows, typed, fieldsOf) => rows.filter(row => SearchTerms.matchesAny(typed, fieldsOf(row))))) {
      shown =>
        if shown.isEmpty then div(cls := "p-4", emptyState(noMatch)) else render(shown)
    }

  // ---- Modal -------------------------------------------------------------------------------------

  /** Visual tone of a [[confirmDialog]]: destructive actions get a warning look, everything else a neutral one. */
  enum DialogTone derives CanEqual:
    case Danger, Neutral

  /**
   * A compact, centered confirmation dialog — the house style for "are you sure?" prompts across the app. Shows a tonal icon, a title and message,
   * and a Cancel/Confirm pair (stacked on mobile, inline on desktop). Clicking the backdrop cancels. Drive `isOpen`/`titleText`/`message` from the
   * caller's state (typically a `Var[Option[T]]`); `onCancel`/`onConfirm` should also flip that state closed.
   */
  def confirmDialog(
      isOpen: Signal[Boolean],
      titleText: Signal[String],
      message: Signal[String],
      confirmLabel: String,
      tone: DialogTone,
      onCancel: () => Unit,
      onConfirm: () => Unit,
  ): HtmlElement =
    confirmDialog(isOpen, titleText, message, confirmLabel, tone, emptyNode, Signal.fromValue(true), onCancel, onConfirm)

  /**
   * Variant of [[confirmDialog]] with extra `body` content under the message (e.g. a required "reason" input, carrying its own top margin); the
   * confirm button stays disabled while `confirmEnabled` is false.
   */
  def confirmDialog(
      isOpen: Signal[Boolean],
      titleText: Signal[String],
      message: Signal[String],
      confirmLabel: String,
      tone: DialogTone,
      body: Modifier[HtmlElement],
      confirmEnabled: Signal[Boolean],
      onCancel: () => Unit,
      onConfirm: () => Unit,
  ): HtmlElement =
    val (iconWrapCls, iconChar, confirmCls) = tone match
      case DialogTone.Danger => ("bg-rose-100 text-rose-600", "⚠", btnDangerSolid)
      case DialogTone.Neutral => ("bg-sky-100 text-sky-600", "?", s"$btnPrimary justify-center")
    div(
      cls := "fixed inset-0 z-50 items-center justify-center overflow-y-auto bg-slate-900/50 p-4",
      cls <-- isOpen.map(open => if open then "flex" else "hidden"),
      // Clicking the backdrop (but not the card) cancels.
      onClick --> (_ => onCancel()),
      div(
        cls := "w-full max-w-sm",
        card(
          cls := "p-6",
          onClick.stopPropagation --> (_ => ()),
          div(
            cls := "flex flex-col items-center gap-4 text-center",
            div(
              cls := s"flex h-12 w-12 items-center justify-center rounded-full $iconWrapCls",
              span(cls := "text-2xl leading-none", iconChar),
            ),
            div(
              cls := "space-y-1",
              h2(cls := "text-base font-semibold text-slate-900", child.text <-- titleText),
              p(cls := "text-sm text-slate-500", child.text <-- message),
            ),
          ),
          body,
          div(
            cls := "mt-6 flex flex-col-reverse gap-2 sm:flex-row",
            button(tpe := "button", cls := s"$btnGhost flex-1 justify-center", "Annulla", onClick --> (_ => onCancel())),
            button(
              tpe := "button",
              cls := s"$confirmCls flex-1",
              confirmLabel,
              disabled <-- confirmEnabled.map(!_),
              onClick --> (_ => onConfirm()),
            ),
          ),
        ),
      ),
    )
  end confirmDialog

  def modal(isOpen: Var[Boolean], titleText: String)(content: HtmlElement): HtmlElement =
    modal(isOpen, Signal.fromValue(titleText))(content)

  def modal(isOpen: Var[Boolean], titleText: Signal[String])(content: HtmlElement): HtmlElement =
    modal(isOpen.signal, titleText, "max-w-2xl", () => isOpen.set(false))(content)

  /**
   * Variant of [[modal]] driven by a `Signal` (typically derived from a `Var[Option[T]]` holding what the modal is about), with the card width as a
   * Tailwind `max-w-*` class; `onClose` runs when the ✕ is clicked and should flip the caller's state closed.
   */
  def modal(isOpen: Signal[Boolean], titleText: Signal[String], maxWidthCls: String, onClose: () => Unit)(content: HtmlElement): HtmlElement =
    div(
      cls := "fixed inset-0 z-40 items-start justify-center overflow-y-auto bg-slate-900/40 p-4",
      cls <-- isOpen.map(open => if open then "flex" else "hidden"),
      div(
        cls := s"mt-8 w-full $maxWidthCls",
        card(
          div(
            cls := "flex items-center justify-between border-b border-slate-100 px-4 py-3",
            h2(cls := "text-sm font-semibold text-slate-800", child.text <-- titleText),
            button(tpe := "button", cls := "text-slate-400 hover:text-slate-700", "✕", onClick --> (_ => onClose())),
          ),
          div(cls := "p-4", content),
        ),
      ),
    )
end Components
