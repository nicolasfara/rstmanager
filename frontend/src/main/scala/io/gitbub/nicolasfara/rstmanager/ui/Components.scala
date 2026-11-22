package io.gitbub.nicolasfara.rstmanager.ui

import scala.annotation.targetName
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.StringAsIsCodec
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

  /**
   * Action bar at the bottom of a mobile list card: its buttons (direct children, [[roleGated]] ones included) get a 40px touch target. The cards
   * only show below the breakpoint where their list switches to a table, so the desktop buttons keep their compact size.
   */
  val cardActionsCls = "flex flex-wrap gap-2 border-t border-slate-100 pt-3 *:min-h-10"

  /** Grows the buttons directly inside a group always on screen (e.g. a card row's actions) to a 40px touch target below `md`. */
  val touchChildrenCls = "*:min-h-10 md:*:min-h-0"

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

  /** A read-only labelled value (the fields of a mobile list card, of a details panel): small grey label over the value. */
  def readField(labelText: String, valueNode: Modifier[HtmlElement]): HtmlElement =
    div(
      cls := "min-w-0 space-y-0.5",
      div(cls := "text-xs font-medium text-slate-500", labelText),
      div(cls := "break-words text-sm text-slate-800", valueNode),
    )

  /**
   * Marks the form card that "Modifica" fills in place. On mobile the form is stacked after a long list, so `revealAndFocus()` scrolls it into view
   * when its top is off-screen or in the lower half of the viewport (barely visible near the bottom), then focuses its first field; `scroll-mt-20`
   * keeps it clear of the sticky header.
   */
  final class FormAnchor:
    private val element = Var(Option.empty[dom.Element])

    val bind: Modifier[HtmlElement] = List(
      cls := "scroll-mt-20",
      onMountUnmountCallback(ctx => element.set(Some(ctx.thisNode.ref)), _ => element.set(None)),
    )

    def revealAndFocus(): Unit = element.now().foreach { el =>
      val top = el.getBoundingClientRect().top
      if top < 0 || top > dom.window.innerHeight / 2 then el.scrollIntoView(true)
      // On the next frame, so a form whose content was just replaced is rendered first.
      val _ = dom.window.requestAnimationFrame { _ =>
        el.querySelector(formFirstFieldSelector) match
          case field: dom.HTMLElement =>
            field.focus(new dom.FocusOptions:
              preventScroll = true)
          case _ => ()
      }
    }
  end FormAnchor

  /** The first field of a form, which [[FormAnchor.revealAndFocus]] focuses. */
  private val formFirstFieldSelector = "input:not([disabled]):not([type='hidden']), select:not([disabled]), textarea:not([disabled])"

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
    selectInput(state.signal, state.writer, opts)

  /**
   * Variant of [[selectInput]] that accepts a `Signal` for reading and an `Observer` for writing. The `<option>`s are bound before the controlled
   * value, and the value is re-applied whenever the options change: a browser drops a `select` value that matches no option yet, so a select mounted
   * with a value (or whose options load after it) would otherwise show its first option instead of the bound value.
   */
  def selectInput(valueSignal: Signal[String], writer: Observer[String], opts: Signal[List[(String, String)]]): HtmlElement =
    select(
      cls := inputCls,
      children <-- opts.map(_.map { case (optValue, optLabel) => option(value := optValue, optLabel) }),
      controlled(value <-- valueSignal.combineWith(opts).map((selected, _) => selected), onChange.mapToValue --> writer),
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

  /** Sequence behind the DOM ids that shared components generate for ARIA references (listbox, dialog title). */
  private var domIdSeq = 0

  private def nextDomId(prefix: String): String =
    domIdSeq += 1
    s"$prefix-$domIdSeq"

  /**
   * The combobox behind both [[searchableSelect]] variants: `matches(typed, searchText)` decides which options are listed while typing. It follows
   * the ARIA combobox pattern: the list opens on click, on typing and on ArrowDown (not on focus, so a programmatic focus never covers the field's
   * error); ArrowUp/ArrowDown move the active option (`aria-activedescendant`, kept scrolled into view), Enter picks it, Escape closes the list.
   */
  private def combobox(
      valueSignal: Signal[String],
      writer: Observer[String],
      opts: Signal[List[(String, String, String)]],
      matches: (String, String) => Boolean,
      placeholderText: String,
      maxResults: Int,
      inputMods: Modifier[HtmlElement],
  ): HtmlElement =
    val listId = nextDomId("combobox-list")
    def optionId(index: Int): String = s"$listId-opt-$index"
    // `None` = not editing (show the selected label); `Some(text)` = the search text being typed.
    val query = Var(Option.empty[String])
    val open = Var(false)
    // Index of the highlighted option among the visible ones; -1 = none.
    val active = Var(-1)
    // Synchronous mirrors read by the keyboard handlers.
    val latestOpts = Var(List.empty[(String, String, String)])
    val latestValue = Var("")
    // Visible matches plus how many further matches were cut off by `maxResults`.
    def visibleOf(typed: Option[String], options: List[(String, String, String)]): (List[(String, String, String)], Int) =
      val matching = typed.fold(options)(text => options.filter((_, _, searchText) => matches(text, searchText)))
      (matching.take(maxResults), (matching.size - maxResults).max(0))
    def visibleNow(): List[(String, String, String)] = visibleOf(query.now(), latestOpts.now())._1
    val displayValue: Signal[String] =
      query.signal.combineWith(valueSignal, opts).map { case (typed, selected, options) =>
        typed.getOrElse(options.collectFirst { case (id, labelText, _) if id == selected => labelText }.getOrElse(""))
      }
    val filtered: Signal[(List[(String, String, String)], Int)] = query.signal.combineWith(opts).map(visibleOf)
    val activeDescendant: Signal[String] =
      open.signal.combineWith(active.signal, filtered).map { case (isOpen, index, (options, _)) =>
        if isOpen && index >= 0 && index < options.size then optionId(index) else ""
      }

    def scrollActiveIntoView(): Unit =
      (dom.document.getElementById(listId), dom.document.getElementById(optionId(active.now()))) match
        case (list: dom.HTMLElement, item: dom.HTMLElement) =>
          if item.offsetTop < list.scrollTop then list.scrollTop = item.offsetTop
          else if item.offsetTop + item.offsetHeight > list.scrollTop + list.clientHeight then
            list.scrollTop = item.offsetTop + item.offsetHeight - list.clientHeight
        case _ => ()
    def openList(): Unit =
      if !open.now() then
        active.set(ComboboxNav.initial(visibleNow().map(_._1), latestValue.now()))
        open.set(true)
        scrollActiveIntoView()
    def close(): Unit =
      open.set(false)
      query.set(None)
      active.set(-1)
    def moveActive(delta: Int): Unit =
      active.set(ComboboxNav.move(active.now(), delta, visibleNow().size))
      scrollActiveIntoView()
    def choose(id: String): Unit =
      writer.onNext(id)
      close()
    def onKey(event: dom.KeyboardEvent): Unit = event.key match
      case "ArrowDown" =>
        event.preventDefault()
        if !open.now() then openList() else if !event.altKey then moveActive(1)
      case "ArrowUp" =>
        if open.now() then
          event.preventDefault()
          moveActive(-1)
      case "Enter" =>
        if open.now() then
          visibleNow().lift(active.now()).foreach { (id, _, _) =>
            event.preventDefault()
            choose(id)
          }
      case "Escape" =>
        // Consumed only while there is something to close, so a second Escape still reaches an enclosing dialog.
        if open.now() || query.now().isDefined then
          event.stopPropagation()
          close()
      case _ => ()

    div(
      cls := "relative",
      opts --> latestOpts.writer,
      valueSignal --> latestValue.writer,
      input(
        typ := "text",
        cls := inputCls,
        placeholder := placeholderText,
        role := "combobox",
        aria.autoComplete := "list",
        aria.controls := listId,
        aria.expanded <-- open.signal,
        aria.activeDescendant <-- activeDescendant,
        autoComplete := "off",
        spellCheck := false,
        controlled(
          value <-- displayValue,
          onInput.mapToValue --> { text =>
            query.set(Some(text))
            active.set(0)
            open.set(true)
          },
        ),
        onClick --> (_ => openList()),
        // Selection happens on the options' mousedown, which fires before this blur.
        onBlur --> (_ => close()),
        onKeyDown --> (event => onKey(event)),
        inputMods,
      ),
      // The listbox is always in the DOM (hidden while closed), so the input's `aria-controls` always points to an existing element.
      div(
        idAttr := listId,
        role := "listbox",
        cls := "absolute z-50 mt-1 max-h-56 w-full overflow-y-auto rounded-md border border-slate-200 bg-white shadow-lg",
        cls <-- open.signal.map(isOpen => if isOpen then "" else "hidden"),
        children <-- open.signal.combineWith(filtered).map { case (isOpen, options, hiddenCount) =>
          if !isOpen then Nil
          else if options.isEmpty then List(div(cls := "px-2.5 py-2 text-sm text-slate-400", "Nessun risultato"))
          else
            options.zipWithIndex.map { case ((id, labelText, _), index) =>
              div(
                idAttr := optionId(index),
                role := "option",
                aria.selected <-- valueSignal.map(_ == id),
                cls := "cursor-pointer px-2.5 py-1.5 text-sm text-slate-700",
                cls <-- active.signal.map(current => if current == index then "bg-slate-100" else ""),
                cls <-- valueSignal.map(selected => if selected == id then "font-medium" else ""),
                labelText,
                onMouseEnter --> (_ => active.set(index)),
                // `mousedown` (not `click`) so it wins over the input's blur; preventDefault keeps the input focused.
                onMouseDown.preventDefault --> (_ => choose(id)),
              )
            } ++ Option
              .when(hiddenCount > 0)(
                div(
                  cls := "border-t border-slate-100 px-2.5 py-1.5 text-xs text-slate-400",
                  s"… e altri $hiddenCount risultati — continua a digitare per affinare la ricerca",
                ),
              )
              .toList
        },
      ),
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

  /** Combines a dynamic list of signals into one signal of their values; an empty list gives a constant empty list (`combineSeq` alone would not). */
  def combineAll[A](signals: List[Signal[A]]): Signal[List[A]] =
    if signals.isEmpty then Signal.fromValue(List.empty[A]) else Signal.combineSeq(signals).map(_.toList)

  /**
   * The inline error under a control, with DOM id `id` (the target of the control's `aria-describedby`): a polite live region showing `error`, empty
   * while there is none.
   */
  def fieldError(id: String, error: Signal[Option[String]]): HtmlElement =
    div(
      idAttr := id,
      aria.live := "polite",
      cls <-- error.map(current => if current.isDefined then "mt-1 text-xs font-medium text-rose-600" else ""),
      child.text <-- error.map(_.getOrElse("")),
    )

  /** The legend of a form with required fields: "I campi con * sono obbligatori.", the `*` styled like the fields' required markers. */
  def requiredLegend: HtmlElement =
    p(
      cls := "text-xs text-slate-500",
      "I campi con ",
      span(aria.hidden := true, cls := "text-rose-600", "*"),
      span(cls := "sr-only", "asterisco"),
      " sono obbligatori.",
    )

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
      fieldError(spec.errorId, spec.error),
    )
  end field

  /** Segmented single choice (a few mutually exclusive options, e.g. priority): `opts` are `(value, label)` pairs. */
  def segmented(valueSignal: Signal[String], writer: Observer[String], opts: List[(String, String)]): HtmlElement =
    segmented(valueSignal, writer, opts, emptyMod)

  /**
   * Variant of [[segmented]] with `groupMods` on the `radiogroup` element (e.g. a [[FieldSpec]] `bind`, or `aria.label`). Each option is a `button`
   * with `role=radio` and `aria-checked`, following the ARIA radio group pattern: the group is a single Tab stop (the checked option, or the first
   * when none is), Left/Up and Right/Down select the previous/next option (wrapping), Home/End the first/last, and the focus follows the selection.
   * The options stay mounted when the value changes, so a caller rebuilding other content on a switch keeps the focus here. Options are 40px tall
   * below `md` (touch target).
   */
  def segmented(valueSignal: Signal[String], writer: Observer[String], opts: List[(String, String)], groupMods: Modifier[HtmlElement]): HtmlElement =
    val values = opts.map(_._1)
    val tabStop = valueSignal.map(RadioGroupNav.tabStop(values, _))
    val options = opts.zipWithIndex.map { case ((optValue, optLabel), index) =>
      val active = valueSignal.map(_ == optValue)
      button(
        tpe := "button",
        role := "radio",
        aria.checked <-- active.map(isActive => if isActive then "true" else "false"),
        tabIndex <-- tabStop.map(stop => if stop == index then 0 else -1),
        cls := "min-h-10 rounded px-3 py-1 text-sm font-medium transition-colors md:min-h-0",
        cls := "focus:outline-none focus-visible:ring-2 focus-visible:ring-slate-500",
        cls <-- active.map(isActive => if isActive then "bg-slate-900 text-white shadow-sm" else "text-slate-600 hover:bg-slate-200"),
        optLabel,
        onClick --> (_ => writer.onNext(optValue)),
      )
    }
    options.zipWithIndex.foreach { (option, index) =>
      option.amend(
        onKeyDown --> { event =>
          RadioGroupNav.target(event.key, index, options.size).foreach { next =>
            event.preventDefault()
            writer.onNext(values(next))
            options(next).ref.focus()
          }
        },
      )
    }
    div(
      role := "radiogroup",
      cls := "inline-flex flex-wrap gap-1 rounded-md border border-slate-300 bg-slate-50 p-0.5",
      groupMods,
      options,
    )
  end segmented

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
   * screens `mobileBar` fixed at the bottom instead (the main column gets bottom padding so the bar never covers its end). While mounted, the
   * document gets a scroll padding (bottom below `lg`, top for the sticky header), so a control reached with Tab is scrolled clear of the bar and the
   * header.
   */
  def summaryLayout(main: HtmlElement, sidebar: HtmlElement, mobileBar: HtmlElement): HtmlElement =
    div(
      onMountUnmountCallback(
        _ => summaryScrollPaddingCls.foreach(dom.document.documentElement.classList.add),
        _ => summaryScrollPaddingCls.foreach(dom.document.documentElement.classList.remove),
      ),
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

  /** Scroll padding of the document while a [[summaryLayout]] is mounted (literal Tailwind classes, so the scanner generates them). */
  private val summaryScrollPaddingCls = List("max-lg:scroll-pb-32", "scroll-pt-24")

  /** One `label · value` line of a summary. */
  def summaryRow(labelText: String, value: Signal[String]): HtmlElement =
    div(
      cls := "flex items-baseline justify-between gap-3 text-sm",
      span(cls := "shrink-0 text-slate-500", labelText),
      span(cls := "min-w-0 truncate text-right font-medium text-slate-800", child.text <-- value),
    )

  /**
   * The checklist of what is still missing: each issue is a full-width button that calls `onSelect` (e.g. reveal and focus the field). An issue is
   * neutral (slate/amber) until `emphasize` says so (typically once a submit was attempted, see [[FormTouch.submittedSignal]]), then red; "✓ Pronto"
   * when empty. A polite live region. Re-renders on every emission of `issues`, so pass a `distinct` signal.
   */
  def issueList[A](issues: Signal[List[A]], text: A => String, emphasize: Signal[A => Boolean], onSelect: A => Unit): HtmlElement =
    div(
      aria.live := "polite",
      child <-- issues.combineWith(emphasize).map { (list, isStrong) =>
        if list.isEmpty then div(cls := "text-sm font-medium text-emerald-700", "✓ Pronto")
        else
          ul(
            cls := "-mx-2 space-y-0.5",
            list.map { issue =>
              val strong = isStrong(issue)
              li(
                button(
                  tpe := "button",
                  cls := "flex min-h-9 w-full items-center gap-1.5 rounded-md px-2 py-1 text-left text-xs hover:bg-slate-50 hover:underline focus:outline-none focus-visible:ring-2 focus-visible:ring-slate-400",
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
    badge(OrderStatus.label(status).replace('_', ' ').nn, color)

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

  /** `aria-modal`, which Laminar does not predefine. */
  private val ariaModal: HtmlAttr[String] = htmlAttr("aria-modal", StringAsIsCodec)

  /** Elements that take the keyboard focus inside a dialog. */
  private val focusableSelector =
    "a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex='-1'])"

  /** The rendered (not `hidden`) focusable elements inside `root`, in document order. */
  private def focusablesIn(root: dom.Element): List[dom.HTMLElement] =
    val nodes = root.querySelectorAll(focusableSelector)
    (0 until nodes.length).toList.flatMap { index =>
      nodes(index) match
        case element: dom.HTMLElement if element.getClientRects().length > 0 => Some(element)
        case _ => None
    }

  /** Keeps Tab / Shift+Tab cycling among the focusable elements of `root` (a dialog), so the focus never reaches the page underneath. */
  private def trapTab(root: dom.Element, event: dom.KeyboardEvent): Unit =
    // Handled by the innermost dialog only (a dialog may sit inside another one's content).
    event.stopPropagation()
    val items = focusablesIn(root)
    val current = dom.document.activeElement
    (items.headOption, items.lastOption) match
      case (Some(first), Some(last)) =>
        val outside = !root.contains(current)
        if event.shiftKey && (outside || (current eq first)) then
          event.preventDefault()
          last.focus()
        else if !event.shiftKey && (outside || (current eq last)) then
          event.preventDefault()
          first.focus()
      case _ => event.preventDefault()

  /**
   * Focus handling of a dialog shown while `isOpen`: on opening it remembers the focused element and, on the next frame (once the dialog is
   * displayed), focuses `initial()`; on closing it gives the focus back to the remembered element if it is still in the page.
   */
  private def dialogFocus(isOpen: Signal[Boolean], initial: () => Option[dom.HTMLElement]): Modifier[HtmlElement] =
    var wasOpen = false
    var previous = Option.empty[dom.HTMLElement]
    isOpen --> { open =>
      if open && !wasOpen then
        previous = dom.document.activeElement match
          case element: dom.HTMLElement => Some(element)
          case _ => None
        val _ = dom.window.requestAnimationFrame(_ => initial().foreach(_.focus()))
      else if !open && wasOpen then
        previous.filter(element => dom.document.body.contains(element)).foreach(_.focus())
        previous = None
      wasOpen = open
    }

  /** Visual tone of a [[confirmDialog]]: destructive actions get a warning look, everything else a neutral one. */
  enum DialogTone derives CanEqual:
    case Danger, Neutral

  /**
   * A compact, centered confirmation dialog — the house style for "are you sure?" prompts across the app. Shows a tonal icon, a title and message,
   * and a Cancel/Confirm pair (stacked on mobile, inline on desktop). Clicking the backdrop cancels. Drive `isOpen`/`titleText`/`message` from the
   * caller's state (typically a `Var[Option[T]]`); `onCancel`/`onConfirm` should also flip that state closed.
   *
   * It is an ARIA `alertdialog`: on opening the focus moves to the safest choice ("Annulla" for a [[DialogTone.Danger]] dialog, otherwise the first
   * field of `body` or the confirm button), Tab cycles inside the dialog, Escape cancels, and on closing the focus returns where it was.
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
    confirmDialog(isOpen, titleText, message, confirmLabel, "Annulla", tone, body, confirmEnabled, onCancel, onConfirm)

  /** Variant of [[confirmDialog]] with a custom cancel label (e.g. "Continua a modificare" when discarding a draft). */
  def confirmDialog(
      isOpen: Signal[Boolean],
      titleText: Signal[String],
      message: Signal[String],
      confirmLabel: String,
      cancelLabel: String,
      tone: DialogTone,
      onCancel: () => Unit,
      onConfirm: () => Unit,
  ): HtmlElement =
    confirmDialog(isOpen, titleText, message, confirmLabel, cancelLabel, tone, emptyNode, Signal.fromValue(true), onCancel, onConfirm)

  private def confirmDialog(
      isOpen: Signal[Boolean],
      titleText: Signal[String],
      message: Signal[String],
      confirmLabel: String,
      cancelLabel: String,
      tone: DialogTone,
      body: Modifier[HtmlElement],
      confirmEnabled: Signal[Boolean],
      onCancel: () => Unit,
      onConfirm: () => Unit,
  ): HtmlElement =
    val (iconWrapCls, iconChar, confirmCls) = tone match
      case DialogTone.Danger => ("bg-rose-100 text-rose-600", "⚠", btnDangerSolid)
      case DialogTone.Neutral => ("bg-sky-100 text-sky-600", "?", s"$btnPrimary justify-center")
    val titleId = nextDomId("dialog-title")
    val messageId = nextDomId("dialog-message")
    val cancelButton = button(tpe := "button", cls := s"$btnGhost flex-1 justify-center", cancelLabel, onClick --> (_ => onCancel()))
    val confirmButton =
      button(tpe := "button", cls := s"$confirmCls flex-1", confirmLabel, disabled <-- confirmEnabled.map(!_), onClick --> (_ => onConfirm()))
    val bodyBox = div(body)
    val dialogCard = card(
      cls := "p-6",
      role := "alertdialog",
      ariaModal := "true",
      aria.labelledBy := titleId,
      aria.describedBy := messageId,
      onClick.stopPropagation --> (_ => ()),
      div(
        cls := "flex flex-col items-center gap-4 text-center",
        div(
          aria.hidden := true,
          cls := s"flex h-12 w-12 items-center justify-center rounded-full $iconWrapCls",
          span(cls := "text-2xl leading-none", iconChar),
        ),
        div(
          cls := "space-y-1",
          h2(idAttr := titleId, cls := "text-base font-semibold text-slate-900", child.text <-- titleText),
          p(idAttr := messageId, cls := "text-sm text-slate-500", child.text <-- message),
        ),
      ),
      bodyBox,
      div(cls := "mt-6 flex flex-col-reverse gap-2 sm:flex-row", cancelButton, confirmButton),
    )
    def initialFocus(): Option[dom.HTMLElement] = tone match
      case DialogTone.Danger => Some(cancelButton.ref)
      case DialogTone.Neutral => focusablesIn(bodyBox.ref).headOption.orElse(focusablesIn(dialogCard.ref).lastOption)
    div(
      cls := "fixed inset-0 z-50 items-center justify-center overflow-y-auto bg-slate-900/50 p-4",
      cls <-- isOpen.map(open => if open then "flex" else "hidden"),
      // Clicking the backdrop (but not the card) cancels.
      onClick --> (_ => onCancel()),
      onKeyDown --> { event =>
        event.key match
          case "Escape" =>
            event.stopPropagation()
            onCancel()
          case "Tab" => trapTab(dialogCard.ref, event)
          case _ => ()
      },
      dialogFocus(isOpen, () => initialFocus()),
      div(cls := "w-full max-w-sm", dialogCard),
    )
  end confirmDialog

  def modal(isOpen: Var[Boolean], titleText: String)(content: HtmlElement): HtmlElement =
    modal(isOpen, Signal.fromValue(titleText))(content)

  def modal(isOpen: Var[Boolean], titleText: Signal[String])(content: HtmlElement): HtmlElement =
    modal(isOpen.signal, titleText, "max-w-2xl", () => isOpen.set(false))(content)

  /**
   * Variant of [[modal]] driven by a `Signal` (typically derived from a `Var[Option[T]]` holding what the modal is about), with the card width as a
   * Tailwind `max-w-*` class; `onClose` runs when the ✕ is clicked or Escape is pressed, and should flip the caller's state closed. An ARIA `dialog`
   * labelled by its title: on opening the focus moves into the card, Tab cycles inside it, and on closing the focus returns where it was.
   */
  def modal(isOpen: Signal[Boolean], titleText: Signal[String], maxWidthCls: String, onClose: () => Unit)(content: HtmlElement): HtmlElement =
    val titleId = nextDomId("dialog-title")
    val dialogCard = card(
      role := "dialog",
      ariaModal := "true",
      aria.labelledBy := titleId,
      // Focusable only programmatically: the initial focus lands on the card itself, not on the first control (which may be a combobox).
      tabIndex := -1,
      cls := "focus:outline-none",
      div(
        cls := "flex items-center justify-between border-b border-slate-100 px-4 py-3",
        h2(idAttr := titleId, cls := "text-sm font-semibold text-slate-800", child.text <-- titleText),
        button(
          tpe := "button",
          // 40px touch target below `md`; the negative margins keep the header height unchanged.
          cls := "-my-2.5 -mr-2 inline-flex size-10 shrink-0 items-center justify-center rounded-md text-slate-400 hover:text-slate-700 " +
            "md:m-0 md:size-auto",
          aria.label := "Chiudi",
          title := "Chiudi",
          "✕",
          onClick --> (_ => onClose()),
        ),
      ),
      div(cls := "p-4", content),
    )
    div(
      cls := "fixed inset-0 z-40 items-start justify-center overflow-y-auto bg-slate-900/40 p-4",
      cls <-- isOpen.map(open => if open then "flex" else "hidden"),
      onKeyDown --> { event =>
        event.key match
          case "Escape" =>
            event.stopPropagation()
            onClose()
          case "Tab" => trapTab(dialogCard.ref, event)
          case _ => ()
      },
      dialogFocus(isOpen, () => Some(dialogCard.ref)),
      // When the focused control was re-rendered away (e.g. after removing a row) the focus falls back to <body>, out of the handler above:
      // Escape still closes and Tab re-enters the card.
      documentEvents(_.onKeyDown).withCurrentValueOf(isOpen) --> { (event, open) =>
        val focusLost = dom.document.activeElement match
          case element: dom.HTMLElement => element eq dom.document.body
          case _ => true
        if open && focusLost then
          event.key match
            case "Escape" => onClose()
            case "Tab" =>
              event.preventDefault()
              dialogCard.ref.focus()
            case _ => ()
      },
      div(cls := s"mt-8 w-full $maxWidthCls", dialogCard),
    )
  end modal
end Components

/**
 * Pure keyboard navigation of the combobox list ([[Components.searchableSelect]]): which option is active (highlighted) among `size` visible ones, as
 * an index where -1 means none.
 */
private[ui] object ComboboxNav:

  /** The active index after moving by `delta` from `current`, wrapping around; from none, down starts at the first and up at the last. */
  def move(current: Int, delta: Int, size: Int): Int =
    if size <= 0 then -1
    else if current < 0 || current >= size then (if delta >= 0 then 0 else size - 1)
    else Math.floorMod(current + delta, size)

  /** The option active when the list opens: the selected value if it is listed, else the first one; none when the list is empty. */
  def initial(ids: List[String], selected: String): Int =
    if ids.isEmpty then -1 else ids.indexOf(selected).max(0)
end ComboboxNav

/**
 * Pure keyboard navigation of a radio group ([[Components.segmented]]), over `size` options by index: the single Tab stop and the option an arrow,
 * Home, or End key moves to.
 */
private[ui] object RadioGroupNav:

  /** The option index after `key` from `current`: Left/Up and Right/Down step (wrapping), Home/End jump; `None` for any other key. */
  def target(key: String, current: Int, size: Int): Option[Int] =
    val next = key match
      case "ArrowRight" | "ArrowDown" => Some(ComboboxNav.move(current, 1, size))
      case "ArrowLeft" | "ArrowUp" => Some(ComboboxNav.move(current, -1, size))
      case "Home" => Some(if size > 0 then 0 else -1)
      case "End" => Some(size - 1)
      case _ => None
    next.filter(_ >= 0)

  /** The option reachable with Tab: the checked one, else the first; -1 when there are no options. */
  def tabStop(values: List[String], selected: String): Int = ComboboxNav.initial(values, selected)
end RadioGroupNav
