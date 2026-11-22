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

  /** The first field of a form, which [[formPage]] focuses for [[PageFocus.FirstField]]. */
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
   * The options stay mounted when the value changes, so a caller rebuilding other content on a switch keeps the focus here. When the group itself
   * gets the focus (e.g. a page or [[FormFocus.focus]] focusing its DOM id), it hands it to the checked option, so the arrow keys work at once.
   * Options are 40px tall below `md` (touch target).
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
      inContext { group =>
        onFocus --> { event =>
          if event.target.eq(group.ref) then
            group.ref.querySelector("[aria-checked='true']") match
              case option: dom.HTMLElement => option.focus()
              case _ => ()
        }
      },
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

  /**
   * Variant of [[summaryLayout]] without the mobile bar (read-only pages): no fixed bar and no bottom padding for it; below `lg` the sidebar is not
   * shown, so `main` should carry everything the sidebar summarizes. The document gets the top scroll padding only.
   */
  def summaryLayout(main: HtmlElement, sidebar: HtmlElement): HtmlElement =
    div(
      onMountUnmountCallback(
        _ => dom.document.documentElement.classList.add(summaryTopScrollPaddingCls),
        _ => dom.document.documentElement.classList.remove(summaryTopScrollPaddingCls),
      ),
      div(
        cls := "grid gap-6 lg:grid-cols-[minmax(0,1fr)_20rem]",
        div(cls := "min-w-0", main),
        asideTag(cls := "hidden self-start lg:sticky lg:top-20 lg:block", sidebar),
      ),
    )

  /**
   * The content of the fixed bottom bar of a [[summaryLayout]]: `leading` (live totals or changes, truncated), `extra` (e.g. an [[issueBadge]]), and
   * the primary button, showing `busyLabel` while `busy` and disabled while `disabledSignal`.
   */
  def mobileSubmitBar(
      leading: Modifier[HtmlElement],
      extra: Modifier[HtmlElement],
      primaryLabel: String,
      busyLabel: String,
      busy: Signal[Boolean],
      disabledSignal: Signal[Boolean],
      onPrimary: () => Unit,
  ): HtmlElement =
    div(
      cls := "flex items-center gap-3",
      div(cls := "min-w-0 flex-1 truncate text-sm tabular-nums text-slate-700", leading),
      extra,
      button(
        tpe := "button",
        cls := s"$btnPrimary min-h-10 shrink-0",
        disabled <-- disabledSignal,
        aria.busy <-- busy,
        child.text <-- busy.map(isBusy => if isBusy then busyLabel else primaryLabel),
        onClick --> (_ => onPrimary()),
      ),
    )

  /** The "⚠ n" button of a [[mobileSubmitBar]] (nothing while `count` is 0), going to the first issue. */
  def issueBadge(count: Signal[Int], onSelect: () => Unit): Modifier[HtmlElement] =
    child.maybe <-- count.map { n =>
      Option.when(n > 0)(
        button(
          tpe := "button",
          cls := "min-h-10 shrink-0 rounded-full bg-amber-50 px-2.5 py-1 text-xs font-medium text-amber-700 hover:bg-amber-100",
          aria.label := s"$n elementi da completare: vai al primo",
          s"⚠ $n",
          onClick --> (_ => onSelect()),
        ),
      )
    }

  /** Top scroll padding of the document while a [[summaryLayout]] is mounted (for the sticky header). */
  private val summaryTopScrollPaddingCls = "scroll-pt-24"

  /** Scroll padding of the document while a [[summaryLayout]] is mounted (literal Tailwind classes, so the scanner generates them). */
  private val summaryScrollPaddingCls = List("max-lg:scroll-pb-32", summaryTopScrollPaddingCls)

  /** `text`, or "—" when it is blank (an empty value of a summary or read-only field). */
  def orDash(text: String): String = if text.trim.nn.isEmpty then "—" else text

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

  /**
   * Variant of [[formActions]] whose submit is also disabled while `blocked` is `Some(reason)` (e.g. the edited record no longer exists); the reason
   * shows as a hint next to the buttons (above them on mobile). Otherwise the same contract as [[formActions]].
   */
  def formActions(
      submitLabel: String,
      busyLabel: String,
      submitting: Signal[Boolean],
      blocked: Signal[Option[String]],
      onCancel: Option[() => Unit],
      onSubmit: () => Unit,
  ): HtmlElement =
    div(
      cls := "flex flex-col-reverse gap-2 border-t border-slate-100 pt-3 sm:flex-row sm:items-center sm:justify-end",
      actionButtons(submitLabel, busyLabel, submitting, blocked, onCancel, onSubmit),
      blockedHint(blocked, "sm:order-first sm:mr-auto"),
    )

  /**
   * Variant of [[formActions]] for a bar ([[stickyActionBar]]): no top border or padding, the buttons in one row on the right, 40px tall below `md`,
   * the primary one rightmost and widest. Same contract as [[formActions]].
   */
  def formActionsInline(
      submitLabel: String,
      busyLabel: String,
      submitting: Signal[Boolean],
      onCancel: Option[() => Unit],
      onSubmit: () => Unit,
  ): HtmlElement =
    formActionsInline(submitLabel, busyLabel, submitting, Signal.fromValue(None), onCancel, onSubmit)

  /** Variant of [[formActionsInline]] whose submit is also disabled while `blocked` is `Some(reason)`, shown as a hint before the buttons. */
  def formActionsInline(
      submitLabel: String,
      busyLabel: String,
      submitting: Signal[Boolean],
      blocked: Signal[Option[String]],
      onCancel: Option[() => Unit],
      onSubmit: () => Unit,
  ): HtmlElement =
    div(
      cls := "ml-auto flex flex-wrap items-center justify-end gap-2",
      blockedHint(blocked, ""),
      div(
        cls := "flex items-center gap-2 *:min-h-10 md:*:min-h-0",
        actionButtons(submitLabel, busyLabel, submitting, blocked, onCancel, onSubmit).map(_.amend(cls := "min-w-24")),
      ),
    )

  /** The cancel (when `onCancel` is defined) and submit buttons shared by the [[formActions]] variants that take a `blocked` reason. */
  private def actionButtons(
      submitLabel: String,
      busyLabel: String,
      submitting: Signal[Boolean],
      blocked: Signal[Option[String]],
      onCancel: Option[() => Unit],
      onSubmit: () => Unit,
  ): List[HtmlElement] =
    onCancel.toList.map(cancel => button(tpe := "button", cls := s"$btnGhost justify-center", "Annulla", onClick --> (_ => cancel()))) :+
      button(
        tpe := "button",
        cls := s"$btnPrimary justify-center",
        disabled <-- submitting.combineWith(blocked).map((busy, reason) => busy || reason.isDefined),
        aria.busy <-- submitting,
        child.text <-- submitting.map(busy => if busy then busyLabel else submitLabel),
        onClick --> (_ => onSubmit()),
      )

  /** Why the submit is disabled (a polite live region), empty while `blocked` is `None`; `extraCls` positions it in its row. */
  private def blockedHint(blocked: Signal[Option[String]], extraCls: String): HtmlElement =
    p(
      aria.live := "polite",
      cls := s"text-xs text-slate-500 $extraCls",
      cls <-- blocked.map(reason => if reason.isDefined then "" else "hidden"),
      child.text <-- blocked.map(_.getOrElse("")),
    )

  /** "Modifiche da salvare" checklist (complex edit pages): same look as [[issueList]] with neutral slate bullets, "Nessuna modifica" when empty. */
  def changeList[A](changes: Signal[List[A]], text: A => String, onSelect: A => Unit): HtmlElement =
    div(
      aria.live := "polite",
      child <-- changes.map { list =>
        if list.isEmpty then div(cls := "text-sm text-slate-500", "Nessuna modifica")
        else
          ul(
            cls := "-mx-2 space-y-0.5",
            list.map { change =>
              li(
                button(
                  tpe := "button",
                  cls := "flex min-h-9 w-full items-center gap-1.5 rounded-md px-2 py-1 text-left text-xs text-slate-600 hover:bg-slate-50 hover:underline focus:outline-none focus-visible:ring-2 focus-visible:ring-slate-400",
                  span(aria.hidden := true, cls := "text-slate-400", "•"),
                  span(text(change)),
                  onClick --> (_ => onSelect(change)),
                ),
              )
            },
          )
      },
    )

  // ---- Form page ---------------------------------------------------------------------------------

  /** Where the focus lands when a [[formPage]] mounts. */
  enum PageFocus derives CanEqual:
    /** The page `h1` (edit and read-only pages): it announces the page, and Tab next reaches the back link. */
    case Title

    /** The first enabled field of the page (create pages). */
    case FirstField

    /** The element with DOM id `id`, through [[FormFocus.focus]] (e.g. the "Override" section, the movement kind switch). */
    case Element(id: String)

  /**
   * What the header of a [[formPage]] shows.
   *
   * @param backLabel
   *   the back link, e.g. "← Torna ai clienti"
   * @param onBack
   *   the GUARDED leave, the same as "Annulla" (e.g. `() => draft.discardThen(view.close)`); the browser back button runs it too
   * @param title
   *   the `h1`: "Nuovo cliente", "Modifica cliente «Mario Rossi»" (named from the loaded snapshot, so fixed per opening in practice)
   * @param context
   *   the context trail, joined with " / " (e.g. `List("Ordini", "ORD-2026-014")`); plain text, not links; empty = none
   * @param status
   *   badges next to the title (text badges, never color only)
   * @param actions
   *   `btnSmall` header actions on the right of the title
   * @param notes
   *   [[infoNote]] / [[warningNote]] under the title (read-only reason, record no longer exists)
   * @param requiredLegend
   *   whether to show "I campi con * sono obbligatori." (false on read-only pages)
   * @param initialFocus
   *   where the focus lands on mount: [[PageFocus.Title]] for edit and read-only pages, [[PageFocus.FirstField]] for create pages
   */
  final case class PageHeader(
      backLabel: String,
      onBack: () => Unit,
      title: Signal[String],
      context: List[String] = Nil,
      status: Modifier[HtmlElement] = emptyMod,
      actions: Modifier[HtmlElement] = emptyMod,
      notes: Modifier[HtmlElement] = emptyMod,
      requiredLegend: Boolean = true,
      initialFocus: PageFocus = PageFocus.Title,
  )

  /**
   * A full-page form shown in place of its list (see [[SubView]]): the header (back link with a 40px touch target below `md`, context trail, `h1`
   * with `tabindex=-1`, status, actions, legend, notes), then `body` (a [[formColumn]] for a simple page, a [[summaryLayout]] for a complex one). On
   * mount it scrolls to the top and focuses `header.initialFocus` on the next frame. It binds [[SubViewHistory]], so the browser (or Android) back
   * button runs `header.onBack`.
   */
  def formPage(header: PageHeader)(body: Modifier[HtmlElement]*): HtmlElement =
    val heading = h1(
      tabIndex := -1,
      cls := "min-w-0 text-xl font-semibold text-slate-900 focus:outline-none",
      child.text <-- header.title,
    )
    div(
      cls := "space-y-6",
      SubViewHistory.bind(header.onBack),
      onMountCallback { ctx =>
        dom.window.scrollTo(0, 0)
        val root = ctx.thisNode.ref
        header.initialFocus match
          case PageFocus.Element(id) => FormFocus.focus(id)
          case PageFocus.Title =>
            val _ = dom.window.requestAnimationFrame(_ => focusWithoutScroll(heading.ref))
          case PageFocus.FirstField =>
            val _ = dom.window.requestAnimationFrame { _ =>
              root.querySelector(formFirstFieldSelector) match
                case first: dom.HTMLElement => focusWithoutScroll(first)
                case _ => focusWithoutScroll(heading.ref)
            }
      },
      div(
        button(tpe := "button", cls := s"$btnLink min-h-10 md:min-h-0", header.backLabel, onClick --> (_ => header.onBack())),
        if header.context.isEmpty then emptyNode else p(cls := "mt-2 text-xs text-slate-500", header.context.mkString(" / ")),
        div(
          cls := "mt-2 flex flex-wrap items-center gap-x-3 gap-y-2",
          heading,
          div(cls := "flex flex-wrap items-center gap-2", header.status),
          div(cls := "ml-auto flex flex-wrap items-center gap-2", header.actions),
        ),
        if header.requiredLegend then requiredLegend.amend(cls := "mt-1") else emptyNode,
        div(cls := "mt-3 space-y-2 empty:hidden", header.notes),
      ),
      body,
    )
  end formPage

  private def focusWithoutScroll(element: dom.HTMLElement): Unit =
    element.focus(new dom.FocusOptions:
      preventScroll = true)

  /** The column of a simple form page: `max-w-3xl`, sections stacked with `space-y-6`, then usually a [[stickyActionBar]] as the last child. */
  def formColumn(content: Modifier[HtmlElement]*): HtmlElement =
    formColumn("max-w-3xl")(content*)

  /**
   * Variant of [[formColumn]] with another width as a Tailwind `max-w-*` class (e.g. "max-w-2xl" for the planning task, "max-w-6xl" with a table).
   */
  def formColumn(maxWidthCls: String)(content: Modifier[HtmlElement]*): HtmlElement =
    div(cls := s"w-full min-w-0 space-y-6 pb-4 $maxWidthCls", content)

  /**
   * The action bar of a simple form page: sticky at the bottom of its [[formColumn]] at all widths (in the flow, so at the end of the page it sits
   * after the last section instead of covering it), white/95 with a top border and backdrop blur, safe-area bottom padding, `z-20` (below the app
   * header and the dialogs). `leading` sits on the left (e.g. [[dirtyNote]], a live total), `actions` on the right (usually [[formActionsInline]]).
   * While mounted, the document gets a bottom scroll padding below `lg`, so a control reached with Tab is scrolled clear of the bar.
   */
  def stickyActionBar(leading: Modifier[HtmlElement], actions: HtmlElement): HtmlElement =
    div(
      onMountUnmountCallback(
        _ => dom.document.documentElement.classList.add(stickyBarScrollPaddingCls),
        _ => dom.document.documentElement.classList.remove(stickyBarScrollPaddingCls),
      ),
      cls := "sticky bottom-0 z-20 rounded-t-lg border-t border-slate-200 bg-white/95 px-4 pt-3 pb-[max(0.75rem,env(safe-area-inset-bottom))] backdrop-blur",
      div(
        cls := "flex flex-wrap items-center gap-x-3 gap-y-2",
        div(cls := "min-w-0 flex-1 text-sm text-slate-600", leading),
        actions,
      ),
    )

  /** Scroll padding of the document while a [[stickyActionBar]] is mounted (a literal Tailwind class, so the scanner generates it). */
  private val stickyBarScrollPaddingCls = "max-lg:scroll-pb-24"

  /** "● Modifiche non salvate" (amber) while `isDirty`, nothing otherwise; a polite live region, always mounted so the change is announced. */
  def dirtyNote(isDirty: Signal[Boolean]): HtmlElement =
    div(
      aria.live := "polite",
      cls := "text-xs font-medium text-amber-700",
      children <-- isDirty.map(dirty => if dirty then List(span(aria.hidden := true, "● "), span("Modifiche non salvate")) else Nil),
    )

  /** Neutral information box (slate), e.g. the read-only reason under a [[formPage]] title. */
  def infoNote(text: String): HtmlElement =
    div(role := "note", cls := "rounded-md border border-slate-200 bg-slate-50 p-3 text-sm text-slate-700", text)

  /** Warning variant of [[infoNote]] (amber), e.g. "the record no longer exists", "completed manufacturing: stock unload updated". */
  def warningNote(text: String): HtmlElement =
    div(role := "note", cls := "rounded-md border border-amber-200 bg-amber-50 p-3 text-sm text-amber-800", text)

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

  /**
   * Variant of [[errorBanner]] with a "Chiudi" button, for page-level messages not tied to a form (e.g. a refused delete over a list). An ARIA
   * `alert`; `onDismiss` should clear the caller's state. Give it an `idAttr` (via `amend`) to move the focus to it with [[FormFocus.focus]].
   */
  def errorBanner(err: ApiError, onDismiss: () => Unit): HtmlElement =
    div(
      role := "alert",
      cls := "flex items-start gap-3 rounded-md border border-rose-200 bg-rose-50 p-3 text-sm text-rose-700",
      div(
        cls := "min-w-0 flex-1",
        div(cls := "font-medium", err.message),
        if err.details.nonEmpty then ul(cls := "mt-1 list-disc pl-5 text-xs", err.details.map(d => li(d))) else emptyNode,
      ),
      button(
        tpe := "button",
        cls := "inline-flex min-h-10 shrink-0 items-center rounded-md border border-rose-300 px-2 py-1 text-xs font-medium text-rose-700 hover:bg-rose-100 md:min-h-0",
        "Chiudi",
        onClick --> (_ => onDismiss()),
      ),
    )

  def showError(target: Var[Option[ApiError]], context: String)(err: ApiError): Unit =
    ErrorCenter.reportTo(target, context)(err)

  /**
   * Variant of [[showError]] for a refused list action (e.g. a delete) whose banner over the list is the only feedback: logged, without the global
   * toast, so the user does not see the same error twice.
   */
  def showBannerError(target: Var[Option[ApiError]], context: String)(err: ApiError): Unit =
    ErrorCenter.logTo(target, context)(err)

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

  // ---- Dialogs -----------------------------------------------------------------------------------

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
