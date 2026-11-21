package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.FieldSpec
import org.scalajs.dom

/**
 * When the inline errors of a form become visible: a field's error shows once the field was left (`focusout` on its wrapper, which bubbles, so it
 * also covers the inner input of a combobox and groups of chips) or after the first submit attempt. Before that the form stays neutral.
 *
 * Fields are identified by their DOM id (the same one passed to [[FieldSpec]] and [[FormFocus.focus]]).
 */
final class FormTouch private (touched: Var[Set[String]], submitted: Var[Boolean], submitCounts: Signal[Boolean]):

  def this() = this(Var(Set.empty[String]), Var(false), Signal.fromValue(true))

  /**
   * A view of this tracker for a group of fields (e.g. a card added after a failed submit) where a past submit attempt makes errors visible only
   * while `submitCounts` is true; touches and submit attempts are shared with this tracker.
   */
  def scoped(submitCounts: Signal[Boolean]): FormTouch = new FormTouch(touched, submitted, submitCounts)

  /** Marks one field as visited. */
  def touch(id: String): Unit = touched.update(_ + id)

  /** Marks several fields as visited (e.g. "Fatto ✓" on a collapsed card). */
  def touchAll(ids: Iterable[String]): Unit = touched.update(_ ++ ids)

  /** Records a submit attempt: from now on every error is visible. */
  def attemptSubmit(): Unit = submitted.set(true)

  /** Back to the neutral state (e.g. after a successful submit or when the form is reset). */
  def reset(): Unit =
    touched.set(Set.empty)
    submitted.set(false)

  /** Whether a submit was attempted, read synchronously (event handlers). */
  def attempted: Boolean = submitted.now()

  /** True once a submit was attempted (and, for a [[scoped]] view, while that attempt counts). */
  val submittedSignal: Signal[Boolean] = submitted.signal.combineWith(submitCounts).map(_ && _).distinct

  /** The `raw` error of field `id`, shown only once the field was touched or a submit was attempted. */
  def visible(id: String, raw: Signal[Option[String]]): Signal[Option[String]] =
    raw.combineWith(touched.signal, submittedSignal).map((error, visited, attempted) => error.filter(_ => attempted || visited.contains(id)))

  private def tracking(id: String): Modifier[HtmlElement] = FormTouch.onFocusOut --> (_ => touch(id))

  /** An optional field whose error shows per [[visible]]. */
  def spec(labelText: String, id: String, raw: Signal[Option[String]]): FieldSpec =
    FieldSpec(labelText, id, error = visible(id, raw), wrapperMods = tracking(id))

  /** A field, `required` or not, whose error shows per [[visible]]. */
  def spec(labelText: String, id: String, required: Boolean, raw: Signal[Option[String]]): FieldSpec =
    FieldSpec(labelText, id, required = required, error = visible(id, raw), wrapperMods = tracking(id))

  /** A field with a hint, `required` or not, whose error shows per [[visible]]. */
  def spec(labelText: String, id: String, required: Boolean, hintText: String, raw: Signal[Option[String]]): FieldSpec =
    FieldSpec(labelText, id, required, Some(hintText), visible(id, raw), tracking(id))
end FormTouch

object FormTouch:
  /** `focusout` (bubbling, unlike `blur`), which Laminar does not predefine. */
  private val onFocusOut: EventProp[dom.FocusEvent] = eventProp("focusout")

/** Moves the keyboard focus to a form control, e.g. the first invalid field after a submit attempt. */
object FormFocus:

  /** Height kept clear above the target: the sticky header (plus some air). */
  private val topOffset = 96

  /** Height kept clear below the target: the fixed mobile summary bar (plus some air). */
  private val bottomOffset = 128

  private val nativelyFocusable = "input, select, textarea, button, a[href], [tabindex]"

  /**
   * On the next animation frame (so a card just expanded is laid out), focuses the element with DOM id `id` and scrolls it into a comfortable
   * position, clear of the sticky header and of the mobile bar. A non-focusable element (a section, a radiogroup) gets `tabindex=-1` first. Nothing
   * happens when no such element exists.
   */
  def focus(id: String): Unit =
    val _ = dom.window.requestAnimationFrame { _ =>
      dom.document.getElementById(id) match
        case element: dom.HTMLElement =>
          if !element.matches(nativelyFocusable) then element.setAttribute("tabindex", "-1")
          val rect = element.getBoundingClientRect()
          val viewport = dom.window.innerHeight
          if rect.top < topOffset || rect.bottom > viewport - bottomOffset then dom.window.scrollBy(0, (rect.top - viewport / 3).toInt)
          element.focus(new dom.FocusOptions:
            preventScroll = true)
        case _ => ()
    }
end FormFocus
