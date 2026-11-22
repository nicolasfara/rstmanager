package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/**
 * The list ⇄ form-page switch of a page or tab: `None` shows the list, `Some(target)` the form page of `target` (e.g. an `enum XTarget` with `New`
 * and `Edit(record)`). Opening saves a return point (`window.scrollY` and the focused element); closing restores both on the next frame, the focus
 * only if that element is still connected and visible. The list is hidden, not unmounted, so its search, filters, page, sort and loaded data survive.
 *
 * Usage: `div(div(view.listMods, list…), view.page(target => XFormView(target, onExit = () => view.close())))`. Create one per page or tab (a field
 * of its `apply`), so it lives as long as the list does.
 */
final class SubView[A](using CanEqual[A, A]):

  /**
   * The open target; `None` while the list shows. Prefer [[open]] / [[replace]] / [[close]] over setting it directly (they keep the return point).
   */
  val current: Var[Option[A]] = Var(Option.empty[A])

  /** True while a form page is shown in place of the list. */
  val isOpen: Signal[Boolean] = current.signal.map(_.isDefined)

  private var returnPoint = Option.empty[(Double, Option[dom.HTMLElement])]

  /** Shows the page of `target`, saving the return point first. While a page is already open it keeps that page's return point, like [[replace]]. */
  def open(target: A): Unit =
    if current.now().isEmpty then
      val focused = dom.document.activeElement match
        case element: dom.HTMLElement => Some(element)
        case _ => None
      returnPoint = Some((dom.window.scrollY, focused))
    current.set(Some(target))

  /** Swaps the open page for the page of `target` (e.g. `Create` → `Edit(saved)`), keeping the original return point. Opens it when none is open. */
  def replace(target: A): Unit = open(target)

  /** Back to the list: on the next frame, scrolls back where the list was and focuses the control that opened the page if still visible. */
  def close(): Unit =
    val saved = returnPoint
    returnPoint = None
    current.set(None)
    saved.foreach { (scrollY, focused) =>
      val _ = dom.window.requestAnimationFrame { _ =>
        dom.window.scrollTo(0, scrollY.toInt)
        focused
          .filter(element => dom.document.body.contains(element) && element.getClientRects().length > 0)
          .foreach(_.focus(new dom.FocusOptions:
            preventScroll = true))
      }
    }

  /** [[close]], but only while the page of `target` is still the open one (e.g. a save completing after the user moved to another page). */
  def closeIfShowing(target: A): Unit = if current.now() == Some(target) then close()

  /** On the list wrapper: `hidden` while a page is open. */
  val listMods: Modifier[HtmlElement] = cls <-- isOpen.map(open => if open then "hidden" else "")

  /** The page of the open target, mounted fresh for each new target (an equal target does not remount it); nothing while the list shows. */
  def page(render: A => HtmlElement): Modifier[HtmlElement] =
    child.maybe <-- current.signal.distinctByFn((previous, next) => previous == next).map(_.map(render))
end SubView
