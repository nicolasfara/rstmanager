package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.DialogTone
import org.scalajs.dom

/**
 * Protects unsaved drafts from in-app navigation and from closing/reloading the tab. A page with a draft mounts [[guard]]; the shell routes every
 * page change through [[request]], which asks for confirmation (the [[dialog]], rendered once in the shell) while a mounted guard reports a dirty
 * draft, and otherwise proceeds at once.
 */
object NavigationGuard:

  private final case class Entry(id: Int, title: String, message: String, isDirty: () => Boolean)

  private var nextId = 0
  private var entries = List.empty[Entry]

  /** The confirmation being asked: the dirty guard and the navigation waiting for it. */
  private val pending = Var(Option.empty[(Entry, () => Unit)])

  /**
   * Registers the guard while the element it is applied to is mounted. `isDirty` is read at navigation time; while it is true, leaving the page asks
   * for confirmation with `title`/`message`, and closing or reloading the tab triggers the browser's own prompt (`beforeunload`).
   */
  def guard(title: String, message: String, isDirty: () => Boolean): Modifier[HtmlElement] =
    val beforeUnload: js.Function1[dom.BeforeUnloadEvent, Unit] = event =>
      if isDirty() then
        event.preventDefault()
        event.returnValue = ""
    onMountUnmountCallbackWithState[HtmlElement, Int](
      _ =>
        nextId += 1
        val id = nextId
        entries = entries :+ Entry(id, title, message, isDirty)
        dom.window.addEventListener("beforeunload", beforeUnload)
        id
      ,
      (_, registered) =>
        registered.foreach(id => entries = entries.filterNot(_.id == id))
        dom.window.removeEventListener("beforeunload", beforeUnload),
    )
  end guard

  /** Runs `proceed` (a navigation) at once, or after confirmation when a mounted guard has a dirty draft. */
  def request(proceed: () => Unit): Unit =
    entries.find(_.isDirty()) match
      case Some(entry) => pending.set(Some((entry, proceed)))
      case None => proceed()

  /** The "leave the page?" confirmation; render it once, in the app shell. */
  def dialog: HtmlElement =
    Components.confirmDialog(
      pending.signal.map(_.isDefined),
      pending.signal.map(_.fold("")(_._1.title)),
      pending.signal.map(_.fold("")(_._1.message)),
      "Abbandona",
      DialogTone.Danger,
      () => pending.set(None),
      () =>
        val waiting = pending.now()
        pending.set(None)
        waiting.foreach((_, proceed) => proceed()),
    )
end NavigationGuard
