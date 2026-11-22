package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/**
 * One browser-history entry per mounted form page, so the browser or Android back button leaves the page through its guarded leave, like "← Torna".
 * On mount: `history.pushState` (same URL). On `popstate` while mounted: push the entry again (the page is still shown), then run `onBack`; if it
 * leaves, the unmount removes the entry, while a "Continua a modificare" keeps it (so the next back asks again). On unmount (any cause: saved,
 * cancelled, top navigation): `history.back()`, with the resulting `popstate` ignored.
 *
 * The `back()` is deferred to a macrotask after the unmount, so a page replaced by another in the same update ([[SubView.replace]], e.g. Create →
 * Edit) hands its entry over instead of popping and pushing again. Bound by [[Components.formPage]]; pages do not bind it themselves.
 */
object SubViewHistory:

  import Machine.{ Action, Event }

  private var state = Machine.initial
  private var nextId = 0

  /** The `onBack` of the mounted pages, oldest first; the browser back runs the newest one. */
  private var owners = List.empty[(Int, () => Unit)]
  private var listening = false

  /** Marker stored in the pushed entry (the state object must not be null). */
  private val entryState = js.Dynamic.literal(rstmanagerSubView = true)

  private def dispatch(event: Event): Unit =
    val (next, actions) = Machine.step(state, event)
    state = next
    actions.foreach {
      case Action.Push => dom.window.history.pushState(entryState, "")
      case Action.Back => dom.window.history.back()
      case Action.RunBack => owners.lastOption.foreach((_, onBack) => onBack())
      case Action.Ignore => ()
    }

  private def ensureListening(): Unit =
    if !listening then
      listening = true
      dom.window.addEventListener("popstate", (_: dom.PopStateEvent) => dispatch(Event.Popped))

  /** Gives the element a history entry while it is mounted, and runs `onBack` (the guarded leave) when the browser back button pops it. */
  def bind(onBack: () => Unit): Modifier[HtmlElement] =
    onMountUnmountCallbackWithState[HtmlElement, Int](
      _ =>
        ensureListening()
        nextId += 1
        val id = nextId
        owners = owners :+ (id -> onBack)
        dispatch(Event.Mounted)
        id
      ,
      (_, registered) =>
        registered.foreach(id => owners = owners.filterNot(_._1 == id))
        dispatch(Event.Unmounted)
        val _ = setTimeout(0)(dispatch(Event.Settled)),
    )

  /**
   * The pure transition table behind [[bind]]: the history events seen by the mounted pages and the browser actions they cause. The state tracks how
   * many pages are mounted, whether our entry is on top of the history, and how many `popstate`s are our own `back()`s still to come.
   */
  private[ui] object Machine:

    final case class State(mounted: Int, entry: Boolean, ignorePops: Int) derives CanEqual

    val initial: State = State(0, false, 0)

    enum Event derives CanEqual:
      /** A form page was mounted. */
      case Mounted

      /** A form page was unmounted. */
      case Unmounted

      /** The macrotask after an unmount: the moment to drop our entry if no page took it over. */
      case Settled

      /** The browser fired `popstate` (back/forward button, or our own `back()`). */
      case Popped
    end Event

    enum Action derives CanEqual:
      /** `history.pushState`: add our entry. */
      case Push

      /** Run the newest page's `onBack` (its guarded leave). */
      case RunBack

      /** `history.back()`: drop our entry; its `popstate` is ignored. */
      case Back

      /** A `popstate` caused by our own `back()`: nothing to do. */
      case Ignore
    end Action

    def step(state: State, event: Event): (State, List[Action]) = event match
      case Event.Mounted =>
        // A page that takes over the entry of one unmounted in the same update (or nested in a mounted one) pushes nothing.
        if state.entry then (state.copy(mounted = state.mounted + 1), Nil)
        else (state.copy(mounted = state.mounted + 1, entry = true), List(Action.Push))
      case Event.Unmounted => (state.copy(mounted = (state.mounted - 1).max(0)), Nil)
      case Event.Settled =>
        if state.mounted == 0 && state.entry then (state.copy(entry = false, ignorePops = state.ignorePops + 1), List(Action.Back))
        else (state, Nil)
      case Event.Popped =>
        if state.ignorePops > 0 then (state.copy(ignorePops = state.ignorePops - 1), List(Action.Ignore))
        // The browser left our entry: while a page is shown, put the entry back and let the page decide whether to leave.
        else if state.mounted > 0 then (state.copy(entry = true), List(Action.Push, Action.RunBack))
        // An external pop after the unmount (before the deferred back): the entry is already gone, so no back() follows.
        else (state.copy(entry = false), Nil)
  end Machine
end SubViewHistory
