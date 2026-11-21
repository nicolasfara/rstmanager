package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.{ confirmDialog, DialogTone }

/**
 * Protects the draft of a side form (the create/edit card next to a registry list). The draft is dirty while `current()` differs from the content
 * last marked clean ([[markClean]], after loading or resetting the form). While dirty, replacing it ("Modifica" on another row, "Annulla", "+ Nuovo")
 * asks first through [[dialog]], and leaving the page asks through [[NavigationGuard]] (mount [[bind]] on the form card).
 *
 * `title` names what would be discarded, e.g. «Scartare le modifiche a «Mario Rossi»?» (see [[DraftGuard.discardTitle]]).
 */
final class DraftGuard[A](current: () => A, title: A => String)(using CanEqual[A, A]):

  private var baseline: A = current()
  private val pending = Var(Option.empty[(String, () => Unit)])

  /** True while the draft differs from the content last marked clean. */
  def isDirtyNow: Boolean = current() != baseline

  /** The current content becomes the clean one: call right after loading or resetting the form. */
  def markClean(): Unit = baseline = current()

  /** Applies `patch` to the clean content too, for a programmatic change (e.g. a regenerated code) that must not make the draft dirty. */
  def rebase(patch: A => A): Unit = baseline = patch(baseline)

  /** Runs `replace` at once on a clean draft, otherwise once the user confirms discarding it. */
  def discardThen(replace: () => Unit): Unit =
    if isDirtyNow then pending.set(Some((title(current()), replace))) else replace()

  /** Asks before leaving the page (top navigation, tab switch, closing the tab) while the draft is dirty. */
  val bind: Modifier[HtmlElement] =
    NavigationGuard.guard("Abbandonare le modifiche?", "Le modifiche non salvate nel modulo andranno perse.", () => isDirtyNow)

  /** The "discard the draft?" confirmation; render it once, next to the form. */
  val dialog: HtmlElement =
    confirmDialog(
      pending.signal.map(_.isDefined),
      pending.signal.map(_.fold("")(_._1)),
      Signal.fromValue("Le modifiche non salvate andranno perse."),
      "Abbandona",
      DialogTone.Danger,
      () => pending.set(None),
      () =>
        val waiting = pending.now()
        pending.set(None)
        waiting.foreach((_, replace) => replace()),
    )
end DraftGuard

object DraftGuard:

  /** «Scartare le modifiche a «`name`»?» for a record being edited, «Scartare `newDraft`?» (e.g. "il nuovo cliente") for a new one. */
  def discardTitle(editingName: Option[String], newDraft: String): String =
    editingName.fold(s"Scartare $newDraft?")(name => s"Scartare le modifiche a «$name»?")
