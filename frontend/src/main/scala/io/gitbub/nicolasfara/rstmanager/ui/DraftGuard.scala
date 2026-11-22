package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.ui.Components.{ confirmDialog, DialogTone }

/**
 * Protects the draft of a form page. The draft is dirty while `current()` differs from the content last marked clean ([[markClean]], after loading or
 * resetting the form). While dirty, leaving it ("← Torna a…", "Annulla", the browser back button) asks first through [[dialog]] (see
 * [[discardThen]]), and leaving the page asks through [[NavigationGuard]] (mount [[bind]] on the page).
 *
 * `title` names the record as last marked clean (not the edited text), e.g. «Scartare le modifiche a «Mario Rossi»?» (see
 * [[DraftGuard.discardTitle]]).
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
    if isDirtyNow then pending.set(Some((title(baseline), replace))) else replace()

  /** Asks before leaving the page (top navigation, tab switch, closing the tab) while the draft is dirty. */
  val bind: Modifier[HtmlElement] =
    NavigationGuard.guard("Abbandonare le modifiche?", "Le modifiche non salvate nel modulo andranno perse.", () => isDirtyNow)

  /**
   * Variant of [[bind]] asking with `title` and `message` (e.g. «Abbandonare il nuovo DDT?»), and not at all while `suppressWhen()` (e.g. once the
   * record is gone, see [[DraftGuard.trackGone]]: the draft cannot be saved anyway).
   */
  def bind(title: String, message: String, suppressWhen: () => Boolean): Modifier[HtmlElement] =
    NavigationGuard.guard(title, message, () => isDirtyNow && !suppressWhen())

  /** The "discard the draft?" confirmation; render it once, next to the form. */
  val dialog: HtmlElement =
    confirmDialog(
      pending.signal.map(_.isDefined),
      pending.signal.map(_.fold("")(_._1)),
      Signal.fromValue("Le modifiche non salvate andranno perse."),
      "Abbandona",
      "Continua a modificare",
      DialogTone.Danger,
      () => pending.set(None),
      () =>
        val waiting = pending.now()
        pending.set(None)
        waiting.foreach((_, replace) => replace()),
    )
end DraftGuard

object DraftGuard:

  /** «Scartare le modifiche a «`name`»?» for a record being edited, «Abbandonare `newDraft`?» (e.g. "il nuovo cliente") for a new one. */
  def discardTitle(editingName: Option[String], newDraft: String): String =
    editingName.fold(s"Abbandonare $newDraft?")(name => s"Scartare le modifiche a «$name»?")

  /**
   * Whether the record `editingId` is missing from a list load: `Some(true)` when a successful load no longer contains it, `Some(false)` when it
   * does, `None` when the load says nothing (still loading, failed, or no record being edited).
   */
  def missingFrom[A](loaded: Option[ApiClient.Result[List[A]]], editingId: Option[UUID], idOf: A => UUID): Option[Boolean] =
    (loaded, editingId) match
      case (Some(Right(list)), Some(id)) => Some(!list.exists(record => idOf(record) == id))
      case _ => None

  /**
   * The "record deleted elsewhere" flag of an edit page, while the page is mounted: `gone` turns true the first time `missing` does (e.g. a reload no
   * longer lists the record) and `onGone` runs then (e.g. `draft.markClean()`). It never turns back, so a later failed reload does not make the draft
   * savable again. The page pairs it with a `warningNote` in its header, a `blocked` reason on its submit, and a leave that does not ask.
   */
  def trackGone(missing: Signal[Boolean], gone: Var[Boolean])(onGone: () => Unit): Modifier[HtmlElement] =
    missing --> { isMissing =>
      if isMissing && !gone.now() then
        gone.set(true)
        onGone()
    }

  /** [[trackGone]] over a list load: the record `editingId` (none for a new record) is missing once a successful load of `data` lacks it. */
  def trackGone[A](data: Signal[Option[ApiClient.Result[List[A]]]], editingId: Option[UUID], idOf: A => UUID, gone: Var[Boolean])(
      onGone: () => Unit,
  ): Modifier[HtmlElement] =
    trackGone(data.map(missingFrom(_, editingId, idOf).getOrElse(false)), gone)(onGone)
end DraftGuard
