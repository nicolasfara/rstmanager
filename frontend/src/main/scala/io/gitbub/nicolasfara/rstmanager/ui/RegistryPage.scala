package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.ApiError
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/**
 * Pieces shared by the registry pages (customers, catalog tasks, catalog manufacturings): a full-width list with a "+ Nuovo X" entry point, and a
 * create/edit form page shown in its place through a [[SubView]].
 */
private[ui] object RegistryPage:

  /** The list header: the page title and, for Admins, the "+ Nuovo X" button (at every width). */
  def listHeader(titleText: String, newLabel: String, onNew: () => Unit): HtmlElement =
    div(
      cls := "mb-4 flex flex-wrap items-center justify-between gap-2",
      sectionTitle(titleText),
      roleGated(Role.Admin)(button(tpe := "button", cls := s"$btnPrimary min-h-10 md:min-h-0", newLabel, onClick --> (_ => onNew()))),
    )

  /** «Scartare le modifiche a «`name`»?» while editing a record, «Abbandonare `newDraft`?» (e.g. "il nuovo cliente") for a new one. */
  def discardTitle(editingName: Option[String], newDraft: String): String =
    editingName.fold(s"Abbandonare $newDraft?")(name => DraftGuard.discardTitle(Some(name), newDraft))

  /**
   * Whether the record `editingId` is missing from a list load: `Some(true)` when a successful load no longer contains it, `Some(false)` when it
   * does, `None` when the load says nothing (still loading, failed, or no record being edited).
   */
  def missingFrom[A](loaded: Option[ApiClient.Result[List[A]]], editingId: Option[UUID], idOf: A => UUID): Option[Boolean] =
    (loaded, editingId) match
      case (Some(Right(list)), Some(id)) => Some(!list.exists(record => idOf(record) == id))
      case _ => None

  /**
   * Tracks, while the form page is mounted, whether its record vanished from the list (e.g. deleted by another user): sets `gone` from each
   * successful reload of `data` and runs `onGone` once when it first becomes true.
   */
  def trackGone[A](data: Signal[Option[ApiClient.Result[List[A]]]], editingId: Option[UUID], idOf: A => UUID, gone: Var[Boolean])(
      onGone: () => Unit,
  ): Modifier[HtmlElement] =
    data --> { loaded =>
      missingFrom(loaded, editingId, idOf).foreach { missing =>
        val wasGone = gone.now()
        gone.set(missing)
        if missing && !wasGone then onGone()
      }
    }

  /** The save-failure banner at the end of a form page, with the DOM id `id` that [[FormFocus.focus]] moves to. */
  def submitError(error: Signal[Option[ApiError]], id: String): Modifier[HtmlElement] =
    child.maybe <-- error.map(_.map(err => div(idAttr := id, errorBanner(err))))

  /** A dismissible page-level error over a list (e.g. a refused delete), with the DOM id `id`; nothing while `error` is empty. */
  def listError(error: Var[Option[ApiError]], id: String): Modifier[HtmlElement] =
    child.maybe <-- error.signal.map(_.map(err => div(idAttr := id, cls := "mb-4", errorBanner(err, () => error.set(None)))))
end RegistryPage
