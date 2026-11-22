package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
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

  /** The save-failure banner at the end of a form page, with the DOM id `id` that [[FormFocus.focus]] moves to. */
  def submitError(error: Signal[Option[ApiError]], id: String): Modifier[HtmlElement] =
    child.maybe <-- error.map(_.map(err => div(idAttr := id, errorBanner(err))))

  /** A dismissible page-level error over a list (e.g. a refused delete), with the DOM id `id`; nothing while `error` is empty. */
  def listError(error: Var[Option[ApiError]], id: String): Modifier[HtmlElement] =
    child.maybe <-- error.signal.map(_.map(err => div(idAttr := id, cls := "mb-4", errorBanner(err, () => error.set(None)))))
end RegistryPage
