package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js.timers

import com.raquo.laminar.api.L.*

/**
 * Success feedback, modelled on [[ErrorCenter]]: the latest notice is exposed to the app shell, which renders it as a toast. A notice dismisses
 * itself after a few seconds, unless a newer one replaced it meanwhile (then the newer one keeps its own timer).
 */
object Notifications:

  final case class Notice(id: Int, message: String)

  /** How long a notice stays on screen (milliseconds). */
  private val autoDismissMillis = 4000.0

  private var nextId = 0
  private val latest = Var(Option.empty[Notice])

  val latestSignal: Signal[Option[Notice]] = latest.signal

  /** Shows `message` (e.g. "Ordine ORD-2026-001 creato") as a success toast. */
  def success(message: String): Unit =
    nextId += 1
    val id = nextId
    latest.set(Some(Notice(id, message)))
    val _ = timers.setTimeout(autoDismissMillis)(dismiss(id))

  /** Hides notice `id`, if it is still the one shown. */
  def dismiss(id: Int): Unit =
    latest.update {
      case Some(notice) if notice.id == id => None
      case current => current
    }
end Notifications
