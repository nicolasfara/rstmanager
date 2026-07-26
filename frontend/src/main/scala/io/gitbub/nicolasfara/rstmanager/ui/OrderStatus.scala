package io.gitbub.nicolasfara.rstmanager.ui

/** Shared vocabulary for order and manufacturing lifecycle statuses: human-readable labels and the editability rule. */
object OrderStatus:

  /** Only in-progress and suspended orders accept data/task edits (see the domain `Order` aggregate). */
  def isEditable(status: String): Boolean = status == "in_progress" || status == "suspended"

  /** Maps a backend status value to its Italian label; unknown values pass through unchanged. */
  def label(status: String): String = status match
    case "pending" => "In attesa"
    case "in_progress" => "In corso"
    case "suspended" => "Sospeso"
    case "completed" => "Completato"
    case "delivered" => "Consegnato"
    case "cancelled" => "Annullato"
    case "not_started" => "Non iniziata"
    case "paused" => "In pausa"
    case other => other
end OrderStatus
