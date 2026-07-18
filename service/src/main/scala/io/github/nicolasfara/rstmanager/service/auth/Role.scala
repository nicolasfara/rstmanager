package io.github.nicolasfara.rstmanager.service.auth

/** Application roles, ordered by privilege: every role satisfies the ones below it. */
enum Role derives CanEqual:
  case Viewer, Operator, Admin

  /** Hierarchy check: an `Admin` token satisfies an `Operator` or `Viewer` requirement. */
  def satisfies(required: Role): Boolean = ordinal >= required.ordinal

  def name: String = this match
    case Viewer => "viewer"
    case Operator => "operator"
    case Admin => "admin"

object Role:
  def fromString(raw: String): Option[Role] = raw match
    case "viewer" => Some(Viewer)
    case "operator" => Some(Operator)
    case "admin" => Some(Admin)
    case _ => None
end Role
