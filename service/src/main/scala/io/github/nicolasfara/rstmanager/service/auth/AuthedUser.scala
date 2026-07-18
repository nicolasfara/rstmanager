package io.github.nicolasfara.rstmanager.service.auth

/** The caller identity extracted from a validated Keycloak access token. */
final case class AuthedUser(subject: String, username: String, roles: Set[Role]):
  def hasRole(required: Role): Boolean = roles.exists(_.satisfies(required))
