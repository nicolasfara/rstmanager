package io.gitbub.nicolasfara.rstmanager.auth

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** Scala.js facade over the official `keycloak-js` OIDC adapter (Authorization Code + PKCE). */
@js.native
@JSImport("keycloak-js", JSImport.Default)
class Keycloak(@annotation.unused config: KeycloakConfig) extends js.Object:
  def init(options: KeycloakInitOptions): js.Promise[Boolean] = js.native
  def login(options: KeycloakRedirectOptions): js.Promise[Unit] = js.native
  def logout(options: KeycloakRedirectOptions): js.Promise[Unit] = js.native
  /** Refreshes the token if it expires within `minValidity` seconds; rejects when the session is gone. */
  def updateToken(minValidity: Int): js.Promise[Boolean] = js.native
  def token: js.UndefOr[String] = js.native
  def tokenParsed: js.UndefOr[js.Dynamic] = js.native
  var onTokenExpired: js.Function0[Unit] = js.native
  var onAuthLogout: js.Function0[Unit] = js.native
end Keycloak

trait KeycloakConfig extends js.Object:
  val url: String
  val realm: String
  val clientId: String

trait KeycloakInitOptions extends js.Object:
  val onLoad: String
  val pkceMethod: String
  val silentCheckSsoRedirectUri: String
  val checkLoginIframe: Boolean

trait KeycloakRedirectOptions extends js.Object:
  val redirectUri: String
