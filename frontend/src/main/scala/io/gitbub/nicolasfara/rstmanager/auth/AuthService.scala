package io.gitbub.nicolasfara.rstmanager.auth

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*

import com.raquo.laminar.api.L.{ Signal, Var }
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import org.scalajs.dom

/** Application roles, mirroring the backend hierarchy: every role satisfies the ones below it. */
enum Role derives CanEqual:
  case Viewer, Operator, Admin
  def satisfies(required: Role): Boolean = ordinal >= required.ordinal

object Role:
  def fromString(raw: String): Option[Role] = raw match
    case "viewer" => Some(Viewer)
    case "operator" => Some(Operator)
    case "admin" => Some(Admin)
    case _ => None

final case class AuthUser(username: String, roles: Set[Role]):
  def hasRole(required: Role): Boolean = roles.exists(_.satisfies(required))

enum AuthState derives CanEqual:
  case Initializing
  case Anonymous
  case Authenticated(user: AuthUser)

/**
 * Keycloak session for the SPA: hosted login via redirect (Authorization Code + PKCE), silent SSO on reload, in-memory tokens only.
 *
 * Keycloak is reached same-origin under `/auth` (nginx/vite proxy), so the issuer matches the backend expectation and the session cookie stays
 * first-party.
 */
object AuthService:
  private val realm = "rstmanager"
  private val clientId = "rstmanager-frontend"
  private def origin: String = dom.window.location.origin

  private var keycloak: Option[Keycloak] = None
  private val state: Var[AuthState] = Var(AuthState.Initializing)

  def stateSignal: Signal[AuthState] = state.signal

  def hasRoleSignal(required: Role): Signal[Boolean] = stateSignal.map {
    case AuthState.Authenticated(user) => user.hasRole(required)
    case _ => false
  }

  /** Synchronous role check for render paths built per data refresh; the role set is fixed for the whole session. */
  def currentHasRole(required: Role): Boolean = state.now() match
    case AuthState.Authenticated(user) => user.hasRole(required)
    case _ => false

  def init(): Unit =
    val kc = new Keycloak(new KeycloakConfig:
      val url = s"$origin/auth"; val realm = AuthService.realm; val clientId = AuthService.clientId)
    keycloak = Some(kc)
    kc.onTokenExpired = () => { val _ = kc.updateToken(30).`catch`(_ => forceReauth()) }
    kc.onAuthLogout = () => becomeAnonymous()
    val initialized: Future[Boolean] = kc.init(
      new KeycloakInitOptions:
        val onLoad = "check-sso"
        val pkceMethod = "S256"
        val silentCheckSsoRedirectUri = s"$origin/silent-check-sso.html"
        val checkLoginIframe = false,
    )
    initialized
      .map(authenticated => if authenticated then state.set(AuthState.Authenticated(parseUser(kc))) else becomeAnonymous())
      .recover { case _ => becomeAnonymous() }
    ()
  end init

  def login(): Unit =
    keycloak.foreach(kc =>
      kc.login(new KeycloakRedirectOptions:
        val redirectUri = origin),
    )

  def logout(): Unit =
    ApiClient.clearCache()
    keycloak.foreach(kc =>
      kc.logout(new KeycloakRedirectOptions:
        val redirectUri = origin),
    )

  /** Fresh bearer token for an API call, refreshing it when close to expiry; a dead session forces a new login. */
  def bearerToken(): Future[Option[String]] =
    keycloak match
      case None => Future.successful(None)
      case Some(kc) =>
        kc.updateToken(30)
          .map(_ => kc.token.toOption)
          .recover { case _ =>
            forceReauth()
            None
          }

  /** The session is no longer usable (refresh failed or the API answered 401): drop it and go through the hosted login again. */
  def forceReauth(): Unit =
    becomeAnonymous()
    login()

  private def becomeAnonymous(): Unit =
    ApiClient.clearCache()
    state.set(AuthState.Anonymous)

  private def parseUser(kc: Keycloak): AuthUser =
    val parsed = kc.tokenParsed.toOption
    val username = parsed
      .flatMap(stringClaim(_, "preferred_username"))
      .orElse(parsed.flatMap(stringClaim(_, "sub")))
      .getOrElse("utente")
    val roles = parsed
      .flatMap(rolesClaim)
      .fold(Set.empty[Role])(_.flatMap(Role.fromString).toSet)
    AuthUser(username, roles)

  /** Reads a top-level string claim from the parsed token, tolerating missing or non-string values. */
  private def stringClaim(claims: js.Dynamic, field: String): Option[String] =
    (claims.selectDynamic(field): Any) match
      case value: String => Some(value)
      case _ => None

  /** Extracts `realm_access.roles`, tolerating a missing object or a malformed roles array. */
  private def rolesClaim(claims: js.Dynamic): Option[List[String]] =
    val access = claims.selectDynamic("realm_access")
    Option[Any](access).filterNot(js.isUndefined).flatMap { _ =>
      (access.selectDynamic("roles"): Any) match
        case values: js.Array[?] => Some(values.toList.collect { case role: String => role })
        case _ => None
    }
end AuthService
