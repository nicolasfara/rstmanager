package io.github.nicolasfara.rstmanager.service.auth

import java.security.interfaces.RSAPublicKey
import java.util.Base64

import io.github.nicolasfara.rstmanager.service.http.ApiError.ApiFailure
import io.github.nicolasfara.rstmanager.service.http.Secured

import cats.data.EitherT
import cats.effect.IO
import pdi.jwt.{ JwtAlgorithm, JwtCirce, JwtClaim }

/** Expected token issuer (external Keycloak realm URL) and the client id the token must have been issued to (`azp`). */
final case class AuthConfig(issuer: String, clientId: String)

/**
 * Validates Keycloak access tokens: RS256 signature via JWKS lookup, `exp`/`nbf`, `iss`, `azp`, then extracts identity and realm roles.
 *
 * Decoupled from the HTTP layer through the `keyFor` lookup so tests can inject generated keys.
 */
final class JwtValidator(keyFor: String => IO[Option[RSAPublicKey]], config: AuthConfig):
  import JwtValidator.*

  def authorize(token: String, required: Role): IO[Either[ApiFailure, AuthedUser]] =
    (for
      kid <- EitherT.fromEither[IO](extractKid(token).left.map(Secured.unauthorized))
      key <- EitherT.fromOptionF(keyFor(kid), Secured.unauthorized("The token is signed with an unknown key."))
      claim <- EitherT.fromEither[IO](decodeVerified(token, key))
      _ <- EitherT.cond[IO](claim.issuer.contains(config.issuer), (), Secured.unauthorized("The token was issued by an unexpected issuer."))
      user <- EitherT.fromEither[IO](parseClaims(claim, config).left.map(Secured.unauthorized))
      _ <- EitherT.cond[IO](user.hasRole(required), (), Secured.forbidden(required))
    yield user).value

  private def decodeVerified(token: String, key: RSAPublicKey): Either[ApiFailure, JwtClaim] =
    JwtCirce
      .decode(token, key, Seq(JwtAlgorithm.RS256))
      .toEither
      .left
      .map(_ => Secured.unauthorized("The token signature or validity period is invalid."))
end JwtValidator

object JwtValidator:
  /** Reads the `kid` from the (unverified) JWT header segment; verification happens against the resolved key. */
  private[auth] def extractKid(token: String): Either[String, String] =
    token.split('.') match
      case Array(header, _, _) =>
        for
          decoded <- decodeBase64Url(header).toRight("The token header is not valid base64url.")
          json <- io.circe.parser.parse(decoded).left.map(_ => "The token header is not valid JSON.")
          kid <- json.hcursor.get[String]("kid").left.map(_ => "The token header carries no key id.")
        yield kid
      case _ => Left("The token is not a well-formed JWT.")

  /** Extracts identity and roles from the verified claim; unknown role strings are ignored. */
  private[auth] def parseClaims(claim: JwtClaim, config: AuthConfig): Either[String, AuthedUser] =
    for
      content <- io.circe.parser.parse(claim.content).left.map(_ => "The token payload is not valid JSON.")
      cursor = content.hcursor
      azp <- cursor.get[String]("azp").left.map(_ => "The token carries no azp claim.")
      _ <- Either.cond(azp == config.clientId, (), "The token was issued to an unexpected client.")
      subject <- claim.subject.toRight("The token carries no subject.")
      roles = cursor.downField("realm_access").get[List[String]]("roles").getOrElse(Nil).flatMap(Role.fromString).toSet
      username = cursor.get[String]("preferred_username").getOrElse(subject)
    yield AuthedUser(subject, username, roles)

  private def decodeBase64Url(segment: String): Option[String] =
    scala.util.Try(JwksClient.utf8(Base64.getUrlDecoder.nn.decode(segment).nn)).toOption
end JwtValidator
