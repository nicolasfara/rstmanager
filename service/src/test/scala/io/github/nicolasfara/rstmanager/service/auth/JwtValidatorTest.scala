package io.github.nicolasfara.rstmanager.service.auth

import java.security.{ KeyPair, KeyPairGenerator }
import java.security.interfaces.RSAPublicKey
import java.time.Instant

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import pdi.jwt.{ JwtAlgorithm, JwtCirce, JwtClaim, JwtHeader }
import sttp.model.StatusCode

final class JwtValidatorTest extends AnyFlatSpecLike with Matchers with EitherValues:
  private given CanEqual[StatusCode, StatusCode] = CanEqual.derived

  private val issuer = "http://localhost:3333/auth/realms/rstmanager"
  private val clientId = "rstmanager-frontend"
  private val kid = "test-kid"

  private val keyPair: KeyPair = generateKeyPair()
  private val otherKeyPair: KeyPair = generateKeyPair()

  private val validator = new JwtValidator(
    key => IO.pure(Option.when(key == kid)(keyPair.getPublic).collect { case rsa: RSAPublicKey => rsa }),
    AuthConfig(issuer, clientId),
  )

  private def generateKeyPair(): KeyPair =
    val generator = KeyPairGenerator.getInstance("RSA").nn
    generator.initialize(2048)
    generator.generateKeyPair().nn

  private final case class TokenSpec(
      roles: List[String] = List("viewer"),
      issuer: String = issuer,
      azp: String = clientId,
      kid: String = kid,
      keyPair: KeyPair = keyPair,
      expiresInSeconds: Long = 300,
      username: Option[String] = Some("alice"),
  )

  private def token(spec: TokenSpec): String =
    val now = Instant.now.nn.getEpochSecond
    val usernameField = spec.username.fold("")(u => s""""preferred_username":"$u",""")
    val claim = JwtClaim(
      content = s"""{$usernameField"azp":"${spec.azp}","realm_access":{"roles":[${spec.roles.map(r => s"\"$r\"").mkString(",")}]}}""",
      issuer = Some(spec.issuer),
      subject = Some("subject-1"),
      audience = None,
      expiration = Some(now + spec.expiresInSeconds),
      notBefore = None,
      issuedAt = Some(now),
      jwtId = None,
    )
    JwtCirce.encode(JwtHeader(Some(JwtAlgorithm.RS256), Some("JWT"), None, Some(spec.kid)), claim, spec.keyPair.getPrivate.nn)
  end token

  private def authorize(token: String, required: Role) = validator.authorize(token, required).unsafeRunSync()

  "JwtValidator" should "authorize a valid token with a sufficient role" in {
    val user = authorize(token(TokenSpec(roles = List("admin", "operator", "viewer"))), Role.Admin).value
    user.subject shouldBe "subject-1"
    user.username shouldBe "alice"
    user.roles shouldBe Set(Role.Admin, Role.Operator, Role.Viewer)
  }

  it should "let a higher role satisfy a lower requirement (hierarchy)" in {
    authorize(token(TokenSpec(roles = List("admin"))), Role.Viewer).isRight shouldBe true
    authorize(token(TokenSpec(roles = List("operator"))), Role.Viewer).isRight shouldBe true
  }

  it should "reject an insufficient role with 403" in {
    authorize(token(TokenSpec(roles = List("viewer"))), Role.Admin).left.value._1 shouldBe StatusCode.Forbidden
    authorize(token(TokenSpec(roles = List("operator"))), Role.Admin).left.value._1 shouldBe StatusCode.Forbidden
    authorize(token(TokenSpec(roles = Nil)), Role.Viewer).left.value._1 shouldBe StatusCode.Forbidden
  }

  it should "reject an expired token with 401" in {
    authorize(token(TokenSpec(expiresInSeconds = -60)), Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
  }

  it should "reject a wrong issuer with 401" in {
    authorize(token(TokenSpec(issuer = "http://evil.example/realms/rstmanager")), Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
  }

  it should "reject a token issued to another client (azp) with 401" in {
    authorize(token(TokenSpec(azp = "other-client")), Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
  }

  it should "reject an unknown key id with 401" in {
    authorize(token(TokenSpec(kid = "rotated-away")), Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
  }

  it should "reject a token signed with another key with 401" in {
    authorize(token(TokenSpec(keyPair = otherKeyPair)), Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
  }

  it should "reject garbage tokens with 401" in {
    authorize("not-a-jwt", Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
    authorize("", Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
    authorize("aaa.bbb.ccc", Role.Viewer).left.value._1 shouldBe StatusCode.Unauthorized
  }

  it should "fall back to the subject when preferred_username is missing" in {
    authorize(token(TokenSpec(username = None)), Role.Viewer).value.username shouldBe "subject-1"
  }

  it should "ignore unknown role strings" in {
    authorize(token(TokenSpec(roles = List("uma_authorization", "viewer"))), Role.Viewer).value.roles shouldBe Set(Role.Viewer)
  }
end JwtValidatorTest
