package io.github.nicolasfara.rstmanager.service.auth

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.KeyFactory
import java.security.interfaces.RSAPublicKey
import java.security.spec.RSAPublicKeySpec
import java.util.Base64

import scala.concurrent.duration.{ DurationInt, FiniteDuration }

import cats.effect.{ Clock, IO, Ref }
import cats.syntax.all.*
import io.circe.Json
import org.http4s.Uri
import org.http4s.client.Client

/**
 * Fetches and caches the realm RSA signing keys (JWKS) by `kid`.
 *
 * Unknown kids trigger a re-fetch (covering Keycloak key rotation), rate-limited by `minRefetchInterval` so a flood of requests with bogus kids
 * cannot hammer Keycloak.
 */
final class JwksClient private (client: Client[IO], jwksUri: Uri, state: Ref[IO, JwksClient.State], minRefetchInterval: FiniteDuration):
  import JwksClient.State

  def keyFor(kid: String): IO[Option[RSAPublicKey]] =
    state.get.flatMap { current =>
      current.keys.get(kid) match
        case found @ Some(_) => IO.pure(found)
        case None => refetchIfAllowed.map(_.keys.get(kid))
    }

  /** Unconditional fetch; raises on transport/parse errors so the caller can retry at startup. */
  def refresh: IO[Unit] =
    for
      body <- client.expect[String](jwksUri)
      json <- IO.fromEither(io.circe.parser.parse(body))
      keys <- IO.fromEither(JwksClient.parseJwks(json).left.map(new IllegalStateException(_)))
      now <- Clock[IO].realTime
      _ <- state.set(State(keys, now))
    yield ()

  /** Best-effort fetch keeping the previous keys on failure, skipped inside the rate-limit window. */
  private def refetchIfAllowed: IO[State] =
    for
      now <- Clock[IO].realTime
      current <- state.get
      updated <-
        if now - current.lastFetch < minRefetchInterval then IO.pure(current)
        else refresh.attempt *> state.get
    yield updated
end JwksClient

object JwksClient:
  final case class State(keys: Map[String, RSAPublicKey], lastFetch: FiniteDuration)

  def build(client: Client[IO], jwksUri: Uri, minRefetchInterval: FiniteDuration = 30.seconds): IO[JwksClient] =
    Ref.of[IO, State](State(Map.empty, -minRefetchInterval)).map(new JwksClient(client, jwksUri, _, minRefetchInterval))

  /** Parses a JWKS document into `kid -> RSAPublicKey`, skipping non-RSA/non-signature/malformed entries. */
  def parseJwks(json: Json): Either[String, Map[String, RSAPublicKey]] =
    json.hcursor.downField("keys").as[List[Json]].left.map(_ => "JWKS document has no 'keys' array.").map { entries =>
      entries.flatMap { entry =>
        val cursor = entry.hcursor
        val usableForSignatures =
          cursor.get[String]("kty").toOption.contains("RSA") && cursor.get[String]("use").toOption.forall(_ == "sig")
        if !usableForSignatures then None
        else
          for
            kid <- cursor.get[String]("kid").toOption
            modulus <- cursor.get[String]("n").toOption
            exponent <- cursor.get[String]("e").toOption
            key <- rsaPublicKey(modulus, exponent).toOption
          yield kid -> key
      }.toMap
    }

  private def rsaPublicKey(modulus: String, exponent: String): Either[Throwable, RSAPublicKey] =
    Either.catchNonFatal {
      val decoder = Base64.getUrlDecoder.nn
      val spec = new RSAPublicKeySpec(new BigInteger(1, decoder.decode(modulus).nn), new BigInteger(1, decoder.decode(exponent).nn))
      KeyFactory.getInstance("RSA").nn.generatePublic(spec).nn match
        case key: RSAPublicKey => key
        case other: java.security.PublicKey => throw new IllegalStateException(s"Unexpected key type: ${other.getClass.getName}")
    }

  private[auth] def utf8(bytes: Array[Byte]): String = new String(bytes, StandardCharsets.UTF_8)
end JwksClient
