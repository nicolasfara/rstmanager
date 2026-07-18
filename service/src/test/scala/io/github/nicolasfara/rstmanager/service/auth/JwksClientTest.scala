package io.github.nicolasfara.rstmanager.service.auth

import java.security.KeyPairGenerator
import java.security.interfaces.RSAPublicKey
import java.util.Base64

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

final class JwksClientTest extends AnyFlatSpecLike with Matchers with EitherValues:

  private def rsaJwkEntry(kid: String): (String, RSAPublicKey) =
    val generator = KeyPairGenerator.getInstance("RSA").nn
    generator.initialize(2048)
    val publicKey = generator.generateKeyPair().nn.getPublic.asInstanceOf[RSAPublicKey]
    val encoder = Base64.getUrlEncoder.nn.withoutPadding.nn
    val n = encoder.encodeToString(publicKey.getModulus.nn.toByteArray).nn
    val e = encoder.encodeToString(publicKey.getPublicExponent.nn.toByteArray).nn
    val json = s"""{"kty":"RSA","use":"sig","alg":"RS256","kid":"$kid","n":"$n","e":"$e"}"""
    (json, publicKey)

  "JwksClient.parseJwks" should "extract RSA signing keys by kid" in {
    val (entry1, key1) = rsaJwkEntry("kid-1")
    val (entry2, key2) = rsaJwkEntry("kid-2")
    val doc = io.circe.parser.parse(s"""{"keys":[$entry1,$entry2]}""").value

    val keys = JwksClient.parseJwks(doc).value
    keys.keySet shouldBe Set("kid-1", "kid-2")
    keys("kid-1").getModulus.nn shouldBe key1.getModulus.nn
    keys("kid-2").getModulus.nn shouldBe key2.getModulus.nn
  }

  it should "skip non-RSA, encryption-use, and malformed entries" in {
    val (entry, _) = rsaJwkEntry("good")
    val doc = io.circe.parser.parse(
      s"""{"keys":[
            $entry,
            {"kty":"EC","use":"sig","kid":"ec-key","crv":"P-256","x":"AA","y":"AA"},
            {"kty":"RSA","use":"enc","kid":"enc-key","n":"AQAB","e":"AQAB"},
            {"kty":"RSA","use":"sig","kid":"broken","n":"%%not-base64%%","e":"AQAB"},
            {"kty":"RSA","use":"sig","n":"AQAB","e":"AQAB"}
          ]}""",
    ).value

    JwksClient.parseJwks(doc).value.keySet shouldBe Set("good")
  }

  it should "fail on a document without a keys array" in {
    JwksClient.parseJwks(io.circe.parser.parse("""{"foo":1}""").value).isLeft shouldBe true
  }
end JwksClientTest
