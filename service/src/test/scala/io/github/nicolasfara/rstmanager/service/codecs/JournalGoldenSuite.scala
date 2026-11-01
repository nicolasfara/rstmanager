package io.github.nicolasfara.rstmanager.service.codecs

import scala.compiletime.constValueTuple
import scala.deriving.Mirror
import scala.io.Source
import scala.util.Using

import io.circe.{ Codec, Json }
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

/**
 * Golden tests of an event journal, on fixtures in `golden/<directory>/<name>.json`:
 *   - every current-version fixture decodes and is written back exactly, so a changed shape fails until it gets a new schema version;
 *   - every legacy fixture (a shape persisted by an earlier release) decodes to the same event as its current-version counterpart;
 *   - every event case has a current-version fixture.
 *
 * Fixtures are never edited or removed: on a version bump the current ones stay as legacy fixtures and new current ones are added.
 */
abstract class JournalGoldenSuite[E](directory: String, schema: EventSchema[E], cases: Set[String])(using codec: Codec[E])
    extends AnyFlatSpecLike:
  /** Fixtures in the shape written by this release. */
  def current: List[String]

  /** Fixtures in the shapes persisted by earlier releases, each with the current fixture holding the event it must decode to. */
  def legacy: List[(String, String)]

  protected def fixture(name: String): Json =
    val text = Using.resource(Source.fromResource(s"golden/$directory/$name.json"))(_.mkString)
    parse(text).fold(err => fail(s"Fixture $name is not valid JSON: $err"), identity)

  protected def decode(name: String): E = codec.decodeJson(fixture(name)).fold(err => fail(s"Decoding $name failed: $err"), identity)

  protected def decodingError(json: Json): String = codec.decodeJson(json).fold(_.message, event => fail(s"Unexpectedly decoded $event"))

  s"The $directory journal codec" should "write back every current-version fixture exactly" in:
    current.foreach(name => withClue(name)(codec(decode(name)) shouldEqual fixture(name)))

  it should "decode every legacy fixture to the event of its current-version counterpart" in:
    legacy.foreach((name, counterpart) => withClue(name)(decode(name) shouldEqual decode(counterpart)))

  it should "have a current-version fixture for every event case" in:
    current.flatMap(name => fixture(name).hcursor.downField("event").keys.toList.flatten).toSet shouldEqual cases

  it should "reject an event written by a newer release" in:
    current.headOption.foreach { name =>
      decodingError(fixture(name).mapObject(_.add("version", (schema.currentVersion + 1).asJson))) should include("newer release")
    }
end JournalGoldenSuite

object JournalGoldenSuite:
  /** The case names of an enum, which are the tags its derived codec writes. */
  inline def caseNames[E](using mirror: Mirror.SumOf[E]): Set[String] =
    constValueTuple[mirror.MirroredElemLabels].toList.collect { case label: String => label }.toSet
