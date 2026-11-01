package io.github.nicolasfara.rstmanager.service.codecs

import io.circe.{ Decoder, Json, JsonObject }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class EventSchemaTest extends AnyFlatSpecLike:
  /** A v3 schema of counters `{"N": {"n": <int>}}`, upgraded by the given steps. */
  private def schema(steps: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]]): EventSchema[Int] = new EventSchema[Int]:
    val currentVersion: Int = 3
    val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = steps
    def legacyVersion(tag: String, data: JsonObject): Int = 1
    def write(event: Int): JsonObject = JsonObject("N" -> Json.obj("n" -> Json.fromInt(event)))
    def read(tag: String, data: JsonObject): Decoder.Result[Int] = Json.fromJsonObject(data).hcursor.get[Int]("n")

  private def mapN(to: Int, f: Int => Int)(envelope: EventEnvelope): Decoder.Result[EventEnvelope] =
    Json.fromJsonObject(envelope.data).hcursor.get[Int]("n").map(n => EventEnvelope(envelope.tag, to, JsonObject("n" -> Json.fromInt(f(n)))))

  private val legacyRow: Json = Json.obj("N" -> Json.obj("n" -> Json.fromInt(1)))

  "An event schema codec" should "apply the upcasters in version order" in:
    val codec = EventSchema.codec(schema(Map(1 -> mapN(2, _ + 10), 2 -> mapN(3, _ * 2))))
    codec.decodeJson(legacyRow) shouldEqual Right(22)

  it should "fail when an upcaster is missing" in:
    val codec = EventSchema.codec(schema(Map(1 -> mapN(2, identity))))
    codec.decodeJson(legacyRow).left.map(_.message) shouldEqual Left("No upcaster from v2 for event N")

  it should "fail when an upcaster does not advance the version by one" in:
    val codec = EventSchema.codec(schema(Map(1 -> mapN(3, identity))))
    codec.decodeJson(legacyRow).left.map(_.message) shouldEqual Left("The upcaster from v1 produced v3 for event N")
end EventSchemaTest
