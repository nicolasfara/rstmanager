package io.github.nicolasfara.rstmanager.service.codecs

import scala.annotation.tailrec

import io.circe.{ Codec, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject }

/** A persisted event being read: its case tag, the schema version its data was written with, and the data. */
final case class EventEnvelope(tag: String, version: Int, data: JsonObject)

/**
 * The persisted schema of an event family: what a release writes to the journal, and how it reads back every version ever written.
 *
 * Rows are stored as `{"version": 2, "event": {"<Tag>": data}}`. Rows written before versions existed are the bare `{"<Tag>": data}`. Stored rows are
 * never rewritten: every read upcasts the row, one version at a time, to [[currentVersion]] before decoding it. Hence:
 *   - changing what [[write]] produces requires bumping [[currentVersion]] and adding the upcaster from the previous version, plus golden fixtures of
 *     the new shape (the golden tests fail until then);
 *   - a renamed case or field is an upcaster step renaming it, and a changed meaning is a new case;
 *   - a refinement constraint is never tightened without an upcaster that repairs the stored values it would reject.
 */
trait EventSchema[E]:
  /** Schema version written by this release. */
  def currentVersion: Int

  /** `upcasters(n)` rewrites a version-`n` envelope into its version `n + 1` form. */
  def upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]]

  /** Schema version of a row persisted before versions existed, inferred from the shape of its data. */
  def legacyVersion(tag: String, data: JsonObject): Int

  /** Current-version encoding of an event, tagged by its case: `{"<Tag>": data}`. */
  def write(event: E): JsonObject

  /** Reads current-version data back into the event with the given tag. */
  def read(tag: String, data: JsonObject): Decoder.Result[E]
end EventSchema

/**
 * An [[EventSchema]] whose current-version encoding is the circe-derived one of `E`.
 *
 * The data shape then follows the Scala types and the codecs of their fields rather than being pinned by hand: the golden fixtures of every case are
 * what catch a changed shape.
 */
abstract class DerivedEventSchema[E](using codec: Codec.AsObject[E]) extends EventSchema[E]:
  def write(event: E): JsonObject = codec.encodeObject(event)
  def read(tag: String, data: JsonObject): Decoder.Result[E] = codec.decodeJson(Json.obj(tag -> Json.fromJsonObject(data)))

object EventSchema:
  /** No upcaster: every row ever written is still in the current shape. */
  val noUpcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = Map.empty

  /** Journal codec of an event family: writes current-version rows, reads every past version (and pre-version rows) via the upcasters. */
  def codec[E](schema: EventSchema[E]): Codec[E] = Codec.from(decoder(schema), encoder(schema))

  private def encoder[E](schema: EventSchema[E]): Encoder[E] = Encoder.instance { event =>
    Json.obj("version" -> Json.fromInt(schema.currentVersion), "event" -> Json.fromJsonObject(schema.write(event)))
  }

  private def decoder[E](schema: EventSchema[E]): Decoder[E] = Decoder.instance { cursor =>
    for
      stored <- readEnvelope(schema, cursor)
      current <- upcast(schema, stored, cursor)
      event <- schema.read(current.tag, current.data)
    yield event
  }

  private def readEnvelope[E](schema: EventSchema[E], cursor: HCursor): Decoder.Result[EventEnvelope] =
    cursor.as[JsonObject].flatMap { row =>
      if row.contains("version") && row.contains("event") then
        for
          version <- cursor.get[Int]("version")
          event <- cursor.get[JsonObject]("event")
          (tag, data) <- taggedData(event, cursor)
        yield EventEnvelope(tag, version, data)
      else taggedData(row, cursor).map((tag, data) => EventEnvelope(tag, schema.legacyVersion(tag, data), data))
    }

  private def taggedData(event: JsonObject, cursor: HCursor): Decoder.Result[(String, JsonObject)] =
    event.toList match
      case List((tag, data)) => data.asObject.map(tag -> _).toRight(DecodingFailure(s"Event $tag: data is not an object", cursor.history))
      case _ => Left(DecodingFailure("Neither a versioned event row nor a legacy single-key event", cursor.history))

  @tailrec
  private def upcast[E](schema: EventSchema[E], envelope: EventEnvelope, cursor: HCursor): Decoder.Result[EventEnvelope] =
    val version = envelope.version
    if version == schema.currentVersion then Right(envelope)
    else if version > schema.currentVersion then
      Left(
        DecodingFailure(
          s"Event ${envelope.tag} v$version was written by a newer release (this one reads up to v${schema.currentVersion})",
          cursor.history,
        ),
      )
    else
      schema.upcasters.get(version).map(_(envelope)) match
        case None => Left(DecodingFailure(s"No upcaster from v$version for event ${envelope.tag}", cursor.history))
        case Some(Left(failure)) => Left(failure)
        case Some(Right(next)) if next.version != version + 1 =>
          Left(DecodingFailure(s"The upcaster from v$version produced v${next.version} for event ${envelope.tag}", cursor.history))
        case Some(Right(next)) => upcast(schema, next, cursor)
  end upcast
end EventSchema
