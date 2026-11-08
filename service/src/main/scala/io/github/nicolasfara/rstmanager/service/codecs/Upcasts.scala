package io.github.nicolasfara.rstmanager.service.codecs

import cats.syntax.all.*
import io.circe.{ Decoder, DecodingFailure, Json, JsonObject }

/** Building blocks for the upcasters of the [[EventSchema]]s. */
object Upcasts:
  /** Whether any object in `data`, at any depth, has one of the given keys. */
  def hasKey(data: JsonObject, keys: Set[String]): Boolean =
    data.keys.exists(keys.contains) || data.values.exists(value => hasKey(value, keys))

  private def hasKey(json: Json, keys: Set[String]): Boolean =
    json.arrayOrObject(false, _.exists(hasKey(_, keys)), hasKey(_, keys))

  /**
   * The step from `version` to `version + 1` that rewrites the data, at any depth, by renaming each key of `renames` to its value and turning its
   * whole-hours integer into minutes: the switch from hour to minute task estimates.
   */
  def hoursToMinutes(version: Int, renames: Map[String, String])(envelope: EventEnvelope): Decoder.Result[EventEnvelope] =
    hoursToMinutes(renames, envelope.data).map(data => envelope.copy(version = version + 1, data = data))

  private def hoursToMinutes(renames: Map[String, String], data: JsonObject): Decoder.Result[JsonObject] =
    data.toList.traverse { (key, value) =>
      renames.get(key) match
        case Some(renamed) =>
          value
            .as[Int]
            .bimap(failure => DecodingFailure(s"$key is not a whole number of hours: ${failure.message}", Nil), h => renamed -> Json.fromInt(h * 60))
        case None => hoursToMinutes(renames, value).map(key -> _)
    }
      .map(JsonObject.fromIterable)

  private def hoursToMinutes(renames: Map[String, String], json: Json): Decoder.Result[Json] =
    json.arrayOrObject(
      Right(json),
      _.traverse(hoursToMinutes(renames, _)).map(Json.fromValues),
      hoursToMinutes(renames, _).map(Json.fromJsonObject),
    )

  /**
   * The step from `version` to `version + 1` that adds `key -> value` to every object of the data, at any depth, matching `where` and lacking `key`:
   * a new field with a neutral value for the records written before it existed.
   */
  def addFieldWhere(version: Int, where: JsonObject => Boolean, key: String, value: Json)(envelope: EventEnvelope): Decoder.Result[EventEnvelope] =
    Right(envelope.copy(version = version + 1, data = addFieldWhere(where, key, value, envelope.data)))

  private def addFieldWhere(where: JsonObject => Boolean, key: String, value: Json, data: JsonObject): JsonObject =
    val nested = data.mapValues(addFieldWhere(where, key, value, _))
    if where(nested) && !nested.contains(key) then nested.add(key, value) else nested

  private def addFieldWhere(where: JsonObject => Boolean, key: String, value: Json, json: Json): Json =
    json.arrayOrObject(
      json,
      values => Json.fromValues(values.map(addFieldWhere(where, key, value, _))),
      data => Json.fromJsonObject(addFieldWhere(where, key, value, data)),
    )

  /**
   * The step from `version` to `version + 1` that renames the case tag by `tags` and every key of the data, at any depth, by `keys`: a renamed case
   * or field with the same meaning. Tags and keys missing from the maps are kept.
   */
  def rename(version: Int, tags: Map[String, String], keys: Map[String, String])(envelope: EventEnvelope): Decoder.Result[EventEnvelope] =
    Right(EventEnvelope(tags.getOrElse(envelope.tag, envelope.tag), version + 1, renameKeys(keys, envelope.data)))

  private def renameKeys(keys: Map[String, String], data: JsonObject): JsonObject =
    JsonObject.fromIterable(data.toList.map((key, value) => keys.getOrElse(key, key) -> renameKeys(keys, value)))

  private def renameKeys(keys: Map[String, String], json: Json): Json =
    json.arrayOrObject(
      json,
      values => Json.fromValues(values.map(renameKeys(keys, _))),
      data => Json.fromJsonObject(renameKeys(keys, data)),
    )
end Upcasts
