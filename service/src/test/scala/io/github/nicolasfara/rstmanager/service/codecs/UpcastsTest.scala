package io.github.nicolasfara.rstmanager.service.codecs

import io.circe.{ Json, JsonObject }
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class UpcastsTest extends AnyFlatSpecLike:
  private def data(text: String): JsonObject =
    parse(text).toOption.flatMap(_.asObject).getOrElse(fail(s"Not a JSON object: $text"))

  private val isInfo: JsonObject => Boolean = _.contains("tasks")

  private def addMaterials(text: String): EventEnvelope =
    Upcasts
      .addFieldWhere(2, isInfo, "materials", Json.arr())(EventEnvelope("E", 2, data(text)))
      .fold(failure => fail(failure.message), identity)

  "addFieldWhere" should "add the field to every matching object, at any depth, and advance the version" in:
    val upcast = addMaterials("""{"outer":{"tasks":[1]},"list":[{"x":{"tasks":[]}},{"y":1}],"tasks":[]}""")

    upcast.version shouldEqual 3
    upcast.data shouldEqual data(
      """{"outer":{"tasks":[1],"materials":[]},"list":[{"x":{"tasks":[],"materials":[]}},{"y":1}],"tasks":[],"materials":[]}""",
    )

  it should "keep a field that is already there" in:
    addMaterials("""{"m":{"tasks":[],"materials":[1]}}""").data shouldEqual data("""{"m":{"tasks":[],"materials":[1]}}""")

  it should "leave data without matching objects unchanged" in:
    addMaterials("""{"manufacturingId":"a","changedOn":"b","list":[1,"two",null]}""").data shouldEqual
      data("""{"manufacturingId":"a","changedOn":"b","list":[1,"two",null]}""")

  "rename" should "rename the tag and every listed key, at any depth, keeping the order of the keys, and advance the version" in:
    val upcast = Upcasts
      .rename(1, Map("Old" -> "New"), Map("oldId" -> "newId"))(
        EventEnvelope("Old", 1, data("""{"oldId":1,"list":[{"x":{"oldId":2,"y":3}}],"z":4}""")),
      )
      .fold(failure => fail(failure.message), identity)

    upcast shouldEqual EventEnvelope("New", 2, data("""{"newId":1,"list":[{"x":{"newId":2,"y":3}}],"z":4}"""))
    upcast.data.keys.toList shouldEqual List("newId", "list", "z")

  it should "keep a tag that is not listed" in:
    Upcasts.rename(1, Map("Old" -> "New"), Map.empty)(EventEnvelope("Other", 1, data("{}"))).map(_.tag) shouldEqual Right("Other")
end UpcastsTest
