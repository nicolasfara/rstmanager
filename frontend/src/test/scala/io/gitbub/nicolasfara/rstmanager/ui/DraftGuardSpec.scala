package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.ApiError
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure parts of [[DraftGuard]]: the discard prompt copy and the "record no longer exists" detection. */
final class DraftGuardSpec extends AnyFunSuite with Matchers:

  private final case class Row(id: UUID)

  private val first = UUID.fromString("00000000-0000-0000-0000-000000000801").nn
  private val second = UUID.fromString("00000000-0000-0000-0000-000000000802").nn

  test("leaving a new draft asks to abandon it, an edit to discard the changes"):
    DraftGuard.discardTitle(None, "il nuovo cliente") shouldBe "Abbandonare il nuovo cliente?"
    DraftGuard.discardTitle(Some("Mario Rossi"), "il nuovo cliente") shouldBe "Scartare le modifiche a «Mario Rossi»?"

  test("an edited record is missing only when a successful load lacks it"):
    val loaded = Some(Right(List(Row(first))))
    DraftGuard.missingFrom(loaded, Some(first), _.id) shouldBe Some(false)
    DraftGuard.missingFrom(loaded, Some(second), _.id) shouldBe Some(true)
    DraftGuard.missingFrom(Some(Right(List.empty[Row])), Some(first), _.id) shouldBe Some(true)

  test("loading, a failed load or a new record say nothing"):
    DraftGuard.missingFrom(Option.empty[Either[ApiError, List[Row]]], Some(first), _.id) shouldBe None
    DraftGuard.missingFrom[Row](Some(Left(ApiError("network", "Rete non disponibile", Nil))), Some(first), _.id) shouldBe None
    DraftGuard.missingFrom(Some(Right(List(Row(first)))), None, _.id) shouldBe None
end DraftGuardSpec
