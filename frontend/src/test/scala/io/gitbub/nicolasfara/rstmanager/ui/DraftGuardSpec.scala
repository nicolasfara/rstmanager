package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the wording of the "discard the draft?" confirmation of the side forms. */
final class DraftGuardSpec extends AnyFunSuite with Matchers:

  test("a record being edited is named, a new draft is described"):
    DraftGuard.discardTitle(Some("Mario Rossi"), "il nuovo cliente") shouldBe "Scartare le modifiche a «Mario Rossi»?"
    DraftGuard.discardTitle(None, "il nuovo cliente") shouldBe "Scartare il nuovo cliente?"
