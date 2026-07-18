package io.github.nicolasfara.rstmanager.service.auth

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

final class RoleTest extends AnyFlatSpecLike with Matchers:
  "Role.satisfies" should "honour the admin > operator > viewer hierarchy" in {
    Role.Admin.satisfies(Role.Viewer) shouldBe true
    Role.Admin.satisfies(Role.Operator) shouldBe true
    Role.Admin.satisfies(Role.Admin) shouldBe true
    Role.Operator.satisfies(Role.Viewer) shouldBe true
    Role.Operator.satisfies(Role.Admin) shouldBe false
    Role.Viewer.satisfies(Role.Operator) shouldBe false
    Role.Viewer.satisfies(Role.Viewer) shouldBe true
  }

  "Role.fromString" should "round-trip every role name" in {
    Role.values.foreach(role => Role.fromString(role.name) shouldBe Some(role))
  }

  it should "ignore unknown role strings" in {
    Role.fromString("uma_authorization") shouldBe None
    Role.fromString("Admin") shouldBe None
    Role.fromString("") shouldBe None
  }
end RoleTest
