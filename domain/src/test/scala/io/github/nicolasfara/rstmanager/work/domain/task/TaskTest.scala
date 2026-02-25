package io.github.nicolasfara.rstmanager.work.domain.task

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID

class TaskTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  // ---------------------------------------------------------------------------
  // Generators
  // ---------------------------------------------------------------------------

  private val genUUID: Gen[UUID] = Gen.delay(UUID.randomUUID().nn)
  private val genNonEmptyString: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)
  private val genNonNegativeInt: Gen[Int] = Gen.chooseNum(0, Int.MaxValue)
  private val genNegativeInt: Gen[Int] = Gen.chooseNum(Int.MinValue, -1)

  // ---------------------------------------------------------------------------
  // createTask – valid inputs
  // ---------------------------------------------------------------------------

  "Task.createTask" should "succeed for any non-empty name, valid optional description, and non-negative hours" in:
    forAll(genUUID, genNonEmptyString, Gen.option(genNonEmptyString), genNonNegativeInt): (id, name, desc, hours) =>
      Task.createTask(id, name, desc, hours).isValid shouldEqual true

  it should "preserve all field values when creation succeeds" in:
    forAll(genUUID, genNonEmptyString, Gen.option(genNonEmptyString), genNonNegativeInt): (id, name, desc, hours) =>
      Task.createTask(id, name, desc, hours).foreach: task =>
        task.id shouldEqual id
        task.name.toString shouldEqual name
        task.taskDescription.map(_.toString) shouldEqual desc
        task.requiredHours.value shouldEqual hours

  it should "allow zero as a valid requiredHours value" in:
    forAll(genUUID, genNonEmptyString): (id, name) =>
      Task.createTask(id, name, None, 0).isValid shouldEqual true

  // ---------------------------------------------------------------------------
  // createTask – invalid inputs
  // ---------------------------------------------------------------------------

  it should "fail when name is empty" in:
    forAll(genUUID, genNonNegativeInt): (id, hours) =>
      Task.createTask(id, "", None, hours).isValid shouldEqual false

  it should "fail when requiredHours is negative" in:
    forAll(genUUID, genNonEmptyString, genNegativeInt): (id, name, hours) =>
      Task.createTask(id, name, None, hours).isValid shouldEqual false

  it should "fail when the description is provided but empty" in:
    forAll(genUUID, genNonEmptyString, genNonNegativeInt): (id, name, hours) =>
      Task.createTask(id, name, Some(""), hours).isValid shouldEqual false

  it should "accumulate multiple errors when both name and hours are invalid" in:
    forAll(genUUID, genNegativeInt): (id, hours) =>
      val errors = Task.createTask(id, "", None, hours)
      errors.isValid shouldEqual false
      errors.swap.foreach(_.length shouldEqual 2L)

  // ---------------------------------------------------------------------------
  // TaskHours – Monoid laws
  // ---------------------------------------------------------------------------

  "TaskHours Monoid" should "satisfy left identity: empty + x == x" in:
    forAll(genNonNegativeInt): n =>
      val h = TaskHours.applyUnsafe(n)
      (TaskHours.applyUnsafe(0) + h) shouldEqual h

  it should "satisfy right identity: x + empty == x" in:
    forAll(genNonNegativeInt): n =>
      val h = TaskHours.applyUnsafe(n)
      (h + TaskHours.applyUnsafe(0)) shouldEqual h

  it should "satisfy associativity: (a + b) + c == a + (b + c)" in:
    // Bound values to avoid Int overflow when summing three TaskHours
    val genSafeHours = Gen.chooseNum(0, Int.MaxValue / 3)
    forAll(genSafeHours, genSafeHours, genSafeHours): (a, b, c) =>
      val ha = TaskHours.applyUnsafe(a)
      val hb = TaskHours.applyUnsafe(b)
      val hc = TaskHours.applyUnsafe(c)
      ((ha + hb) + hc) shouldEqual (ha + (hb + hc))

  it should "have subtraction return the raw Int difference" in:
    forAll(genNonNegativeInt, genNonNegativeInt): (a, b) =>
      val ha = TaskHours.applyUnsafe(a)
      val hb = TaskHours.applyUnsafe(b)
      (ha - hb) shouldEqual (a - b)
