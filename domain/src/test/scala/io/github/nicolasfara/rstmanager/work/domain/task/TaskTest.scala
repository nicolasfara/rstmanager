package io.github.nicolasfara.rstmanager.work.domain.task

import java.util.UUID

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TaskTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  // ---------------------------------------------------------------------------
  // Generators
  // ---------------------------------------------------------------------------

  private val genUUID: Gen[UUID] = Gen.delay(UUID.randomUUID().nn)
  private val genOptionEmployee: Gen[Option[UUID]] = Gen.option(genUUID)
  private val genNonEmptyString: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)
  private val genNonNegativeInt: Gen[Int] = Gen.chooseNum(0, Int.MaxValue)
  private val genNegativeInt: Gen[Int] = Gen.chooseNum(Int.MinValue, -1)

  // ---------------------------------------------------------------------------
  // createTask – valid inputs
  // ---------------------------------------------------------------------------

  "Task.createTask" should "succeed for any non-empty name, valid optional description, and non-negative hours" in:
    forAll(genUUID, genNonEmptyString, Gen.option(genNonEmptyString), genNonNegativeInt, genOptionEmployee): (id, name, desc, hours, employee) =>
      Task.createTask(id, name, desc, hours, employee).isValid shouldEqual true

  it should "preserve all field values when creation succeeds" in:
    forAll(genUUID, genNonEmptyString, Gen.option(genNonEmptyString), genNonNegativeInt, genOptionEmployee): (id, name, desc, hours, employee) =>
      Task
        .createTask(id, name, desc, hours, employee)
        .foreach: task =>
          task.id shouldEqual id
          task.name.toString shouldEqual name
          task.taskDescription.map(_.toString) shouldEqual desc
          task.requiredDuration.value shouldEqual hours
          task.defaultEmployeeId shouldEqual employee

  it should "allow zero as a valid requiredHours value" in:
    forAll(genUUID, genNonEmptyString): (id, name) =>
      Task.createTask(id, name, None, 0, None).isValid shouldEqual true

  // ---------------------------------------------------------------------------
  // createTask – invalid inputs
  // ---------------------------------------------------------------------------

  it should "fail when name is empty" in:
    forAll(genUUID, genNonNegativeInt): (id, hours) =>
      Task.createTask(id, "", None, hours, None).isValid shouldEqual false

  it should "fail when requiredHours is negative" in:
    forAll(genUUID, genNonEmptyString, genNegativeInt): (id, name, hours) =>
      Task.createTask(id, name, None, hours, None).isValid shouldEqual false

  it should "fail when the description is provided but empty" in:
    forAll(genUUID, genNonEmptyString, genNonNegativeInt): (id, name, hours) =>
      Task.createTask(id, name, Some(""), hours, None).isValid shouldEqual false

  it should "accumulate multiple errors when both name and hours are invalid" in:
    forAll(genUUID, genNegativeInt): (id, hours) =>
      val errors = Task.createTask(id, "", None, hours, None)
      errors.isValid shouldEqual false
      errors.swap.foreach(_.length shouldEqual 2L)

  // ---------------------------------------------------------------------------
  // TaskDuration – Monoid laws
  // ---------------------------------------------------------------------------

  "TaskDuration Monoid" should "satisfy left identity: empty + x == x" in:
    forAll(genNonNegativeInt): n =>
      val h = TaskDuration.applyUnsafe(n)
      (TaskDuration.applyUnsafe(0) + h) shouldEqual h

  it should "satisfy right identity: x + empty == x" in:
    forAll(genNonNegativeInt): n =>
      val h = TaskDuration.applyUnsafe(n)
      (h + TaskDuration.applyUnsafe(0)) shouldEqual h

  it should "satisfy associativity: (a + b) + c == a + (b + c)" in:
    // Bound values to avoid Int overflow when summing three TaskDuration
    val genSafeHours = Gen.chooseNum(0, Int.MaxValue / 3)
    forAll(genSafeHours, genSafeHours, genSafeHours): (a, b, c) =>
      val ha = TaskDuration.applyUnsafe(a)
      val hb = TaskDuration.applyUnsafe(b)
      val hc = TaskDuration.applyUnsafe(c)
      ((ha + hb) + hc) shouldEqual (ha + (hb + hc))

  it should "have subtraction return the raw Int difference" in:
    forAll(genNonNegativeInt, genNonNegativeInt): (a, b) =>
      val ha = TaskDuration.applyUnsafe(a)
      val hb = TaskDuration.applyUnsafe(b)
      (ha - hb) shouldEqual (a - b)
end TaskTest
