package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.task.TaskDuration
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.*
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskError.*

import com.github.nscala_time.time.Imports.DateTime
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ScheduledTaskTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  // ---------------------------------------------------------------------------
  // Generators
  // ---------------------------------------------------------------------------

  private val genUUID: Gen[UUID] = Gen.delay(UUID.randomUUID().nn)
  private val genHours: Gen[TaskDuration] = Gen.posNum[Int].map(TaskDuration.applyUnsafe)

  /**
   * Two hour values where `completed <= expected`, modelling a task still in progress but not yet over budget.
   */
  private val genBoundedHours: Gen[(TaskDuration, TaskDuration)] =
    for
      expected <- genHours
      completed <- Gen.chooseNum(1, expected.value).map(TaskDuration.applyUnsafe)
    yield (expected, completed)

  private val genPendingTask: Gen[PendingTask] =
    for
      id <- genUUID
      taskId <- genUUID
      hours <- genHours
    yield PendingTask(id, taskId, hours)

  private val genInProgressTask: Gen[InProgressTask] =
    for
      id <- genUUID
      taskId <- genUUID
      expectedDuration <- genHours
      completedDuration <- genHours
    yield InProgressTask(id, taskId, expectedDuration, completedDuration)

  private val genBoundedInProgressTask: Gen[InProgressTask] =
    for
      id <- genUUID
      taskId <- genUUID
      (expected, completed) <- genBoundedHours
    yield InProgressTask(id, taskId, expected, completed)

  private val genCompletedTask: Gen[CompletedTask] =
    for
      id <- genUUID
      taskId <- genUUID
      expectedDuration <- genHours
      completedDuration <- genHours
    yield CompletedTask(id, taskId, expectedDuration, completedDuration, DateTime.now())

  private val zeroHours: TaskDuration = TaskDuration.applyUnsafe(0)

  // ---------------------------------------------------------------------------
  // PendingTask invariants
  // ---------------------------------------------------------------------------

  "A PendingTask" should "always report zero completedDuration" in:
    forAll(genPendingTask): task =>
      task.completedDuration shouldEqual zeroHours

  it should "always report remainingDuration equal to expectedDuration" in:
    forAll(genPendingTask): task =>
      task.remainingDuration shouldEqual task.expectedDuration

  it should "transition to InProgressTask with same id, taskId and expectedDuration" in:
    forAll(genPendingTask): task =>
      val result = task.markAsInProgress
      result.isRight shouldEqual true
      result.foreach: t =>
        t shouldBe a[InProgressTask]
        t.id shouldEqual task.id
        t.taskId shouldEqual task.taskId
        t.expectedDuration shouldEqual task.expectedDuration
        t.completedDuration shouldEqual zeroHours

  it should "reject revertToInProgress with TaskMustBeInProgress" in:
    forAll(genPendingTask): task =>
      task.revertToInProgress shouldEqual Left(TaskMustBeInProgress)

  it should "reject advanceInProgressTask with TaskMustBeInProgress" in:
    forAll(genPendingTask, genHours): (task, hours) =>
      task.advanceInProgressTask(hours) shouldEqual Left(TaskMustBeInProgress)

  it should "reject rollbackInProgressTask with TaskMustBeInProgress" in:
    forAll(genPendingTask, genHours): (task, hours) =>
      task.rollbackInProgressTask(hours) shouldEqual Left(TaskMustBeInProgress)

  // ---------------------------------------------------------------------------
  // CompletedTask invariants
  // ---------------------------------------------------------------------------

  "A CompletedTask" should "always report zero remainingDuration" in:
    forAll(genCompletedTask): task =>
      task.remainingDuration shouldEqual zeroHours

  it should "reject further completion with TaskAlreadyCompleted" in:
    forAll(genCompletedTask, genHours): (task, hours) =>
      task.completeTask(hours) shouldEqual Left(TaskAlreadyCompleted)

  it should "reject markAsInProgress with TaskAlreadyCompleted" in:
    forAll(genCompletedTask): task =>
      task.markAsInProgress shouldEqual Left(TaskAlreadyCompleted)

  it should "revert to an InProgressTask whose expectedDuration equals its completedDuration" in:
    forAll(genCompletedTask): task =>
      val result = task.revertToInProgress
      result.isRight shouldEqual true
      result.foreach: t =>
        t shouldBe a[InProgressTask]
        t.id shouldEqual task.id
        t.taskId shouldEqual task.taskId
        t.expectedDuration shouldEqual task.completedDuration
        t.completedDuration shouldEqual task.completedDuration

  // ---------------------------------------------------------------------------
  // InProgressTask invariants
  // ---------------------------------------------------------------------------

  "An InProgressTask" should "report remainingDuration = max(0, expectedDuration - completedDuration)" in:
    forAll(genInProgressTask): task =>
      val expected = Math.max(0, task.expectedDuration.value - task.completedDuration.value)
      task.remainingDuration shouldEqual TaskDuration.applyUnsafe(expected)

  it should "satisfy completedDuration + remainingDuration == expectedDuration when not over budget" in:
    forAll(genBoundedInProgressTask): task =>
      task.completedDuration.value + task.remainingDuration.value shouldEqual task.expectedDuration.value

  it should "return zero remainingDuration when completedDuration exceeds expectedDuration" in:
    forAll(genInProgressTask): task =>
      if task.completedDuration.value >= task.expectedDuration.value then task.remainingDuration shouldEqual zeroHours

  it should "be unchanged after advance then rollback by the same hours (roundtrip)" in:
    forAll(genInProgressTask, genHours): (task, hours) =>
      val roundtrip = task.advanceInProgressTask(hours).flatMap(_.rollbackInProgressTask(hours))
      roundtrip.isRight shouldEqual true
      roundtrip.foreach: t =>
        t.completedDuration shouldEqual task.completedDuration
        t.expectedDuration shouldEqual task.expectedDuration
        t.id shouldEqual task.id

  it should "accumulate completedDuration monotonically when advanced multiple times" in:
    forAll(genInProgressTask, genHours, genHours): (task, h1, h2) =>
      val result = task.advanceInProgressTask(h1).flatMap(_.advanceInProgressTask(h2))
      result.isRight shouldEqual true
      result.foreach: t =>
        t.completedDuration.value shouldEqual task.completedDuration.value + h1.value + h2.value

  it should "reject rollbackInProgressTask when withHours exceeds completedDuration" in:
    forAll(genInProgressTask): task =>
      val tooManyHours = TaskDuration.applyUnsafe(task.completedDuration.value + 1)
      task.rollbackInProgressTask(tooManyHours) shouldEqual Left(TaskWithNegativeProgress)

  it should "reject markAsInProgress with TaskAlreadyInProgress" in:
    forAll(genInProgressTask): task =>
      task.markAsInProgress shouldEqual Left(TaskAlreadyInProgress)

  it should "reject revertToInProgress with TaskAlreadyInProgress" in:
    forAll(genInProgressTask): task =>
      task.revertToInProgress shouldEqual Left(TaskAlreadyInProgress)

  it should "complete successfully and carry final completedDuration = completedDuration + withHours" in:
    forAll(genInProgressTask, genHours): (task, hours) =>
      val result = task.completeTask(hours)
      result.isRight shouldEqual true
      result.foreach: t =>
        t shouldBe a[CompletedTask]
        t.completedDuration.value shouldEqual task.completedDuration.value + hours.value
        t.expectedDuration shouldEqual task.expectedDuration

  // ---------------------------------------------------------------------------
  // setProgress / changeExpectedDuration
  // ---------------------------------------------------------------------------

  "setProgress" should "complete the task when the completed hours reach the expected hours" in:
    forAll(genPendingTask): task =>
      val result = task.setProgress(task.expectedDuration)
      result shouldBe a[CompletedTask]
      result.completedDuration shouldEqual task.expectedDuration
      result.remainingDuration shouldEqual zeroHours

  it should "return the task to pending when the completed hours are zero" in:
    forAll(genBoundedInProgressTask): task =>
      val result = task.setProgress(zeroHours)
      result shouldBe a[PendingTask]
      result.expectedDuration shouldEqual task.expectedDuration

  it should "keep the task in progress for a partial completion below the estimate" in:
    forAll(genBoundedInProgressTask): task =>
      whenever(task.expectedDuration.value > 1):
        val partial = TaskDuration.applyUnsafe(task.expectedDuration.value - 1)
        val result = task.setProgress(partial)
        result shouldBe a[InProgressTask]
        result.completedDuration shouldEqual partial

  it should "reopen a completed task when progress drops below the estimate" in:
    forAll(genCompletedTask): task =>
      whenever(task.expectedDuration.value > 1):
        val result = task.setProgress(TaskDuration.applyUnsafe(task.expectedDuration.value - 1))
        result shouldBe a[InProgressTask]

  "changeExpectedDuration" should "preserve the completed hours while re-deriving the state" in:
    forAll(genBoundedInProgressTask, genHours): (task, newExpected) =>
      val result = task.changeExpectedDuration(newExpected)
      result.completedDuration shouldEqual task.completedDuration
      result.expectedDuration shouldEqual newExpected

  it should "complete the task when the new estimate is not above the completed hours" in:
    forAll(genBoundedInProgressTask): task =>
      whenever(task.completedDuration.value > 0):
        val result = task.changeExpectedDuration(task.completedDuration)
        result shouldBe a[CompletedTask]

  // ---------------------------------------------------------------------------
  // createScheduledTask smart constructor
  // ---------------------------------------------------------------------------

  "createScheduledTask" should "succeed for any positive expectedDuration and produce a PendingTask" in:
    forAll(genUUID, genUUID, Gen.posNum[Int]): (id, taskId, hours) =>
      val result = ScheduledTask.createScheduledTask(id, taskId, hours)
      result.isValid shouldEqual true
      result.foreach: task =>
        task.expectedDuration.value shouldEqual hours
        task.completedDuration shouldEqual zeroHours

  it should "fail for zero or negative expectedDuration" in:
    forAll(Gen.chooseNum(Int.MinValue, -1)): hours =>
      ScheduledTask.createScheduledTask(UUID.randomUUID().nn, UUID.randomUUID().nn, hours).isValid shouldEqual false
end ScheduledTaskTest
