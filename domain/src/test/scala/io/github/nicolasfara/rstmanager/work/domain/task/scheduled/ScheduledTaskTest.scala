package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.task.{TaskHours}
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.*
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskError.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID

class ScheduledTaskTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  // ---------------------------------------------------------------------------
  // Generators
  // ---------------------------------------------------------------------------

  private val genUUID: Gen[UUID] = Gen.delay(UUID.randomUUID().nn)
  private val genHours: Gen[TaskHours] = Gen.posNum[Int].map(TaskHours.applyUnsafe)

  /** Two hour values where `completed <= expected`, modelling a task still in
    * progress but not yet over budget.
    */
  private val genBoundedHours: Gen[(TaskHours, TaskHours)] =
    for
      expected  <- genHours
      completed <- Gen.chooseNum(1, expected.value).map(TaskHours.applyUnsafe)
    yield (expected, completed)

  private val genPendingTask: Gen[PendingTask] =
    for
      id      <- genUUID
      taskId  <- genUUID
      hours   <- genHours
    yield PendingTask(id, taskId, hours)

  private val genInProgressTask: Gen[InProgressTask] =
    for
      id             <- genUUID
      taskId         <- genUUID
      expectedHours  <- genHours
      completedHours <- genHours
    yield InProgressTask(id, taskId, expectedHours, completedHours)

  private val genBoundedInProgressTask: Gen[InProgressTask] =
    for
      id                      <- genUUID
      taskId                  <- genUUID
      (expected, completed)   <- genBoundedHours
    yield InProgressTask(id, taskId, expected, completed)

  private val genCompletedTask: Gen[CompletedTask] =
    for
      id             <- genUUID
      taskId         <- genUUID
      expectedHours  <- genHours
      completedHours <- genHours
    yield CompletedTask(id, taskId, expectedHours, completedHours, DateTime.now())

  private val zeroHours: TaskHours = TaskHours.applyUnsafe(0)

  // ---------------------------------------------------------------------------
  // PendingTask invariants
  // ---------------------------------------------------------------------------

  "A PendingTask" should "always report zero completedHours" in:
    forAll(genPendingTask): task =>
      task.completedHours shouldEqual zeroHours

  it should "always report remainingHours equal to expectedHours" in:
    forAll(genPendingTask): task =>
      task.remainingHours shouldEqual task.expectedHours

  it should "transition to InProgressTask with same id, taskId and expectedHours" in:
    forAll(genPendingTask): task =>
      val result = task.markAsInProgress
      result.isRight shouldEqual true
      result.foreach: t =>
        t shouldBe a[InProgressTask]
        t.id shouldEqual task.id
        t.taskId shouldEqual task.taskId
        t.expectedHours shouldEqual task.expectedHours
        t.completedHours shouldEqual zeroHours

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

  "A CompletedTask" should "always report zero remainingHours" in:
    forAll(genCompletedTask): task =>
      task.remainingHours shouldEqual zeroHours

  it should "reject further completion with TaskAlreadyCompleted" in:
    forAll(genCompletedTask, genHours): (task, hours) =>
      task.completeTask(hours) shouldEqual Left(TaskAlreadyCompleted)

  it should "reject markAsInProgress with TaskAlreadyCompleted" in:
    forAll(genCompletedTask): task =>
      task.markAsInProgress shouldEqual Left(TaskAlreadyCompleted)

  it should "revert to an InProgressTask whose expectedHours equals its completedHours" in:
    forAll(genCompletedTask): task =>
      val result = task.revertToInProgress
      result.isRight shouldEqual true
      result.foreach: t =>
        t shouldBe a[InProgressTask]
        t.id shouldEqual task.id
        t.taskId shouldEqual task.taskId
        t.expectedHours shouldEqual task.completedHours
        t.completedHours shouldEqual task.completedHours

  // ---------------------------------------------------------------------------
  // InProgressTask invariants
  // ---------------------------------------------------------------------------

  "An InProgressTask" should "report remainingHours = max(0, expectedHours - completedHours)" in:
    forAll(genInProgressTask): task =>
      val expected = Math.max(0, task.expectedHours.value - task.completedHours.value)
      task.remainingHours shouldEqual TaskHours.applyUnsafe(expected)

  it should "satisfy completedHours + remainingHours == expectedHours when not over budget" in:
    forAll(genBoundedInProgressTask): task =>
      task.completedHours.value + task.remainingHours.value shouldEqual task.expectedHours.value

  it should "return zero remainingHours when completedHours exceeds expectedHours" in:
    forAll(genInProgressTask): task =>
      if task.completedHours.value >= task.expectedHours.value then
        task.remainingHours shouldEqual zeroHours

  it should "be unchanged after advance then rollback by the same hours (roundtrip)" in:
    forAll(genInProgressTask, genHours): (task, hours) =>
      val roundtrip = task.advanceInProgressTask(hours).flatMap(_.rollbackInProgressTask(hours))
      roundtrip.isRight shouldEqual true
      roundtrip.foreach: t =>
        t.completedHours shouldEqual task.completedHours
        t.expectedHours shouldEqual task.expectedHours
        t.id shouldEqual task.id

  it should "accumulate completedHours monotonically when advanced multiple times" in:
    forAll(genInProgressTask, genHours, genHours): (task, h1, h2) =>
      val result = task.advanceInProgressTask(h1).flatMap(_.advanceInProgressTask(h2))
      result.isRight shouldEqual true
      result.foreach: t =>
        t.completedHours.value shouldEqual task.completedHours.value + h1.value + h2.value

  it should "reject rollbackInProgressTask when withHours exceeds completedHours" in:
    forAll(genInProgressTask): task =>
      val tooManyHours = TaskHours.applyUnsafe(task.completedHours.value + 1)
      task.rollbackInProgressTask(tooManyHours) shouldEqual Left(TaskWithNegativeProgress)

  it should "reject markAsInProgress with TaskAlreadyInProgress" in:
    forAll(genInProgressTask): task =>
      task.markAsInProgress shouldEqual Left(TaskAlreadyInProgress)

  it should "reject revertToInProgress with TaskAlreadyInProgress" in:
    forAll(genInProgressTask): task =>
      task.revertToInProgress shouldEqual Left(TaskAlreadyInProgress)

  it should "complete successfully and carry final completedHours = completedHours + withHours" in:
    forAll(genInProgressTask, genHours): (task, hours) =>
      val result = task.completeTask(hours)
      result.isRight shouldEqual true
      result.foreach: t =>
        t shouldBe a[CompletedTask]
        t.completedHours.value shouldEqual task.completedHours.value + hours.value
        t.expectedHours shouldEqual task.expectedHours

  // ---------------------------------------------------------------------------
  // createScheduledTask smart constructor
  // ---------------------------------------------------------------------------

  "createScheduledTask" should "succeed for any positive expectedHours and produce a PendingTask" in:
    forAll(genUUID, genUUID, Gen.posNum[Int]): (id, taskId, hours) =>
      val result = ScheduledTask.createScheduledTask(id, taskId, hours)
      result.isValid shouldEqual true
      result.foreach: task =>
        task.expectedHours.value shouldEqual hours
        task.completedHours shouldEqual zeroHours

  it should "fail for zero or negative expectedHours" in:
    forAll(Gen.chooseNum(Int.MinValue, -1)): hours =>
      ScheduledTask.createScheduledTask(UUID.randomUUID().nn, UUID.randomUUID().nn, hours).isValid shouldEqual false
