package io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled

import cats.data.NonEmptyList
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.constraint.any.{DescribedAs, Not}
import io.github.iltotore.iron.constraint.collection.Empty
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.*
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId.given
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingError.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID

class ScheduledManufacturingTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  // ---------------------------------------------------------------------------
  // Generators
  // ---------------------------------------------------------------------------

  private val genUUID: Gen[UUID] = Gen.delay(UUID.randomUUID().nn)
  private val genHours: Gen[TaskHours] = Gen.posNum[Int].map(TaskHours.applyUnsafe)

  private val genPendingTask: Gen[PendingTask] =
    for
      id     <- genUUID
      taskId <- genUUID
      hours  <- genHours
    yield PendingTask(id, taskId, hours)

  private val genInProgressTask: Gen[InProgressTask] =
    for
      id             <- genUUID
      taskId         <- genUUID
      expectedHours  <- genHours
      completedHours <- genHours
    yield InProgressTask(id, taskId, expectedHours, completedHours)

  private val genScheduledTask: Gen[ScheduledTask] =
    Gen.oneOf(genPendingTask, genInProgressTask)

  // ManufacturingCode = DescribedAs[Not[Empty], "..."]: a phantom class with no
  // fields — all instances are equivalent at runtime.
  private val fixedCode: ManufacturingCode =
    new DescribedAs[Not[Empty], "The code manufacturing should be not empty"]()

  private def genInfo(tasks: NonEmptyList[ScheduledTask]): Gen[ScheduledManufacturingInfo] =
    genUUID.map(ScheduledManufacturingInfo(_, fixedCode, DateTime.now(), tasks, ManufacturingDependencies()))

  private val genNonEmptyTaskList: Gen[NonEmptyList[ScheduledTask]] =
    Gen.nonEmptyListOf(genScheduledTask).map(NonEmptyList.fromListUnsafe)

  private val genNotStarted: Gen[NotStartedManufacturing] =
    genNonEmptyTaskList.flatMap(tasks => genInfo(tasks).map(NotStartedManufacturing.apply))

  // Flat triple (mfg, taskId, hours) for advance/rollback forAll lambdas.
  private val genNotStartedWithInProgressAndHours: Gen[(NotStartedManufacturing, ScheduledTaskId, TaskHours)] =
    for
      focus <- genInProgressTask
      rest  <- Gen.listOf(genScheduledTask)
      info  <- genInfo(NonEmptyList(focus, rest))
      hours <- genHours
    yield (NotStartedManufacturing(info), focus.id, hours)

  // Flat triple for single-task complete/revert forAll lambdas.
  private val genNotStartedSingleAndHours: Gen[(NotStartedManufacturing, ScheduledTaskId, TaskHours)] =
    for
      task  <- genInProgressTask
      info  <- genInfo(NonEmptyList.one(task))
      hours <- genHours
    yield (NotStartedManufacturing(info), task.id, hours)

  // ---------------------------------------------------------------------------
  // Hours aggregation invariants
  // ---------------------------------------------------------------------------

  "ScheduledManufacturing" should "report expectedHours equal to the sum of all tasks' expectedHours" in:
    forAll(genNotStarted): mfg =>
      val expected = mfg.info.tasks.foldMap(_.expectedHours)
      mfg.expectedHours shouldEqual expected

  it should "report remainingHours equal to the sum of all tasks' remainingHours" in:
    forAll(genNotStarted): mfg =>
      val expected = mfg.info.tasks.foldMap(_.remainingHours)
      mfg.remainingHours shouldEqual expected

  it should "report completedHours equal to the sum of all tasks' completedHours" in:
    forAll(genNotStarted): mfg =>
      val expected = mfg.info.tasks.foldMap(_.completedHours)
      mfg.completedHours shouldEqual expected

  // ---------------------------------------------------------------------------
  // addTask
  // ---------------------------------------------------------------------------

  "addTask" should "increase the task count by exactly 1" in:
    forAll(genNotStarted, genPendingTask): (mfg, task) =>
      val before = mfg.info.tasks.size
      mfg.addTask(task, Set.empty).info.tasks.size shouldEqual before + 1

  it should "make the new task findable by its ID afterwards" in:
    forAll(genNotStarted, genPendingTask): (mfg, task) =>
      mfg.addTask(task, Set.empty).info.tasks.exists(_.id == task.id) shouldEqual true

  it should "preserve the IDs of all pre-existing tasks" in:
    forAll(genNotStarted, genPendingTask): (mfg, task) =>
      val originalIds = mfg.info.tasks.map(_.id).toList.toSet
      val updatedIds  = mfg.addTask(task, Set.empty).info.tasks.map(_.id).toList.toSet
      originalIds.subsetOf(updatedIds) shouldEqual true

  // ---------------------------------------------------------------------------
  // removeTask
  // ---------------------------------------------------------------------------

  "removeTask" should "return TaskIdNotFound for an ID that does not exist" in:
    forAll(genNotStarted, genUUID): (mfg, unknownId) =>
      whenever(!mfg.info.tasks.exists(_.id == unknownId)):
        mfg.removeTask(unknownId) shouldEqual Left(TaskIdNotFound(unknownId))

  it should "return ManufacturingWithNoTasks when the manufacturing has a single task" in:
    forAll(genPendingTask.flatMap(t => genInfo(NonEmptyList.one(t)).map(NotStartedManufacturing.apply))): mfg =>
      val onlyId = mfg.info.tasks.head.id
      mfg.removeTask(onlyId) shouldEqual Left(ManufacturingWithNoTasks)

  it should "decrease task count by 1 on success" in:
    // At least two tasks are needed to allow removal
    forAll(genNotStarted.suchThat(_.info.tasks.size >= 2)): mfg =>
      val targetId = mfg.info.tasks.head.id
      val result   = mfg.removeTask(targetId)
      result.isRight shouldEqual true
      result.foreach(_.info.tasks.size shouldEqual mfg.info.tasks.size - 1)

  it should "make the removed task's ID absent from the updated task list" in:
    forAll(genNotStarted.suchThat(_.info.tasks.size >= 2)): mfg =>
      val targetId = mfg.info.tasks.head.id
      mfg.removeTask(targetId).foreach: updated =>
        updated.info.tasks.exists(_.id == targetId) shouldEqual false

  it should "be consistent with addTask: add then remove yields the original task count" in:
    forAll(genNotStarted, genPendingTask): (mfg, newTask) =>
      val roundtrip = mfg.addTask(newTask, Set.empty).removeTask(newTask.id)
      roundtrip.isRight shouldEqual true
      roundtrip.foreach(_.info.tasks.size shouldEqual mfg.info.tasks.size)

  // ---------------------------------------------------------------------------
  // advanceTask / rollbackTask
  // ---------------------------------------------------------------------------

  "advanceTask" should "return TaskIdNotFound for an unknown ID" in:
    forAll(genNotStarted, genUUID, genHours): (mfg, unknownId, hours) =>
      whenever(!mfg.info.tasks.exists(_.id == unknownId)):
        mfg.advanceTask(unknownId, hours) shouldEqual Left(TaskIdNotFound(unknownId))

  it should "increase the total completedHours by the advanced amount" in:
    forAll(genNotStartedWithInProgressAndHours): t =>
      val (mfg, taskId, hours) = t
      val result = mfg.advanceTask(taskId, hours)
      result.isRight shouldEqual true
      result.foreach: updated =>
        updated.completedHours.value shouldEqual mfg.completedHours.value + hours.value

  it should "transition a NotStartedManufacturing to InProgressManufacturing" in:
    forAll(genNotStartedWithInProgressAndHours): t =>
      val (mfg, taskId, hours) = t
      mfg.advanceTask(taskId, hours).foreach(_ shouldBe a[InProgressManufacturing])

  "rollbackTask" should "return TaskIdNotFound for an unknown ID" in:
    forAll(genNotStarted, genUUID, genHours): (mfg, unknownId, hours) =>
      whenever(!mfg.info.tasks.exists(_.id == unknownId)):
        mfg.rollbackTask(unknownId, hours) shouldEqual Left(TaskIdNotFound(unknownId))

  "advanceTask then rollbackTask" should "be a roundtrip: total completedHours is unchanged" in:
    forAll(genNotStartedWithInProgressAndHours): t =>
      val (mfg, taskId, hours) = t
      val roundtrip = mfg.advanceTask(taskId, hours).flatMap(_.rollbackTask(taskId, hours))
      roundtrip.isRight shouldEqual true
      roundtrip.foreach(_.completedHours shouldEqual mfg.completedHours)

  // ---------------------------------------------------------------------------
  // completeTask
  // ---------------------------------------------------------------------------

  "completeTask" should "return TaskIdNotFound for an unknown ID" in:
    forAll(genNotStarted, genUUID, genHours): (mfg, unknownId, hours) =>
      whenever(!mfg.info.tasks.exists(_.id == unknownId)):
        mfg.completeTask(unknownId, hours) shouldEqual Left(TaskIdNotFound(unknownId))

  it should "transition to CompletedManufacturing when the last task is completed" in:
    forAll(genNotStartedSingleAndHours): t =>
      val (mfg, taskId, hours) = t
      mfg.completeTask(taskId, hours).foreach(_ shouldBe a[CompletedManufacturing])

  it should "stay in InProgressManufacturing when there are still incomplete tasks" in:
    val genTwoInProgressAndHours: Gen[(NotStartedManufacturing, ScheduledTaskId, TaskHours)] =
      for
        t1    <- genInProgressTask
        t2    <- genInProgressTask
        rest  <- Gen.listOf(genScheduledTask)
        info  <- genInfo(NonEmptyList(t1, t2 :: rest))
        hours <- genHours
      yield (NotStartedManufacturing(info), t1.id, hours)
    forAll(genTwoInProgressAndHours): t =>
      val (mfg, taskId, hours) = t
      mfg.completeTask(taskId, hours).foreach(_ shouldBe a[InProgressManufacturing])

  // ---------------------------------------------------------------------------
  // revertTaskToInProgress
  // ---------------------------------------------------------------------------

  "revertTaskToInProgress" should "return TaskIdNotFound for an unknown ID" in:
    forAll(genNotStarted, genUUID): (mfg, unknownId) =>
      whenever(!mfg.info.tasks.exists(_.id == unknownId)):
        mfg.revertTaskToInProgress(unknownId) shouldEqual Left(TaskIdNotFound(unknownId))

  it should "transition a CompletedManufacturing back to InProgressManufacturing" in:
    forAll(genNotStartedSingleAndHours): t =>
      val (mfg, taskId, hours) = t
      val result =
        for
          completed <- mfg.completeTask(taskId, hours)
          reverted  <- completed.revertTaskToInProgress(completed.info.tasks.head.id)
        yield reverted
      result.isRight shouldEqual true
      result.foreach(_ shouldBe a[InProgressManufacturing])
