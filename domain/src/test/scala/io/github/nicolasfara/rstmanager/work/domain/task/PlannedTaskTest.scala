package io.github.nicolasfara.rstmanager.work.domain.task

import cats.syntax.all.*
import io.github.iltotore.iron.*
import com.github.nscala_time.time.Imports.*
import io.github.nicolasfara.rstmanager.work.domain.task.completable.{CompletableTask, CompletableTaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.SchedulableTask
import org.scalatest.flatspec.AnyFlatSpecLike

import java.util.UUID

class PlannedTaskTest extends AnyFlatSpecLike:
  "A PlannedTask" should "have an ordering based on delivery date, priority, and hours" in:
    val task1 = SchedulableTask(
      CompletableTask(CompletableTaskId(UUID.randomUUID()), TaskId(UUID.randomUUID()), Hours(5), Hours(2)),
      TaskPriority.High,
      DateTime.now() + 2.days
    )

    val task2 = SchedulableTask(
      CompletableTask(CompletableTaskId(UUID.randomUUID()), TaskId(UUID.randomUUID()), Hours(8), Hours(3)),
      TaskPriority.Medium,
      DateTime.now() + 1.day
    )

    val task3 = SchedulableTask(
      CompletableTask(CompletableTaskId(UUID.randomUUID()), TaskId(UUID.randomUUID()), Hours(6), Hours(1)),
      TaskPriority.High,
      DateTime.now() + 2.days
    )

    assert(task2 < task1) // Earlier deadline
    assert(task1 < task3) // Same deadline, higher priority
    assert(task2 < task3) // Earlier deadline
