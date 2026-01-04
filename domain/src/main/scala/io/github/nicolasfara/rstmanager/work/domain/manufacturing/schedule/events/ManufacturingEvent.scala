package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.events

import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, TaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTask
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.ScheduledManufacturingId

/** Events representing state changes to ScheduledManufacturing entities within the Order aggregate.
  *
  * Important: ScheduledManufacturing is an ENTITY within the Order aggregate boundary, NOT a separate
  * aggregate root. These events are logically organized separately from OrderEvent for clarity, but they
  * are still part of the Order aggregate's event stream.
  *
  * When using these events:
  * - They should be emitted through the Order aggregate root
  * - They affect ScheduledManufacturing entities owned by Order
  * - They maintain the aggregate's consistency boundary
  * - Each event should include the manufacturingId to identify which entity changed
  *
  * These events capture manufacturing execution details and task-level changes.
  */
enum ManufacturingEvent:
  /** Manufacturing has been completed */
  case ManufacturingCompleted(manufacturingId: ScheduledManufacturingId, completionDate: DateTime)
  
  /** A task has been added to the manufacturing */
  case TaskAdded(manufacturingId: ScheduledManufacturingId, task: ScheduledTask, dependencies: Set[TaskId], addedOn: DateTime)
  
  /** A task has been removed from the manufacturing */
  case TaskRemoved(manufacturingId: ScheduledManufacturingId, taskId: TaskId, removedOn: DateTime)
  
  /** Task progress has been updated with hours worked */
  case TaskProgressUpdated(manufacturingId: ScheduledManufacturingId, taskId: TaskId, completedHours: Hours, updatedOn: DateTime)
  
  /** A task has been marked as completed */
  case TaskCompleted(manufacturingId: ScheduledManufacturingId, taskId: TaskId, completionDate: DateTime)
  
  /** Task status has changed */
  case TaskStatusChanged(manufacturingId: ScheduledManufacturingId, taskId: TaskId, newStatus: TaskStatus, changedOn: DateTime)  
