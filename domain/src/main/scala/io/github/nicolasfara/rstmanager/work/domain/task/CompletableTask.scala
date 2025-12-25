package io.github.nicolasfara.rstmanager.work.domain.task

/** A task that can be completed over time. The [[expectedHours]] is snapshot of the required hours of the underlying
  * task at the time of creation. However, it can be updated later if the underlying task's required hours change via
  * the [[updateExpectedHours]].
  *
  * @param completableTaskId
  *   the unique identifier of the completable task.
  * @param taskId
  *   the unique identifier of the underlying task.
  * @param expectedHours
  *   the total number of hours expected to complete the task.
  * @param completedHours
  *   the number of hours already completed on the task.
  */
final case class CompletableTask(
    completableTaskId: CompletableTaskId,
    taskId: TaskId,
    expectedHours: Hours,
    completedHours: Hours
):
  def remainingHours: Hours = expectedHours - completedHours

  def advance(hours: Hours): CompletableTask = copy(completedHours = completedHours + hours)

  def deAdvance(hours: Hours): CompletableTask = copy(completedHours = completedHours - hours)

  def updateExpectedHours(newExpectedHours: Hours): CompletableTask = copy(expectedHours = newExpectedHours)
