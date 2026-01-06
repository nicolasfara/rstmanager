package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTaskId

opaque type ManufacturingDependencies = Map[ScheduledTaskId, Set[ScheduledTaskId]]

object ManufacturingDependencies:
  def apply(value: Map[ScheduledTaskId, Set[ScheduledTaskId]]): ManufacturingDependencies = value
  def containsCycles(dependencies: Map[ScheduledTaskId, Set[ScheduledTaskId]]): Boolean =
    def visit(node: ScheduledTaskId, visited: Set[ScheduledTaskId], recStack: Set[ScheduledTaskId]): Boolean =
      if recStack.contains(node) then true
      else if visited.contains(node) then false
      else
        val newVisited = visited + node
        val newRecStack = recStack + node
        dependencies.getOrElse(node, Set()).exists(neighbor => visit(neighbor, newVisited, newRecStack))
    dependencies.keys.exists(node => visit(node, Set(), Set()))

  def topologicalSort(dependencies: ManufacturingDependencies): Either[String, List[ScheduledTaskId]] =
    if containsCycles(dependencies) then Left("The dependency graph contains cycles.")
    else
      def visit(
          node: ScheduledTaskId,
          visited: Set[ScheduledTaskId],
          stack: List[ScheduledTaskId]
      ): (Set[ScheduledTaskId], List[ScheduledTaskId]) =
        if visited.contains(node) then (visited, stack)
        else
          val newVisited = visited + node
          val (finalVisited, finalStack) = dependencies.getOrElse(node, Set()).foldLeft((newVisited, stack)) { case ((v, s), neighbor) =>
            visit(neighbor, v, s)
          }
          (finalVisited, node :: finalStack)
      val (_, sortedList) = dependencies.keys.foldLeft((Set.empty[ScheduledTaskId], List.empty[ScheduledTaskId])) { case ((v, s), node) =>
        visit(node, v, s)
      }
      Right(sortedList.reverse)

  extension (md: ManufacturingDependencies)
    def setDependency(task: ScheduledTaskId, dependsOn: Set[ScheduledTaskId]): ManufacturingDependencies =
      val updated = md.get(task) match
        case Some(existing) => existing ++ dependsOn
        case None           => dependsOn
      ManufacturingDependencies(md + (task -> updated))
    def removeTaskDependency(task: ScheduledTaskId): ManufacturingDependencies =
      ManufacturingDependencies(md - task)
