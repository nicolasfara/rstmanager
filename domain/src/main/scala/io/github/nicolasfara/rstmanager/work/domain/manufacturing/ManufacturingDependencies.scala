package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.task.TaskId

opaque type ManufacturingDependencies = Map[TaskId, Set[TaskId]]

object ManufacturingDependencies:
  def apply(value: Map[TaskId, Set[TaskId]]): ManufacturingDependencies = value
  def containsCycles(dependencies: Map[TaskId, Set[TaskId]]): Boolean =
    def visit(node: TaskId, visited: Set[TaskId], recStack: Set[TaskId]): Boolean =
      if recStack.contains(node) then true
      else if visited.contains(node) then false
      else
        val newVisited = visited + node
        val newRecStack = recStack + node
        dependencies.getOrElse(node, Set()).exists(neighbor => visit(neighbor, newVisited, newRecStack))
    dependencies.keys.exists(node => visit(node, Set(), Set()))

  def topologicalSort(dependencies: ManufacturingDependencies): Either[String, List[TaskId]] =
    if containsCycles(dependencies) then Left("The dependency graph contains cycles.")
    else
      def visit(node: TaskId, visited: Set[TaskId], stack: List[TaskId]): (Set[TaskId], List[TaskId]) =
        if visited.contains(node) then (visited, stack)
        else
          val newVisited = visited + node
          val (finalVisited, finalStack) = dependencies.getOrElse(node, Set()).foldLeft((newVisited, stack)) {
            case ((v, s), neighbor) => visit(neighbor, v, s)
          }
          (finalVisited, node :: finalStack)
      val (_, sortedList) = dependencies.keys.foldLeft((Set.empty[TaskId], List.empty[TaskId])) { case ((v, s), node) =>
        visit(node, v, s)
      }
      Right(sortedList.reverse)

  extension (md: ManufacturingDependencies)
    def addTaskDependency(task: TaskId, dependsOn: Set[TaskId]): ManufacturingDependencies =
      val updated = md.get(task) match
        case Some(existing) => existing ++ dependsOn
        case None           => dependsOn
      ManufacturingDependencies(md + (task -> updated))
    def removeTaskDependency(task: TaskId): ManufacturingDependencies =
      ManufacturingDependencies(md - task)
