package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencyError.CycleDetected
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId

import scalax.collection.edges.{ DiEdge, DiEdgeImplicits }
import scalax.collection.immutable.Graph

/** Errors returned when operating on a manufacturing dependency graph. */
enum ManufacturingDependencyError:
  case CycleDetected(cyclePath: Set[TaskId])

/** Directed graph describing ordering constraints between tasks in a manufacturing. */
opaque type ManufacturingDependencies = Graph[TaskId, DiEdge[TaskId]]

object ManufacturingDependencies:
  /** Creates a dependency graph from a sequence of directed edges. */
  def apply(edges: DiEdge[TaskId]*): ManufacturingDependencies = Graph.from(edges)

  extension (md: ManufacturingDependencies)
    /** Adds dependency edges for a task.
      *
      * @param task
      *   Task that depends on the provided predecessors.
      * @param dependsOn
      *   Tasks that must be completed before `task`.
      */
    def addTaskDependencies(task: TaskId, dependsOn: Set[TaskId]): ManufacturingDependencies =
      val newEdges = dependsOn.map(dep => DiEdge(task, dep))
      md ++ newEdges

    /** Removes a task and all related dependency edges. */
    def removeTask(task: TaskId): ManufacturingDependencies = md - task

    /** Returns `true` when the dependency graph contains at least one cycle. */
    def hasCycle: Boolean = md.isCyclic

    /** Produces a topological ordering for the tasks when the graph is acyclic.
      *
      * Returns `ManufacturingDependencyError.CycleDetected` when a cycle prevents sorting.
      */
    def sort: Either[ManufacturingDependencyError, List[TaskId]] =
      md.topologicalSort.left
        .map(error => CycleDetected(error.candidateCycleNodes.map(_.outer)))
        .map(_.map(_.outer).toList)
  end extension
end ManufacturingDependencies
