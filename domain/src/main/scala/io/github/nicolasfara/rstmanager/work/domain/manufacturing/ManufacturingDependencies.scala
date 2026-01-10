package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencyError.CycleDetected
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId
import scalax.collection.edges.{DiEdge, DiEdgeImplicits}
import scalax.collection.immutable.Graph

enum ManufacturingDependencyError:
  case CycleDetected(cyclePath: Set[TaskId])

opaque type ManufacturingDependencies = Graph[TaskId, DiEdge[TaskId]]

object ManufacturingDependencies:
  def apply(edges: DiEdge[TaskId]*): ManufacturingDependencies = Graph.from(edges)

  extension (md: ManufacturingDependencies)
    /** Add dependencies for a given task in the manufacturing dependencies graph.
      * @param task
      *   The [[TaskId]] of the task to which dependencies are to be added.
      * @param dependsOn
      *   A set of [[TaskId]]s that the specified task depends on.
      * @return
      *   A new [[ManufacturingDependencies]] instance with the added dependencies.
      */
    def addTaskDependencies(task: TaskId, dependsOn: Set[TaskId]): ManufacturingDependencies =
      val newEdges = dependsOn.map(dep => DiEdge(task, dep))
      md ++ newEdges

    /** Remove a task and all its associated dependencies from the manufacturing dependencies graph.
      * @param task
      *   The [[TaskId]] of the task to be removed.
      * @return
      *   A new [[ManufacturingDependencies]] instance with the specified task and its dependencies removed.
      */
    def removeTask(task: TaskId): ManufacturingDependencies = md - task

    /** Check if the manufacturing dependencies graph contains cycles.
      * @return
      *   [[true]] if there are cycles in the graph, [[false]] otherwise.
      */
    def hasCycle: Boolean = md.isCyclic

    /** Perform a topological sort on the manufacturing dependencies graph.
      * @return
      *   An [[Either]] containing a list of [[TaskId]]s in topologically sorted order if successful, or a [[ManufacturingDependencyError]] if cycles
      *   are detected.
      */
    def sort: Either[ManufacturingDependencyError, List[TaskId]] =
      md.topologicalSort.left
        .map(error => CycleDetected(error.candidateCycleNodes.map(_.outer)))
        .map(_.map(_.outer).toList)
