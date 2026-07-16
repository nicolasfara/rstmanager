package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderDependencyError.CycleDetected

import scalax.collection.edges.DiEdge
import scalax.collection.immutable.Graph

/** Errors returned when operating on an order-level manufacturing dependency graph. */
enum OrderDependencyError:
  case CycleDetected(cyclePath: Set[ScheduledManufacturingId])

/** Directed graph describing ordering constraints between the manufacturings of an order. */
opaque type OrderDependencies = Graph[ScheduledManufacturingId, DiEdge[ScheduledManufacturingId]]

object OrderDependencies:
  /** Creates a dependency graph from a sequence of directed edges. */
  def apply(edges: DiEdge[ScheduledManufacturingId]*): OrderDependencies = Graph.from(edges)

  /** The empty dependency graph. */
  val empty: OrderDependencies = Graph.empty

  /** Rebuilds a dependency graph from directed `(source, target)` edge pairs. Used to persist and restore the graph. */
  def fromEdgePairs(pairs: List[(ScheduledManufacturingId, ScheduledManufacturingId)]): OrderDependencies =
    Graph.from(pairs.map((source, target) => DiEdge(source, target)))

  extension (od: OrderDependencies)
    /**
     * Adds dependency edges for a manufacturing.
     *
     * @param manufacturing
     *   Manufacturing that depends on the provided predecessors.
     * @param dependsOn
     *   Manufacturings that must be completed before `manufacturing`.
     */
    def addManufacturingDependencies(
        manufacturing: ScheduledManufacturingId,
        dependsOn: Set[ScheduledManufacturingId],
    ): OrderDependencies =
      val newEdges = dependsOn.map(dep => DiEdge(manufacturing, dep))
      od ++ newEdges

    /** Removes a manufacturing and all related dependency edges. */
    def removeManufacturing(manufacturing: ScheduledManufacturingId): OrderDependencies = od - manufacturing

    /** Returns the directed dependency edges as `(source, target)` pairs. Used to persist and restore the graph. */
    def toEdgePairs: List[(ScheduledManufacturingId, ScheduledManufacturingId)] =
      od.edges.toList.map(edge => (edge.outer.source, edge.outer.target))

    /** Returns the direct prerequisites of a manufacturing, or an empty set when it has no registered dependencies. */
    def dependenciesOf(manufacturing: ScheduledManufacturingId): Set[ScheduledManufacturingId] =
      od.find(manufacturing).map(_.diSuccessors.map(_.outer).toSet).getOrElse(Set.empty)

    /** Returns every manufacturing id referenced by the graph (as dependent or prerequisite). */
    def referencedManufacturingIds: Set[ScheduledManufacturingId] = od.nodes.map(_.outer).toSet

    /** Returns `true` when the dependency graph contains at least one cycle. */
    def hasCycle: Boolean = od.isCyclic

    /**
     * Produces a topological ordering of the manufacturings when the graph is acyclic.
     *
     * Returns `OrderDependencyError.CycleDetected` when a cycle prevents sorting.
     */
    def sort: Either[OrderDependencyError, List[ScheduledManufacturingId]] =
      od.topologicalSort.left
        .map(error => CycleDetected(error.candidateCycleNodes.map(_.outer)))
        .map(_.map(_.outer).toList)
  end extension
end OrderDependencies
