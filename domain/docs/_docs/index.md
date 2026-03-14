# Domain Model

`rstmanager-domain` models a small production-management system with explicit domain
boundaries, immutable state transitions, and refined value objects.

This site complements the API reference by describing:

- bounded contexts
- aggregate roots
- entities
- value objects
- domain services and policies
- lifecycle state machines and events

## Core ideas

- Validation happens at the edges with `cats.data.ValidatedNec`.
- Domain primitives are refined with Iron types such as hours, names, postal codes, and codes.
- Aggregates are immutable and return a new version of themselves after each valid change.
- The work execution model uses event sourcing for [[io.github.nicolasfara.rstmanager.work.domain.order.Order]].

## Main entry points

- Customer master data starts from [[io.github.nicolasfara.rstmanager.customer.domain.Customer]].
- Workforce capacity starts from [[io.github.nicolasfara.rstmanager.hr.domain.Employee]].
- Manufacturing templates start from [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.Manufacturing]].
- Work execution and fulfillment start from [[io.github.nicolasfara.rstmanager.work.domain.order.Order]].
- Scheduling belongs to [[io.github.nicolasfara.rstmanager.planning.SchedulingService]].

## Site map

- [Bounded Contexts](bounded-contexts.html)
- [Tactical Design](tactical-design.html)
- [Lifecycles And Events](lifecycles.html)
