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

## Operational Requirements

The domain models a production-management service that plans manufacturing work, tracks
workforce capacity over time, and manages customer orders from creation to delivery.

The core planning requirement is to produce and maintain a day-by-day production schedule that
allocates manufacturing tasks to available workforce capacity and keeps customer orders on track
for their expected delivery dates.

Scheduling decisions must account for:

- the set of active customer orders
- the scheduled manufacturings required by each order
- the tasks that make up each manufacturing
- task dependencies and valid execution order
- estimated task effort and actual progress
- employee capacity, including weekly capacity and temporary overrides
- expected or promised delivery dates
- order priority

The scheduler optimizes only open orders. Suspended orders remain in the system but their
manufacturings must not appear in the schedule while the order is suspended. Cancelled orders are
excluded from scheduling because their execution has been abandoned.

The schedule must be recalculated at least once per day and whenever a relevant domain change
occurs. Relevant changes include:

- a new order is added
- an order is suspended, reactivated, completed, delivered, cancelled, or reopened
- an order delivery date or priority changes
- a manufacturing is added to or removed from an order
- a scheduled manufacturing is started, paused, completed, delayed, or cancelled
- a scheduled task is started, advanced, rolled back, completed, or reopened
- workforce capacity changes

The schedule is expressed at day granularity. For each planned day, the system must report which
tasks should be worked on and, when the task does not fit entirely in that day, how much work is
planned for that day. A task may be split across multiple days. Only one employee may work on a
given task at a time, but the assigned employee may change between scheduling slices. Tasks from
different manufacturings may overlap when dependencies and capacity allow it.

When the system must choose between competing orders, it should use this tie-break policy:

- higher order priority first, with `Urgent` before `Normal`
- earlier expected or promised delivery date first
- earlier order creation date first
- stable order identifier or order number last, only to make the result deterministic

This policy fits the current model because
[[io.github.nicolasfara.rstmanager.work.domain.order.OrderData]] already carries priority,
creation date, delivery date, and stable order identity.

When all open orders cannot be completed on time with the available capacity, the planning context
must treat the schedule as infeasible. In that case, it must apply the priority policy to decide
which orders remain planned and which orders are delayed. The same rule applies when a new order
or mid-period change makes an existing schedule infeasible.

An order is delayed when the computed delivery date is later than the expected delivery date. A
delayed order requires updating the expected or promised delivery date to the first admissible date
produced by the new schedule. A scheduled manufacturing is delayed when its computed completion
date is later than its expected completion date; in other words, the manufacturing does not fit the
schedule under the current constraints.

Planning failures and infeasible decisions must be explicit. The system must report the affected
orders, manufacturings, tasks, dates, and capacity constraints to the user, and it must leave the
domain in a recoverable state so that a later change can trigger a new planning attempt.

The system is reactive: schedule updates are derived from domain changes instead of being treated
as isolated manual edits. Operational aggregates keep execution state, while the planning context
coordinates orders, manufacturing dependencies, and workforce capacity.

There is no manual rescheduling use case at the domain level. Every planning-relevant change
triggers a new system-managed planning attempt. Planning decisions, failures, infeasible results,
reschedules, computed delays, and delivery-date changes should be persisted as events, following
the event-sourced style used by the execution model with Edomata.

## Capacity And Calendar Requirements

Administrators register employee capacity and calendar exceptions in the system. Scheduling must
consider those values when computing daily work:

- weekly employee capacity
- daily working-hours overrides
- vacation or absence intervals
- holidays and other non-working days when represented as capacity overrides

The current HR model supports weekly hours, daily overrides, and vacation intervals through
[[io.github.nicolasfara.rstmanager.hr.domain.BudgetHours]] and
[[io.github.nicolasfara.rstmanager.hr.domain.HoursOverride]].

Task effort is intentionally modeled in hours through
[[io.github.nicolasfara.rstmanager.work.domain.task.TaskHours]]. A standard full working day is
eight hours per worker, unless the employee capacity model or an override provides a different
daily availability. The day schedule should therefore allocate task hours into daily work slices
according to each candidate employee's available hours.

## Planning Error Requirements

At the domain level, planning errors must contain enough structured information to build a clear
end-user message. A planning error should identify:

- the affected order
- the affected manufacturing, when applicable
- the affected task, when applicable
- the expected deadline
- the computed feasible completion or delivery date, when available
- the missing capacity or blocking constraint
- whether the result causes an order delay, manufacturing delay, or full infeasibility

[[io.github.nicolasfara.rstmanager.planning.PlanningError]] carries the relevant identifiers,
dates, and capacity values needed to explain the scheduling result without reconstructing context
elsewhere. For example, insufficient capacity identifies the planning window, required hours,
available hours, and affected orders or manufacturings.

## Planning Model

The planning context contains a persisted schedule model built from day-level allocations:

- `DailySchedule`: the schedule for one production day.
- `ScheduledTaskSlice`: the amount of task work planned for one day.
- `CandidateEmployee`: the employee selected for a task slice, including daily availability and
  the task hours assigned by the schedule.
- `PlanningResult`: the result of a scheduling attempt, including planned days, delayed orders,
  delayed manufacturings, and planning warnings or errors.

A `ScheduledTaskSlice` should identify:

- the order
- the scheduled manufacturing
- the scheduled task
- the production day
- the assigned candidate employee
- the planned hours for that day
- the remaining task hours after the slice

The candidate employee is attached to planned task slices so the schedule can explain who is
expected to work on each piece of manufacturing work. Because an employee may change between
slices, the assignment belongs to the slice-level schedule data rather than to the task
definition.

Planning behavior is event-sourced. The planning event model includes:

- `PlanningRequested`: a domain change triggered a new scheduling attempt.
- `ScheduleComputed`: a feasible schedule was computed for the open orders.
- `ScheduleRejected`: no feasible schedule could be produced under the current constraints.
- `OrderDelayedByPlanning`: an order received a new promised delivery date.
- `ManufacturingDelayedByPlanning`: a manufacturing cannot complete by its expected date.
- `TaskSliceAssigned`: task hours were assigned to an employee for a production day.
- `PlanningWarningRaised`: a non-fatal planning issue was detected and should be shown to users.

These events should carry the same identifiers and dates exposed by planning errors, so the system
can audit how a schedule was produced and recover from failed or partial planning attempts.

## Remaining Modeling Gaps

These requirements are now decided at the business level and the planning concepts are modeled.
The remaining gaps are implementation concerns for the future scheduler:

- the scheduling algorithm that allocates open orders into task slices
- total daily capacity aggregation across all slices assigned to the same employee
- persistence and integration of planning events with the application layer
- propagation from planning delays to order promised-delivery updates

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

- [Bounded Contexts](bounded-contexts.md)
- [Tactical Design](tactical-design.md)
- [Lifecycles And Events](lifecycles.md)
