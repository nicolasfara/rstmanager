# Lifecycles And Events

The execution side of the model is built around explicit state machines.

## Order lifecycle

[[io.github.nicolasfara.rstmanager.work.domain.order.Order]] moves through these states:

- `NewOrder`
- `InProgressOrder`
- `SuspendedOrder`
- `CompletedOrder`
- `DeliveredOrder`
- `CancelledOrder`

Commands such as `create`, `suspend`, `reactivate`, `complete`, `deliver`, and `cancel`
emit [[io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent]] values and
validate the resulting state.

This makes `Order` the main consistency boundary for execution and fulfillment.

## Scheduled manufacturing lifecycle

[[io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing]]
tracks execution progress for a single manufacturing attached to an order.

States:

- `NotStartedManufacturing`
- `InProgressManufacturing`
- `PausedManufacturing`
- `CompletedManufacturing`

The aggregate updates scheduled tasks and can automatically transition to completed when all
tasks are done.

## Scheduled task lifecycle

[[io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask]] uses a smaller
state machine:

- `PendingTask`
- `InProgressTask`
- `CompletedTask`

Progress changes are explicit:

- `markAsInProgress`
- `advanceInProgressTask`
- `rollbackInProgressTask`
- `completeTask`
- `revertToInProgress`

## Events and errors

The work execution model keeps successful changes and failed decisions explicit.

- Successful changes are represented by
  [[io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent]].
- Order-level failures are represented by
  [[io.github.nicolasfara.rstmanager.work.domain.order.OrderError]].
- Nested manufacturing and task failures are represented by
  [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingError]]
  and [[io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskError]].

## Planning lifecycle

[[io.github.nicolasfara.rstmanager.planning.Planning]] records one scheduling attempt as an
event-sourced aggregate.

States:

- `NewPlanning`
- `InProgressPlanning`
- `CompletedPlanning`
- `RejectedPlanning`

The aggregate accepts a [[io.github.nicolasfara.rstmanager.planning.PlanningRequest]], records
task slices, delays, and warnings, and then either computes a
[[io.github.nicolasfara.rstmanager.planning.PlanningResult]] from the accepted facts or rejects
the attempt with structured [[io.github.nicolasfara.rstmanager.planning.PlanningError]] values.

Planning events are represented by
[[io.github.nicolasfara.rstmanager.planning.events.PlanningEvent]].

[[io.github.nicolasfara.rstmanager.planning.SchedulingService]] implements the scheduling
algorithm: it walks the Monday-to-Friday production days of the window, allocates each task
to the available employee hours in priority and dependency order, and reports delays,
warnings, or structured errors. [[io.github.nicolasfara.rstmanager.planning.PlanningService]]
wires that algorithm to the `Planning` aggregate as an edomata service: one `ComputePlan`
command starts the attempt, records every produced fact as planning events, and completes or
rejects the attempt, publishing notifications for downstream contexts. Neither service mutates
operational aggregates directly.
