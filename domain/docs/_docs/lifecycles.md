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

## Planning boundary

The planning package is currently smaller than the operational model.

- [[io.github.nicolasfara.rstmanager.planning.SchedulingService]] marks the service boundary.
- [[io.github.nicolasfara.rstmanager.planning.PlanningError]] defines the expected planning
  failures.

As planning behavior grows, this is the place where higher-level scheduling policies can stay
separate from the operational aggregates.
