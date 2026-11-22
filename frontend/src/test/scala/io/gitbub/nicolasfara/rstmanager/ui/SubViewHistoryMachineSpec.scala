package io.gitbub.nicolasfara.rstmanager.ui

import io.gitbub.nicolasfara.rstmanager.ui.SubViewHistory.Machine
import io.gitbub.nicolasfara.rstmanager.ui.SubViewHistory.Machine.{ Action, Event, State }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the history entry of a form page: browser back → guarded leave, and our own `back()` never handled twice. */
final class SubViewHistoryMachineSpec extends AnyFunSuite with Matchers:

  /** Runs `events` from `from`, returning the final state and the actions of each step. */
  private def run(from: State, events: Event*): (State, List[List[Action]]) =
    events.foldLeft((from, List.empty[List[Action]])) { case ((state, acc), event) =>
      val (next, actions) = Machine.step(state, event)
      (next, acc :+ actions)
    }

  private val mounted: State = Machine.step(Machine.initial, Event.Mounted)._1

  test("mounting a page pushes one entry"):
    Machine.step(Machine.initial, Event.Mounted) shouldBe (State(1, true, 0), List(Action.Push))

  test("a pop while mounted pushes the entry again and runs the guarded leave"):
    Machine.step(mounted, Event.Popped) shouldBe (State(1, true, 0), List(Action.Push, Action.RunBack))

  test("a pop while dirty keeps the entry, so the next back asks again"):
    // "Continua a modificare": no unmount follows the pop.
    val (state, actions) = run(mounted, Event.Popped, Event.Popped)
    actions shouldBe List(List(Action.Push, Action.RunBack), List(Action.Push, Action.RunBack))
    state shouldBe State(1, true, 0)

  test("unmounting drops the entry once, and our own back() pop is ignored"):
    val (state, actions) = run(mounted, Event.Unmounted, Event.Settled, Event.Popped)
    actions shouldBe List(Nil, List(Action.Back), List(Action.Ignore))
    state shouldBe Machine.initial

  test("leaving through the browser back: one back() for the re-pushed entry, its pop ignored"):
    val (state, actions) = run(mounted, Event.Popped, Event.Unmounted, Event.Settled, Event.Popped)
    actions shouldBe List(List(Action.Push, Action.RunBack), Nil, List(Action.Back), List(Action.Ignore))
    state shouldBe Machine.initial

  test("an external pop after the unmount does not call back() a second time"):
    val (state, actions) = run(mounted, Event.Unmounted, Event.Popped, Event.Settled)
    actions shouldBe List(Nil, Nil, Nil)
    state shouldBe Machine.initial

  test("a page replaced in the same update takes the entry over: no back(), no second push"):
    val (state, actions) = run(mounted, Event.Unmounted, Event.Mounted, Event.Settled)
    actions shouldBe List(Nil, Nil, Nil)
    state shouldBe State(1, true, 0)

  test("a pop with no page mounted and no back() pending does nothing"):
    Machine.step(Machine.initial, Event.Popped) shouldBe (Machine.initial, Nil)

  test("a settle with no entry does nothing"):
    Machine.step(Machine.initial, Event.Settled) shouldBe (Machine.initial, Nil)
end SubViewHistoryMachineSpec
