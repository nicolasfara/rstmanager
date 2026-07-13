package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js.timers.setTimeout

import com.raquo.laminar.api.L.*

/**
 * Global reactive refresh bus.
 *
 * Every page loads its remote data off [[ticks]] and reports each successful mutation through [[mutated]]. Because the bus is app-wide, a change made
 * on one page is picked up by every other mounted (or later-mounted) page without manual refresh buttons.
 *
 * A mutation on orders or employees also triggers an asynchronous planning recalculation on the backend (outbox consumer), so a single immediate
 * re-fetch would race it: [[mutated]] therefore re-emits a couple of delayed ticks to converge on the recomputed plan. Reloads are in-place (see
 * `Components.loadable`), so these extra ticks are visually silent.
 */
object AppBus:

  /** Delays (ms) of the extra refreshes fired after a mutation, to catch the async plan recalculation once it has settled. */
  private val settleDelaysMs = List(1200, 3200)

  private val version = Var(0)

  /** Signal every page's `loadable` hangs off: bumping it reloads all mounted remote data. */
  val ticks: Signal[Int] = version.signal

  /** Reloads all mounted remote data right away (used by polling; does not schedule settle refreshes). */
  def refresh(): Unit = version.update(_ + 1)

  /** Reports a successful mutation: refreshes immediately and again after the backend's async plan recalculation has settled. */
  def mutated(): Unit =
    refresh()
    settleDelaysMs.foreach(delay => setTimeout(delay)(refresh()))
end AppBus
