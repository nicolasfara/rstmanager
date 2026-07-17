package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.{ Signal, Var }

/** Tracks whether a mutable UI state diverged from the snapshot loaded into an editor. */
final case class DirtyTracker[A](initial: A, current: Var[A])(using CanEqual[A, A]):
  val isDirty: Signal[Boolean] =
    current.signal.map(_ != initial)

  def isDirtyNow: Boolean =
    current.now() != initial

  def changed[B](select: A => B)(using CanEqual[B, B]): Boolean =
    select(current.now()) != select(initial)
