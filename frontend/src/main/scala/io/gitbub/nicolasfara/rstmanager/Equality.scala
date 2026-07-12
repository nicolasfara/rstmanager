package io.gitbub.nicolasfara.rstmanager

import java.util.UUID

/** `CanEqual` instances required by `-language:strictEquality` for value types compared across the UI.
  * Imported explicitly where equality on these types is used.
  */
object Equality:
  given CanEqual[UUID, UUID] = CanEqual.derived
  given CanEqual[Option[UUID], Option[UUID]] = CanEqual.derived
