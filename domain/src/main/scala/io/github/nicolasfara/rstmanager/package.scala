package io.github.nicolasfara

/** Shared exports used across the domain module.
  *
  * The package object centralizes commonly used Cats syntax and Iron refined-type aliases so
  * domain code can import a smaller surface area.
  */
package object rstmanager:
  export cats.syntax.all.*
  export io.github.iltotore.iron.{ :|, Not }
  export io.github.iltotore.iron.constraint.all.{ Empty, Match, GreaterEqual, LessEqual }
  export io.github.iltotore.iron.cats.*
//  export io.github.iltotore.iron.constraint.all.*
