package io.gitbub.nicolasfara.rstmanager

import com.raquo.laminar.api.L.{*, given}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object HelloWorld {

  @JSImport("@find/**/HelloWorld.less", JSImport.Namespace)
  @js.native private object Stylesheet extends js.Object

  val _ = Stylesheet // Use import to prevent DCE

  def apply(): HtmlElement = {
    val nameVar = Var(initial = "world")
    div(
      cls("HelloWorld"),
      label("Your name: "),
      input(
        onMountFocus,
        placeholder := "Enter your name here",
        onInput.mapToValue --> nameVar
      ),
      div(
        cls("-greeting"),
        "Hello, ",
        text <-- nameVar.signal.map(_.toUpperCase)
      )
    )
  }
}
