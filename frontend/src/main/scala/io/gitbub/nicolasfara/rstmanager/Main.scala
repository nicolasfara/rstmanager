package io.gitbub.nicolasfara.rstmanager

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("@find/**/HelloWorld.less", JSImport.Namespace)
@js.native private object Stylesheet extends js.Object

val _ = Stylesheet // Use import to prevent DCE

@main def main(): Unit = {
  renderOnDomContentLoaded(
    container = dom.document.querySelector("#app"),
    rootNode = {
      div(
        cls("Main"),
        h1("Laminar Template"),
        HelloWorld(),
      )
    }
  )
}
