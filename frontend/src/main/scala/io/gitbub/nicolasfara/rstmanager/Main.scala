package io.gitbub.nicolasfara.rstmanager

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.ui.App
import org.scalajs.dom

@main def main(): Unit =
  renderOnDomContentLoaded(dom.document.querySelector("#app"), App())
