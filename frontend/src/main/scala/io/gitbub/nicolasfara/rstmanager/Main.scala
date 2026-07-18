package io.gitbub.nicolasfara.rstmanager

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.auth.AuthService
import io.gitbub.nicolasfara.rstmanager.ui.App
import org.scalajs.dom

@main def main(): Unit =
  AuthService.init()
  renderOnDomContentLoaded(dom.document.querySelector("#app"), App())
