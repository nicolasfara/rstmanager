package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.auth.AuthService

/** Welcome page shown to unauthenticated visitors: the only action is the redirect to the Keycloak hosted login. */
object LandingPage:
  def apply(): HtmlElement =
    div(
      cls := "flex min-h-[70vh] items-center justify-center px-4",
      Components.card(
        cls := "w-full max-w-md p-8 text-center",
        div(cls := "mx-auto h-10 w-10 rounded-lg bg-slate-900"),
        h1(cls := "mt-4 text-xl font-semibold tracking-tight text-slate-900", "RST Manager"),
        p(
          cls := "mt-2 text-sm text-slate-600",
          "Pianificazione della produzione, ordini e anagrafiche. Accedi con il tuo account aziendale per continuare.",
        ),
        button(
          tpe := "button",
          cls := Components.btnPrimary,
          cls := "mt-6 w-full justify-center",
          "Accedi",
          onClick --> (_ => AuthService.login()),
        ),
      ),
    )
end LandingPage
