# rstmanager — guida per agenti

Sistema di gestione aziendale event-sourced. Tre moduli sbt con layout
bounded-context-per-package (`customer`, `hr`, `work` {order, manufacturing,
task}, `planning`).

- **`domain`** — puro (niente `IO`/I/O). Aggregati edomata, eventi, errori,
  tipi refined (iron), servizi di dominio. Non dipende mai da `service`.
- **`service`** — shell impuro: cats-effect `IO`/`Resource`, http4s + tapir,
  skunk (Postgres), backend edomata, auth Keycloak, codec, consumer/scheduler.
- **`frontend`** — Scala.js + Laminar. **Non** dipende dai moduli JVM: i DTO
  API sono replicati in `frontend/.../api/Dtos.scala` con codec circe.

Dipendenze sempre verso l'interno: `service → domain`, mai il contrario.

## Agenti dedicati (`.claude/agents/`)

Preferisci questi agenti per il lavoro sul rispettivo layer — conoscono già
stack e convenzioni del repo:

- **`scala-fp-ddd`** — domain & service: aggregati edomata
  (`DomainModel`/`Decision`/`.validate`), `App.router`/notifiche, wiring
  `XApp` (backend + registry), iron/`RefinedType`, cats-effect, http4s/tapir,
  monocle. Per progettare/implementare logica di dominio e API, o per review
  di correttezza FP/DDD.
- **`scala-laminar-frontend`** — UI Scala.js/Laminar: reattività `Var`/`Signal`,
  `AppBus` (tick per-dominio + settle tick planning), `ApiClient`, componenti
  condivisi, auth/role-gating cosmetico, `ErrorCenter`, mirror DTO. Per pagine,
  form, client API, o review di reattività e trappole Scala.js.

## Regole imposte da compiler & scalafix (tutti i moduli)

Build con `-Werror -Wunused:all -Yexplicit-nulls -Ysafe-init
-language:strictEquality`. scalafix `DisableSyntax` vieta: `null`,
`asInstanceOf`, `isInstanceOf`, `throw`, `return`, xml, default args **sui
`def`** (i default nelle case class sono ok), `final val`, val-pattern. Quindi:

- **strictEquality**: ogni tipo confrontato con `==` deve avere `derives
  CanEqual` (o un given esplicito). Un `CanEqual` che sembra "unused" è
  normale — non rimuoverlo.
- **explicit-nulls**: interop Java/JS ritorna `T | Null` → usa `.nn` al confine
  (`UUID.randomUUID().nn`, `Instant.now().nn`, `trim.nn`, `toLowerCase.nn`).
- **niente cast**: narrowing con pattern matching; per l'interop JS usa facade
  `@js.native trait` invece di `asInstanceOf`.
- Import/parametri inutilizzati sono errori. OrganizeImports:
  `javax? / scala. / altro / io.github.nicolasfara`, simboli prima.
- Formattazione scalafmt (maxColumn 150, indent 2, trailing commas sempre,
  sintassi Scala 3, `end` marker sui blocchi lunghi).

## Comandi utili

- `sbt <module>/test` — test (domain puro e veloce; frontend ha infra ScalaTest
  Scala.js).
- `sbt scalafixAll && sbt scalafmtAll` — prima di considerare finito.
- Frontend dev: `sbt ~fastLinkJS` (un terminale) + `npm run dev` (Vite, altro
  terminale) → http://localhost:3333.
- Stack completo: `docker compose up --build` (Postgres + backend + frontend
  nginx + Keycloak). Dettagli in `README.md`.

## Note

- Il package del frontend è `io.gitbub.nicolasfara.rstmanager` (grafia `gitbub`
  intenzionale — non "correggere").
- Policy di recalcolo planning: event-driven + un consolidamento giornaliero;
  mai polling periodico lato backend.
- Gli eventi persistiti e i codec JSON sono schema append-only: preserva la
  decodifica retro-compatibile quando evolvi eventi/DTO.
