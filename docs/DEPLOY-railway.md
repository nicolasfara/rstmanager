# Deploy su Railway

Guida per portare rstmanager su [Railway](https://railway.com). Lo stack
`docker-compose` non si trasferisce così com'è: Railway deploya **un servizio per
immagine** e i servizi si parlano via **private networking solo IPv6**
(`<nome>.railway.internal`). Il design same-origin dell'app (nginx unico
entrypoint che proxa `/api`, `/auth`, `/docs`) è però ideale per Railway e viene
preservato.

## Topologia

Un progetto Railway con quattro servizi, **un solo dominio pubblico** (il
frontend); backend e Keycloak restano privati.

```
                Internet (HTTPS, TLS all'edge Railway)
                              │
                     ┌────────▼─────────┐   dominio pubblico
                     │  frontend (nginx)│   es. rstmanager.up.railway.app
                     │  Dockerfile.frontend
                     └───┬───────────┬──┘
          /api  /docs    │           │   /auth
     (IPv6 privato)      │           │  (IPv6 privato)
            ┌────────────▼──┐   ┌────▼──────────────┐
            │ planning-     │   │ keycloak          │
            │ service       │   │ Dockerfile.keycloak
            │ Dockerfile.   │   └────┬──────────────┘
            │ service       │        │
            └──────┬────────┘        │
                   │  (IPv6 privato) │
              ┌────▼─────────────────▼────┐
              │  Postgres (plugin managed)│
              │  db "railway" + db "keycloak"
              └───────────────────────────┘
```

## ⚠️ I due vincoli Railway che rompono tutto se ignorati

1. **Private networking è solo IPv6.** Un processo che ascolta solo su
   `0.0.0.0` (IPv4) **non riceve** traffico privato. Backend e Keycloak devono
   bindare `::` (vedi `RSTMANAGER_HTTP_HOST=::` e `KC_HTTP_HOST=::` sotto). Sulla
   JVM `::` è dual-stack, quindi accetta anche IPv4 — nessun altro effetto.
2. **Gli upstream nginx devono usare il nome privato completo**
   `*.railway.internal`, non il nome corto. Per questo sul servizio frontend si
   impostano `RSTMANAGER_API_UPSTREAM` e `RSTMANAGER_KC_UPSTREAM` con il suffisso
   `.railway.internal` (i default corti valgono solo per docker compose).

> **Naming dei servizi:** chiama i servizi Railway **`planning-service`**,
> **`keycloak`** e **`frontend`**. I valori degli upstream e degli URL interni
> qui sotto assumono questi nomi. Se usi nomi diversi, adegua di conseguenza gli
> hostname `.railway.internal`.

## Cosa c'è già nel repo

| File | Ruolo |
|------|-------|
| `Dockerfile.service` | Immagine backend (invariata) |
| `Dockerfile.frontend` | Immagine nginx; il conf viene reso da template all'avvio |
| `Dockerfile.keycloak` | Keycloak + realm bakato, origin riscritto all'avvio |
| `nginx.frontend.conf.template` | Conf nginx parametrizzato (porta/upstream/resolver) |
| `docker/nginx/10-rstmanager-resolver.envsh` | Calcola resolver IPv6/IPv4 e default |
| `docker/keycloak/entrypoint.sh` | Riscrive l'origin pubblico nel realm import |
| `railway.service.json` / `railway.frontend.json` / `railway.keycloak.json` | Config-as-code per servizio |

Nessuna modifica al codice Scala: il frontend è già same-origin
(`window.location.origin + "/auth"`, `baseUrl = "/api/v1"`) e il backend legge
host/porta DB e config Keycloak da env.

## Procedura passo-passo

### 1. Progetto + Postgres

1. Crea un nuovo progetto Railway (consigliato: collegalo a questo repo GitHub).
2. **New → Database → Add PostgreSQL.** Rinomina il servizio `Postgres`.

### 2. Crea il database `keycloak` nella stessa istanza Postgres

Il plugin espone un solo database (`railway`); il tuo init-script locale non
gira su Railway. Crea a mano DB e utente per Keycloak, una tantum. Con la CLI:

```bash
railway link         # seleziona il progetto
railway connect Postgres   # apre psql sull'istanza
```

poi in psql:

```sql
CREATE USER keycloak WITH PASSWORD '<scegli-una-password>';
CREATE DATABASE keycloak OWNER keycloak;
```

(In alternativa: tab **Data** del servizio Postgres → Query.)

### 3. Servizio `frontend` e dominio pubblico

Crealo per primo così ottieni subito il dominio a cui puntano Keycloak e il
backend.

1. **New → GitHub Repo** (lo stesso repo) → nome servizio `frontend`.
2. **Settings → Config-as-code** → path `railway.frontend.json`.
3. **Settings → Networking → Generate Domain.** Annota l'URL, es.
   `https://rstmanager-frontend-production.up.railway.app`. Chiamiamolo
   **`<APP_URL>`**. Il suo path auth è **`<APP_URL>/auth`** = **`<AUTH_URL>`**.

   > ⚠️ **Includi sempre lo schema `https://`.** La variabile Railway
   > `RAILWAY_PUBLIC_DOMAIN` restituisce il **solo dominio** (senza schema):
   > usata nuda in `KC_HOSTNAME` fa fallire l'avvio di Keycloak con *"Provided
   > hostname is neither a plain hostname nor a valid URL"*. Se vuoi la reference
   > variable, scrivila come `https://${{frontend.RAILWAY_PUBLIC_DOMAIN}}`.
   > (L'entrypoint Keycloak aggiunge comunque `https://` in mancanza di schema,
   > ma è meglio impostarlo esplicitamente.)
4. Variabili (le patchi tra poco quando i nomi interni esistono):

   | Variabile | Valore |
   |-----------|--------|
   | `RSTMANAGER_API_UPSTREAM` | `planning-service.railway.internal:8080` |
   | `RSTMANAGER_KC_UPSTREAM` | `keycloak.railway.internal:8080` |

   Il primo deploy fallirà l'health finché il backend non è su: è atteso.

### 4. Servizio `keycloak`

1. **New → GitHub Repo** → nome `keycloak`.
2. **Settings → Config-as-code** → `railway.keycloak.json`.
3. Variabili:

   | Variabile | Valore |
   |-----------|--------|
   | `KC_DB` | `postgres` |
   | `KC_DB_URL` | `jdbc:postgresql://${{Postgres.PGHOST}}:${{Postgres.PGPORT}}/keycloak` |
   | `KC_DB_USERNAME` | `keycloak` |
   | `KC_DB_PASSWORD` | la password scelta al passo 2 |
   | `KC_HTTP_ENABLED` | `true` |
   | `KC_HTTP_HOST` | `::` &nbsp;← **bind IPv6 per il private networking** |
   | `KC_HTTP_PORT` | `8080` |
   | `KC_HTTP_RELATIVE_PATH` | `/auth` |
   | `KC_HOSTNAME` | `<AUTH_URL>` &nbsp;(es. `https://…up.railway.app/auth`) |
   | `KC_PROXY_HEADERS` | `xforwarded` |
   | `KC_HEALTH_ENABLED` | `true` |
   | `KC_BOOTSTRAP_ADMIN_USERNAME` | scegli (non `admin` in prod) |
   | `KC_BOOTSTRAP_ADMIN_PASSWORD` | scegli una password robusta |
   | `RSTMANAGER_PUBLIC_APP_URL` | `<APP_URL>` &nbsp;(senza `/auth`) |
   | `RSTMANAGER_EXTRA_APP_URLS` | *(opz.)* origin aggiuntivi, separati da virgola/spazio |

   `RSTMANAGER_PUBLIC_APP_URL` è l'origin **primario** con cui l'entrypoint
   riscrive `rootUrl` / redirect URIs / webOrigins / post-logout del client nel
   realm importato. `RSTMANAGER_EXTRA_APP_URLS` aggiunge **altri origin** a
   redirect URIs / webOrigins / post-logout (non a `rootUrl`): serve per tenere
   valido il vecchio dominio Railway accanto a un custom domain, così il cambio
   dominio non è un flag day. Ogni origin senza schema viene forzato a `https://`.
   **Non** dare un dominio pubblico a questo servizio: resta privato.

   > **Custom domain:** se il custom domain non è tra gli origin del client,
   > l'iframe silent-SSO di keycloak-js non torna mai indietro e la SPA resta
   > appesa sullo **spinner di caricamento** (né login né app). Metti il custom
   > domain in `RSTMANAGER_PUBLIC_APP_URL` (e in `KC_HOSTNAME` +
   > `RSTMANAGER_KEYCLOAK_ISSUER` del backend), tenendo l'eventuale vecchio
   > dominio in `RSTMANAGER_EXTRA_APP_URLS`.

   > **Immagine optimized:** `Dockerfile.keycloak` esegue `kc.sh build` a build
   > time bakando le opzioni **build-time** `KC_DB=postgres`,
   > `KC_HEALTH_ENABLED=true`, `KC_HTTP_RELATIVE_PATH=/auth`, e a runtime lancia
   > `start --optimized`. Così l'augmentation Quarkus non gira all'avvio (era la
   > causa dell'OOM `Killed` sul default da 512MB, e resta vicina al tetto anche
   > a 1GB). Le tre variabili qui sopra restano impostate con gli **stessi
   > valori** — combaciano col build, nessun warning; se un domani ne cambi una,
   > serve rifare il build dell'immagine.

### 5. Servizio `planning-service`

1. **New → GitHub Repo** → nome `planning-service`.
2. **Settings → Config-as-code** → `railway.service.json`.
3. Variabili:

   | Variabile | Valore |
   |-----------|--------|
   | `RSTMANAGER_HTTP_HOST` | `::` &nbsp;← **bind IPv6 per il private networking** |
   | `RSTMANAGER_HTTP_PORT` | `8080` |
   | `RSTMANAGER_DB_HOST` | `${{Postgres.PGHOST}}` |
   | `RSTMANAGER_DB_PORT` | `${{Postgres.PGPORT}}` |
   | `RSTMANAGER_DB_NAME` | `${{Postgres.PGDATABASE}}` |
   | `RSTMANAGER_DB_USER` | `${{Postgres.PGUSER}}` |
   | `RSTMANAGER_DB_PASSWORD` | `${{Postgres.PGPASSWORD}}` |
   | `RSTMANAGER_DB_POOL_SIZE` | `4` |
   | `RSTMANAGER_KEYCLOAK_INTERNAL_URL` | `http://keycloak.railway.internal:8080/auth` |
   | `RSTMANAGER_KEYCLOAK_REALM` | `rstmanager` |
   | `RSTMANAGER_KEYCLOAK_ISSUER` | `<AUTH_URL>/realms/rstmanager` |
   | `RSTMANAGER_KEYCLOAK_CLIENT_ID` | `rstmanager-frontend` |

   Il backend fetcha le JWKS dall'URL **interno** ma valida l'issuer **esterno**
   (`<AUTH_URL>/realms/rstmanager`): devono combaciare esattamente col dominio
   pubblico del frontend, altrimenti tutti 401.

### 6. Ordine di avvio e redeploy

1. Verifica che `Postgres` sia up e che il DB `keycloak` esista (passo 2).
2. Deploya/redeploya `keycloak` → attendi che i log mostrino
   `Listening on: http://[::]:8080` e l'import del realm.
3. Deploya/redeploya `planning-service` → health `/api/v1/health` verde.
4. Redeploya `frontend` → health `/` verde.

### 7. Utenti e ruoli

Il realm non contiene utenti. Vai su `<APP_URL>/auth/admin`, login con le
credenziali bootstrap, crea gli utenti e assegna i realm role `viewer` /
`operator` / `admin` (gerarchici — vedi `README.md`).

## Verifica

```bash
curl -sf <APP_URL>/api/v1/health        # backend via nginx → 200
curl -sf <APP_URL>/auth/realms/rstmanager/.well-known/openid-configuration | jq .issuer
#   deve stampare esattamente "<AUTH_URL>/realms/rstmanager"
```

Poi apri `<APP_URL>`, fai login: il redirect a Keycloak e il ritorno con token
devono funzionare same-origin.

## Troubleshooting

| Sintomo | Causa probabile |
|---------|-----------------|
| nginx 502 su `/api` o `/auth` | Backend/Keycloak non bindano `::` (IPv6), o upstream senza suffisso `.railway.internal` |
| 401 su tutte le `/api/v1/*` | `RSTMANAGER_KEYCLOAK_ISSUER` ≠ issuer reale del token; controlla che `KC_HOSTNAME` = `<AUTH_URL>` |
| Redirect di login su `localhost:3333` | `RSTMANAGER_PUBLIC_APP_URL` non impostata **al primo import** del realm; aggiornala e ri-crea il realm (o correggi il client nell'admin console — l'import salta se il realm esiste già) |
| SPA appesa sullo **spinner** (né login né app) su custom domain | Il custom domain non è tra i redirect URIs/webOrigins del client → l'iframe silent-SSO non completa e `keycloak.init()` resta pendente. Aggiungi il dominio a `RSTMANAGER_PUBLIC_APP_URL` / `RSTMANAGER_EXTRA_APP_URLS` e allinea `KC_HOSTNAME`; se il realm esiste già, correggi il client nell'admin console (vedi sotto) |
| Keycloak "insecure"/mixed content | Manca `KC_PROXY_HEADERS=xforwarded`; nginx già inoltra `X-Forwarded-Proto` reale |
| nginx non risolve gli upstream | Il resolver viene letto da `/etc/resolv.conf` all'avvio; su Railway è IPv6 e lo script forza `ipv6=on` — se cambi immagine base verifica che resolv.conf sia presente |
| Cambio dominio pubblico | Aggiorna `KC_HOSTNAME`, `RSTMANAGER_PUBLIC_APP_URL` (+ `RSTMANAGER_EXTRA_APP_URLS` per tenere il vecchio dominio), `RSTMANAGER_KEYCLOAK_ISSUER` e i redirect URIs del client. ⚠️ Le variabili d'origine sono lette **solo al primo import** del realm: se il realm esiste già in Postgres, `--import-realm` lo **salta** e devi aggiornare il client `rstmanager-frontend` a mano nell'admin console (Valid redirect URIs → `https://dominio/*`, Web origins, Root URL, Post logout) oppure forzare il re-import |
| Keycloak `Killed` in avvio (OOM sullo step `build-and-exit`) | `Dockerfile.keycloak` fa `kc.sh build` a build time e `start --optimized` a runtime, così l'augmentation non gira all'avvio. Se persiste, aumenta la RAM del servizio Keycloak su Railway |

## Costi/note

Keycloak è il servizio più pesante (deve stare sempre acceso, ~500 MB RAM, DB
dedicato). Se in futuro vuoi alleggerire, valuta un IdP gestito, ma richiede di
ricablare issuer/JWKS del backend.
