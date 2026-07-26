#!/bin/bash
# Renders the realm import file for the target public origin, then hands off to
# Keycloak. The committed realm (keycloak/rstmanager-realm.json) hardcodes the
# local-dev origin http://localhost:3333 in the client's rootUrl / redirectUris /
# webOrigins / post-logout URIs; here we rewrite that single origin to whatever
# the deployment exposes (RSTMANAGER_PUBLIC_APP_URL). Uses only bash builtins so
# no extra tooling (envsubst/sed) needs to be present in the Keycloak image.
set -euo pipefail

: "${RSTMANAGER_PUBLIC_APP_URL:=http://localhost:3333}"

src="/opt/keycloak/data/import-template/rstmanager-realm.json"
dst_dir="/opt/keycloak/data/import"
dst="${dst_dir}/rstmanager-realm.json"

mkdir -p "${dst_dir}"
content="$(cat "${src}")"
content="${content//http:\/\/localhost:3333/${RSTMANAGER_PUBLIC_APP_URL}}"
printf '%s\n' "${content}" > "${dst}"

echo "[rstmanager] realm import rendered for origin: ${RSTMANAGER_PUBLIC_APP_URL}"

exec /opt/keycloak/bin/kc.sh "$@"
