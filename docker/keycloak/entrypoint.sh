#!/bin/bash
# Renders the realm import file for the target public origin, then hands off to
# Keycloak. The committed realm (keycloak/rstmanager-realm.json) hardcodes the
# local-dev origin http://localhost:3333 in the client's rootUrl / redirectUris /
# webOrigins / post-logout URIs; here we rewrite that single origin to whatever
# the deployment exposes (RSTMANAGER_PUBLIC_APP_URL). Uses only bash builtins so
# no extra tooling (envsubst/sed) needs to be present in the Keycloak image.
set -euo pipefail

: "${RSTMANAGER_PUBLIC_APP_URL:=http://localhost:3333}"

# Ensure an explicit scheme. Railway's RAILWAY_PUBLIC_DOMAIN yields a bare host
# (no scheme); without one, the realm's redirect URIs / webOrigins are invalid.
case "${RSTMANAGER_PUBLIC_APP_URL}" in
  http://* | https://*) : ;;
  *) RSTMANAGER_PUBLIC_APP_URL="https://${RSTMANAGER_PUBLIC_APP_URL}" ;;
esac

# KC_HOSTNAME accepts a bare hostname OR a full URL; a host WITH a path but no
# scheme (e.g. "example.com/auth") is rejected by Keycloak at startup. Add the
# scheme in that case so a schemeless value from RAILWAY_PUBLIC_DOMAIN + /auth
# still starts.
if [ -n "${KC_HOSTNAME:-}" ]; then
  case "${KC_HOSTNAME}" in
    http://* | https://*) : ;;
    */*) export KC_HOSTNAME="https://${KC_HOSTNAME}" ;;
    *) : ;; # bare hostname, valid as-is
  esac
fi

src="/opt/keycloak/data/import-template/rstmanager-realm.json"
dst_dir="/opt/keycloak/data/import"
dst="${dst_dir}/rstmanager-realm.json"

mkdir -p "${dst_dir}"
content="$(cat "${src}")"
content="${content//http:\/\/localhost:3333/${RSTMANAGER_PUBLIC_APP_URL}}"
printf '%s\n' "${content}" > "${dst}"

echo "[rstmanager] realm import rendered for origin: ${RSTMANAGER_PUBLIC_APP_URL}"
echo "[rstmanager] KC_HOSTNAME: ${KC_HOSTNAME:-<unset>}"

exec /opt/keycloak/bin/kc.sh "$@"
