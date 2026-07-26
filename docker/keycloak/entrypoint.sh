#!/bin/bash
# Renders the realm import file for the target public origin(s), then hands off
# to Keycloak. The committed realm (keycloak/rstmanager-realm.json) hardcodes the
# local-dev origin http://localhost:3333 in the client's rootUrl / redirectUris /
# webOrigins / post-logout URIs; here we rewrite that single origin to whatever
# the deployment exposes.
#
#   RSTMANAGER_PUBLIC_APP_URL   primary public origin (drives rootUrl + issuer
#                               expectations); the browser-visible base URL.
#   RSTMANAGER_EXTRA_APP_URLS   optional additional origins (comma- and/or
#                               whitespace-separated) also accepted as redirect
#                               URIs / web origins / post-logout URIs. Use it to
#                               keep the old Railway domain valid alongside a new
#                               custom domain, so a domain switch is not a
#                               flag day and the silent-SSO iframe never wedges
#                               the SPA on the loading spinner.
#
# Uses only bash builtins so no extra tooling (envsubst/sed) needs to be present
# in the Keycloak image.
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

# Build the origin list: primary first, then any extras. Each extra is given an
# explicit scheme (defaulting to https) for the same reason as the primary above.
origins=("${RSTMANAGER_PUBLIC_APP_URL}")
if [ -n "${RSTMANAGER_EXTRA_APP_URLS:-}" ]; then
  # Accept both comma- and whitespace-separated lists; the // turns commas into
  # spaces so the unquoted expansion word-splits on either.
  for extra in ${RSTMANAGER_EXTRA_APP_URLS//,/ }; do
    case "${extra}" in
      http://* | https://*) : ;;
      *) extra="https://${extra}" ;;
    esac
    origins+=("${extra}")
  done
fi

# Compose the multi-value fields from the origin list. Keycloak's redirect URIs
# and web origins are JSON arrays; post.logout.redirect.uris is a single
# `##`-delimited string attribute.
redirect_list=""
weborigin_list=""
postlogout_list=""
for origin in "${origins[@]}"; do
  redirect_list+="${redirect_list:+, }\"${origin}/*\""
  weborigin_list+="${weborigin_list:+, }\"${origin}\""
  postlogout_list+="${postlogout_list:+##}${origin}/*"
done

src="/opt/keycloak/data/import-template/rstmanager-realm.json"
dst_dir="/opt/keycloak/data/import"
dst="${dst_dir}/rstmanager-realm.json"

mkdir -p "${dst_dir}"
content="$(cat "${src}")"

# Splice the multi-origin fields first, matching the committed localhost
# literals. Quoting the search variable makes each a literal (non-glob) match, so
# the many `/` in the URLs need no escaping. This must run before the global
# origin rewrite below, which would otherwise erase the localhost literals.
find_redirect='"redirectUris": ["http://localhost:3333/*"]'
find_weborigin='"webOrigins": ["http://localhost:3333"]'
find_postlogout='"post.logout.redirect.uris": "http://localhost:3333/*"'
content="${content/"${find_redirect}"/"\"redirectUris\": [${redirect_list}]"}"
content="${content/"${find_weborigin}"/"\"webOrigins\": [${weborigin_list}]"}"
content="${content/"${find_postlogout}"/"\"post.logout.redirect.uris\": \"${postlogout_list}\""}"

# Remaining single-origin fields (rootUrl) still carry the localhost literal:
# rewrite them to the primary origin.
content="${content//http:\/\/localhost:3333/${RSTMANAGER_PUBLIC_APP_URL}}"
printf '%s\n' "${content}" > "${dst}"

echo "[rstmanager] realm import rendered for origins: ${origins[*]}"
echo "[rstmanager] KC_HOSTNAME: ${KC_HOSTNAME:-<unset>}"

exec /opt/keycloak/bin/kc.sh "$@"
