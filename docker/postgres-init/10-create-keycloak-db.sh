#!/bin/sh
# Creates the Keycloak database in the shared postgres instance.
# NOTE: docker-entrypoint init scripts only run when the postgres-data volume is
# empty. On an existing deployment run the statements below manually:
#   docker compose exec postgres psql -U "$POSTGRES_USER" -d "$POSTGRES_DB"
set -e

psql -v ON_ERROR_STOP=1 -U "$POSTGRES_USER" -d "$POSTGRES_DB" <<-EOSQL
  CREATE USER keycloak WITH PASSWORD '${KEYCLOAK_DB_PASSWORD:-keycloak}';
  CREATE DATABASE keycloak OWNER keycloak;
EOSQL
