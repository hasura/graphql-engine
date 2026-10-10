#!/usr/bin/env bash
#
# Run the console Cypress e2e suite locally against a dockerised backend.
# Local counterpart of .buildkite/scripts/frontend/test-{oss,ee}-console.sh:
# same services (Postgres, graphql-engine, `hasura console` API on :9693) and
# the same NX_PUBLIC_* console env, so local runs behave like CI.
#
# The console under test is always the current frontend checkout: the script
# starts the webpack dev server (`nx run console-<edition>:serve`, :4200 for CE,
# :5500 for EE) with the CI env, waits for it, and points Cypress at it.
#
# Usage (from anywhere):
#   frontend/docker/e2e/run-e2e.sh [ce|ee] [--keep] [-- <extra cypress args>]
#
# Examples:
#   frontend/docker/e2e/run-e2e.sh
#   frontend/docker/e2e/run-e2e.sh ce -- --spec src/e2e/cron-triggers/cron.test.ts
#   HGE_VERSION=latest frontend/docker/e2e/run-e2e.sh ee --keep
#
# Options:
#   ce|ee    which suite to run (default: ce)
#   --keep   leave the docker stack running afterwards (default: torn down,
#            volumes included, so every run starts from empty metadata).
#            The dev server is always stopped.
#
# Env:
#   HGE_VERSION   graphql-engine image tag (default: see docker-compose.yml)
#   HGE_PORT      host port for graphql-engine (default: 8080)
#   CLI_API_PORT  host port for the CLI migrate API (default: 9693)
#   Change the ports to run next to a graphql-engine / `hasura console`
#   you already have on the defaults.
#
# Dev server and container logs are written to frontend/tmp/e2e-logs/
# (gitignored). Paths given to --spec are relative to apps/console-<edition>-e2e.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRONTEND_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
COMPOSE=(docker compose -f "$SCRIPT_DIR/docker-compose.yml")
LOG_DIR="$FRONTEND_DIR/tmp/e2e-logs"

export HGE_PORT="${HGE_PORT:-8080}"
export CLI_API_PORT="${CLI_API_PORT:-9693}"
HGE_URL="http://localhost:$HGE_PORT"

EDITION=ce
KEEP=false
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    ce | ee) EDITION="$1" ;;
    --keep) KEEP=true ;;
    --)
      shift
      EXTRA_ARGS=("$@")
      break
      ;;
    -h | --help)
      sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "Unknown argument: $1 (see --help)" >&2
      exit 1
      ;;
  esac
  shift
done

# Ports are fixed by apps/console-{ce,ee}/webpack.config.js and match the
# baseUrl in each e2e app's cypress.config.ts.
if [[ "$EDITION" == ee ]]; then
  CONSOLE_PORT=5500
else
  CONSOLE_PORT=4200
fi
CONSOLE_URL="http://localhost:$CONSOLE_PORT"
SERVER_PID=

port_in_use() {
  nc -z localhost "$1" >/dev/null 2>&1
}

stack_running() {
  [[ -n "$("${COMPOSE[@]}" ps -q 2>/dev/null)" ]]
}

save_logs() {
  mkdir -p "$LOG_DIR"
  for svc in graphql-engine cli; do
    "${COMPOSE[@]}" logs --no-color "$svc" >"$LOG_DIR/$svc.log" 2>&1 || true
  done
  echo "Logs saved to $LOG_DIR"
}

stop_dev_server() {
  if [[ -n "$SERVER_PID" ]]; then
    echo "--- Stopping the console dev server"
    # nx spawns webpack as a child; stop the tree, then anything left on the port.
    pkill -TERM -P "$SERVER_PID" >/dev/null 2>&1 || true
    kill -TERM "$SERVER_PID" >/dev/null 2>&1 || true
    sleep 1
    lsof -ti "tcp:$CONSOLE_PORT" -sTCP:LISTEN 2>/dev/null | xargs kill -TERM 2>/dev/null || true
  fi
}

cleanup() {
  local status=$?
  stop_dev_server
  save_logs
  if [[ "$KEEP" == true ]]; then
    echo "Leaving the docker stack running (--keep). Stop it with:"
    echo "  ${COMPOSE[*]} down -v"
  else
    echo "--- Tearing down the docker stack"
    "${COMPOSE[@]}" down -v --remove-orphans >/dev/null 2>&1 || true
  fi
  exit "$status"
}

# Whatever already answers on the console port may be built from other code or
# with another env, and Cypress would silently test it. Require a free port.
if port_in_use "$CONSOLE_PORT"; then
  echo "Port $CONSOLE_PORT is in use. Stop the running console dev server first:" >&2
  echo "this script serves the current checkout with the CI env itself." >&2
  exit 1
fi

if ! stack_running; then
  for port in "$HGE_PORT" "$CLI_API_PORT"; do
    if port_in_use "$port"; then
      echo "Port $port is already in use by something outside this stack." >&2
      echo "Stop it, or pick free ports with HGE_PORT / CLI_API_PORT." >&2
      exit 1
    fi
  done
fi

trap cleanup EXIT
mkdir -p "$LOG_DIR"

echo "--- Starting graphql-engine ${HGE_VERSION:-(compose default)}, postgres and the CLI"
"${COMPOSE[@]}" up -d --wait

SERVER_VERSION="$(curl -sf "$HGE_URL/v1/version" | sed -E 's/.*"version":"([^"]+)".*/\1/')"
echo "graphql-engine $SERVER_VERSION is up on :$HGE_PORT; CLI migrate API is on :$CLI_API_PORT"

# Same console env as CI. Process env wins over frontend/.env in Nx, so a
# developer's personal .env can't leak in; the admin-secret vars are blanked
# explicitly because the stack runs without one.
export NX_NODE_ENV=development
export NX_PUBLIC_DATA_API_URL="$HGE_URL"
export NX_DEV_DATA_API_URL="$HGE_URL"
export NX_PUBLIC_API_HOST=http://localhost
export NX_PUBLIC_API_PORT="$CLI_API_PORT"
export NX_PUBLIC_CONSOLE_MODE=cli
export NX_PUBLIC_URL_PREFIX=/
export NX_PUBLIC_SERVER_VERSION="$SERVER_VERSION"
export NX_PUBLIC_IS_ADMIN_SECRET_SET=
export NX_PUBLIC_ADMIN_SECRET=
if [[ "$EDITION" == ee ]]; then
  export NX_PUBLIC_HASURA_CONSOLE_TYPE=pro
else
  export NX_PUBLIC_HASURA_CONSOLE_TYPE=oss
fi
# The specs' own requests (support/endpoints.ts).
export CYPRESS_HGE_URL="$HGE_URL"
export CYPRESS_CLI_URL="http://localhost:$CLI_API_PORT"

cd "$FRONTEND_DIR"

echo "--- Serving console-$EDITION from the current checkout on :$CONSOLE_PORT"
npx nx run "console-$EDITION:serve" --outputStyle static >"$LOG_DIR/console.log" 2>&1 &
SERVER_PID=$!

# First start builds the shared libs (^build) and the app; allow 10 minutes.
for _ in $(seq 1 600); do
  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    echo "The console dev server exited early. Last lines of $LOG_DIR/console.log:" >&2
    tail -30 "$LOG_DIR/console.log" >&2
    exit 1
  fi
  curl -sf -o /dev/null "$CONSOLE_URL" && break
  sleep 1
done
if ! curl -sf -o /dev/null "$CONSOLE_URL"; then
  echo "Timed out waiting for $CONSOLE_URL (see $LOG_DIR/console.log)" >&2
  exit 1
fi
echo "Console is up on $CONSOLE_URL"

# Editor-integrated terminals (VS Code, Cursor) set this, which makes the
# Cypress Electron binary run as plain Node and fail to start.
unset ELECTRON_RUN_AS_NODE

echo "--- Running console-$EDITION-e2e against $CONSOLE_URL"
cd "$FRONTEND_DIR/apps/console-$EDITION-e2e"
# nxE2EPreset sees the server already answering on baseUrl and reuses it.
npx cypress run --config-file=./cypress.config.ts --config "baseUrl=$CONSOLE_URL" \
  ${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}
