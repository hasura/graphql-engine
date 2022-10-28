#!/usr/bin/env bash

# A utility script to setup local Hoogle instance.

set -euo pipefail

HOOGLE=hoogle
CABAL=cabal
CABAL_ARGS="--haddock-hoogle"
CABAL_DIST=dist-newstyle
HOOGLE_DATABASE=$CABAL_DIST/hge.hoo
HOOGLE_PORT=1337
PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"

MODE=$1

print_usage() {
cat << EOL
A utility script for setting up local Hoogle instance

Usage: $0 <COMMAND>

Available Commands:

  generate
    Generate local hoogle database and store it at $HOOGLE_DATABASE

  serve [--port INT]
    Start local hoogle server. If --port is not provided then the server will run on port $HOOGLE_PORT.

Global flags:
-h, --help        Show this help text

EOL
}

die_usage() {
  print_usage
  exit 1
}

show_help() {
  print_usage
  exit 0
}

case "${1-}" in
  -h)
  show_help
  ;;
  --help)
  show_help
  ;;
  generate)
  ;;
  serve)
    case "${2-}" in
      --port)
        HOOGLE_PORT="${*:3}"
      ;;
    esac
  ;;
  *)
  die_usage
  ;;
esac

check_hoogle() {
  if ! command -v "$HOOGLE" &> /dev/null ; then
    echo "ERROR: $HOOGLE not found, Please install."
    exit 1
  fi
}

if [ "$MODE" = "generate" ]; then
  check_hoogle
  cd "$PROJECT_ROOT"
 	echo "Generating haddock hoogle files"
  $CABAL haddock $CABAL_ARGS all
	echo "Generating hoogle database"
  $HOOGLE generate --local=$CABAL_DIST --database=$HOOGLE_DATABASE

elif [ "$MODE" = "serve" ]; then
  check_hoogle
  cd "$PROJECT_ROOT"
  if ! [ -f "$HOOGLE_DATABASE" ]; then
    echo "Hoogle database $HOOGLE_DATABASE does not exist. Run '$0 generate' to generate database."
    exit 1
  fi
  echo "Starting local hoogle server. Visit http://localhost:$HOOGLE_PORT"
  $HOOGLE server --local --database=$HOOGLE_DATABASE --port $HOOGLE_PORT

else
  echo "Invalid command; $MODE"

fi
