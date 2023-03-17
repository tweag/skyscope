#!/usr/bin/env bash
set -euo pipefail

BIN_DIR="$PWD"
cd "$USER_PWD"
bazel info 1>/dev/null

while [[ "${1:-}" ]]
  do case "$1" in
    --query) export SKYSCOPE_QUERY="deps(//...)" ;;
    --aquery) export SKYSCOPE_AQUERY='deps(//...)' ;;
    --query=*) export SKYSCOPE_QUERY="${1#--query=}" ;;
    --aquery=*) export SKYSCOPE_AQUERY="${1#--aquery=}" ;;
    --port=*) export SKYSCOPE_PORT="${1#--port=}" ;;
    --server) export SKYSCOPE_SERVER="1" ;;
    --debug) set -x ;;
    *)
      echo "invalid arg: $1"
      exit 1
      ;;
  esac
  shift
done

export BAZEL_VERSION="$(bazel version | grep -Po '(?<=Build label: ).*')"
export SKYSCOPE_WORKSPACE="$(bazel info | grep -Po '(?<=workspace: ).*')"
export SKYSCOPE_OUTPUT_BASE="$(bazel info | grep -Po '(?<=output_base: ).*')"
export SKYSCOPE_DATA="${SKYSCOPE_DATA:-$HOME/.skyscope}"

run-skyscope() {
  "$BIN_DIR/ld-linux-x86-64.so.2" --library-path "$BIN_DIR" \
    "$BIN_DIR/skyscope" +RTS -N -RTS "$@"

}

run-bazel() {
  bazel "$@" || {
    exec 1>&2
    echo -e "\e[31mbazel failed:\e[37m $@\e[0m"
    case "$1" in
      query)
        QUERY_CLASS="target"
        ;;
      aquery)
        QUERY_CLASS="action"
        ;;
    esac
    if [[ "$QUERY_CLASS" ]]
      then echo -e "\e[33mSkipping import of additional $QUERY_CLASS data. Will be unable to show specific types."
           echo -e "Try passing a different pattern to the \e[1;33m--$1=\e[0;33m argument to work around this.\e[0m"
           return
    fi
    exit 1
  }
}

PID_FILE="$SKYSCOPE_DATA/server.pid"
PID=$(cat "$PID_FILE" 2>/dev/null) && {
  kill $PID || true
  rm $PID_FILE
}

run-skyscope server "${SKYSCOPE_PORT:-28581}"
if [[ "${SKYSCOPE_SERVER:-}" ]]; then exit 0; fi

DB_TEMPLATE="$(basename "$SKYSCOPE_WORKSPACE")-XXXXX.sqlite"
IMPORT_DIR="$SKYSCOPE_DATA/imports"
mkdir -p "$IMPORT_DIR"
DB_PATH="$(mktemp -p "$IMPORT_DIR" "$DB_TEMPLATE")"

case "$BAZEL_VERSION" in
  3.*|4.*|5.*)
    DUMP_OPT="detailed"
    export SKYSCOPE_LEGACY_BAZEL="1"
    ;;
  *)
    DUMP_OPT="deps"
    ;;
esac

run-bazel dump --skyframe=$DUMP_OPT | run-skyscope import-skyframe "$DB_PATH"

if [[ "${SKYSCOPE_AQUERY:-}" ]]
  then run-bazel aquery "$SKYSCOPE_AQUERY" | run-skyscope import-actions "$DB_PATH"
fi

if [[ "${SKYSCOPE_QUERY:-}" ]]
  then run-bazel query "$SKYSCOPE_QUERY" --output build | run-skyscope import-targets "$DB_PATH"
fi

URL_BASE="http://localhost:28581"
IMPORT_RESULT=$(curl -s -XPOST "$URL_BASE" -d "[\"$SKYSCOPE_WORKSPACE\",\"$DB_PATH\"]")
URL="$URL_BASE/$(jq -r .importId <<<"$IMPORT_RESULT")"

printf "\nOpen this link in your browser:\n"
printf "  🔭 \x1b[1;36m%s\x1b[0m\n" "$URL"
