#!/usr/bin/env bash
set -euo pipefail

[[ "${SKYSCOPE_DEBUG:-}" ]] && set -x

# This script can be invoked in three ways, so establish some consistency first.
BUILD_WORKSPACE_DIRECTORY="${BUILD_WORKSPACE_DIRECTORY:-$(bazel info workspace)}"
export SKYSCOPE_WORKSPACE="${SKYSCOPE_WORKSPACE:-$BUILD_WORKSPACE_DIRECTORY}"
RUNPATH="$PWD"
cd "${SKYSCOPE_WORKSPACE}"

# Determine some Bazel information.
export SKYSCOPE_OUTPUT_BASE="$(bazel info output_base)"
export SKYSCOPE_DATA="${SKYSCOPE_DATA:-$HOME/.skyscope}"
export BAZEL_VERSION="$(bazel version | grep -Po '(?<=Build label: ).*')"

skyscope() {(
    cd "$RUNPATH"
    CLOSURE="${BASH_SOURCE[0]%/bin/skyscope}/closure"
    SKYSCOPE_BINARY="${SKYSCOPE_BINARY:-$CLOSURE/skyscope}"

    # Hack: fix interpreter on non-NixOS systems.
    if ! patchelf --version; then
        echo
        echo "Warning: could not find patchelf"
        echo "This tool is needed to patch the dynamic loader on Linux systems. Will attempt to continue anyway."
        echo "If you get 'No such file or directory' errors, try installing 'patchelf' with your preferred package manager."
        echo "    For example: sudo apt install patchelf"
        echo
    else
        chmod u+w "$SKYSCOPE_BINARY"
        INTERPRETER="$(patchelf --print-interpreter "$SKYSCOPE_BINARY")"
        if [[ ! -x "$INTERPRETER" ]]; then
            patchelf --set-interpreter "$CLOSURE/$(basename $INTERPRETER)" "$SKYSCOPE_BINARY"
        fi
    fi

    LD_LIBRARY_PATH="$CLOSURE" "$SKYSCOPE_BINARY" +RTS -N -RTS "$@"
)}

# Check the required tools are available.
missing() {
    echo
    echo "Unable to find required tool: $1"
    echo
    echo "You should install '${2:-$1}' using your preferred package manager and ensure"
    echo "its binaries are reachable from your PATH variable. For more information see:"
    echo "  https://github.com/tweag/skyscope#getting-skyscope"
    exit 1
}
dot -V || missing dot graphviz
curl -V || missing curl
jq -V || missing jq

case "${1:-}" in
    import|server)
        # Restart the server.
        PID_FILE="$SKYSCOPE_DATA/server.pid"
        PID=$(cat "$PID_FILE" 2>/dev/null) && {
          kill $PID 2>/dev/null || true
          rm $PID_FILE
        }
        skyscope server "${SKYSCOPE_PORT:-28581}"
        [[ "$1" = server ]] && exit 0
        shift
        ;;
    *)
        echo "usage: skyscope [import|server] [args]" >&2
        exit 1
esac

# Parse extra arguments.
SKYSCOPE_QUERY='deps(//...)'
SKYSCOPE_AQUERY='deps(//...)'
while [[ "${1:-}" ]]
  do case "$1" in
    --no-query) SKYSCOPE_QUERY='' ;;
    --no-aquery) SKYSCOPE_AQUERY='' ;;
    --query=*) SKYSCOPE_QUERY="${1#--query=}" ;;
    --aquery=*) SKYSCOPE_AQUERY="${1#--aquery=}" ;;
    *)
      echo "invalid arg: $1"
      exit 1
      ;;
  esac
  shift
done

# Prepare a path for import database.
DB_TEMPLATE="$(basename "$SKYSCOPE_WORKSPACE")-XXXXX.sqlite"
IMPORT_DIR="$SKYSCOPE_DATA/imports"
mkdir -p "$IMPORT_DIR"
DB_PATH="$(mktemp -p "$IMPORT_DIR" "$DB_TEMPLATE")"

# Determine dump options.
case "$BAZEL_VERSION" in
  3.*|4.*|5.*)
    DUMP_OPT="detailed"
    export SKYSCOPE_LEGACY_BAZEL="1"
    ;;
  *)
    DUMP_OPT="deps"
    ;;
esac

bazel dump --skyframe=$DUMP_OPT | skyscope import-skyframe "$DB_PATH"

if [[ "${SKYSCOPE_AQUERY:-}" ]]; then
    if ! bazel aquery "$SKYSCOPE_AQUERY" | skyscope import-actions "$DB_PATH"; then
        echo -e "\e[33mSkipping import of additional action data. Will be unable to show specific ActionExecution types." >&2
        echo -e "You can try passing a different pattern to the \e[1;33m--aquery=\e[0;33m parameter to work around this.\e[0m" >&2
    fi
fi

if [[ "${SKYSCOPE_QUERY:-}" ]]; then
    if ! bazel query "$SKYSCOPE_QUERY" --output build | skyscope import-targets "$DB_PATH"; then
        echo -e "\e[33mSkipping import of additional target data. Will be unable to show specific ConfiguredTarget types." >&2
        echo -e "You can try passing a different pattern to the \e[1;33m--query=\e[0;33m parameter to work around this.\e[0m" >&2
    fi
fi

# Notify server about new import.
URL_BASE="http://localhost:28581"
IMPORT_RESULT=$(curl -s -XPOST "$URL_BASE" -d "[\"$SKYSCOPE_WORKSPACE\",\"$DB_PATH\"]")
URL="$URL_BASE/$(jq -r .importId <<<"$IMPORT_RESULT")"

printf "\nOpen this link in your browser:\n"
printf "  \x1b[1;36m%s\x1b[0m\n" "$URL"
