#!/usr/bin/env bash
set -euo pipefail

#entrypoint

PID_FILE="$SKYSCOPE_DATA/server.pid"
PID=$(cat "$PID_FILE" 2>/dev/null) && {
  kill $PID 2>/dev/null || true
  rm $PID_FILE
}

skyscope server "${SKYSCOPE_PORT:-28581}"
