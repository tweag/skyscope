#!/usr/bin/env bash
set -euo pipefail
NIX_DB_PATH="${1:-/nix/var/nix/db/db.sqlite}"

sql-query() {
  sqlite3 -column -readonly "file://$NIX_DB_PATH?immutable=1" "$1"
}

printf 'digraph "" {\n'

sql-query 'select id, path from ValidPaths;' | sed '
  s/\([0-9]\+\)\s\+\(\S*\)/echo "  node\1 [ label=\\"$(basename \2)\\" ];"/e
  s/label="\([0-9a-z]\{32\}-\([^"]*\)\)"/label="\2\\n\1"/
'

sql-query 'select referrer, reference from Refs where referrer != reference;' | sed '
  s/\([0-9]\+\)\s\+\([0-9]\+\)/  node\1 -> node\2;/
'

printf '}\n'
