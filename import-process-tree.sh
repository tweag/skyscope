#!/usr/bin/env bash
set -euo pipefail
ps --no-headers -eo "%P||%p||%c||%a" | jq -R '
    split("|" + "|") | to_entries | map (
        if .key > 2 then
            .value
        else
            .value | match("[^ ]+") | .string
        end
    )
' | jq -s '
    [
        (map({
            key: (.[1]),
            value: { nodeData: .[3], nodeType: .[2] }
        }) | from_entries),
        map({
            edgeGroup: 0,
            edgeSource: .[0],
            edgeTarget: .[1]
        } | select(.edgeSource != "0"))
    ]
' | tee process-tree.json | SKYSCOPE_DEBUG=1 bazel run -- //:skyscope import-json

echo -e "\n\x1b[35mWrote graph to \x1b[1;35mprocess-tree.json\x1b[0m"
