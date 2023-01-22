#!/bin/bash
nix-shell --packages bazel_5 ghcid --run '
    LC_ALL=C.UTF-8 ghcid --allow-eval --command "
        bazel run //:skylight@repl
    "
'
