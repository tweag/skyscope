#!/bin/bash
nix-shell --run '
    LC_ALL=C.UTF-8 ghcid --allow-eval --command "
        bazel run //backend:skyscope@repl
    "
'
