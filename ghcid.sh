#!/bin/bash
LC_ALL=C.UTF-8 ghcid --allow-eval --command '
  nix-shell --packages bazel_5 --run "
    bazel run //:example@repl
  "
'
