#!/bin/bash
LC_ALL=C.UTF-8 nix \
  --extra-experimental-features flakes \
  --extra-experimental-features nix-command \
  develop --command ghcid --allow-eval --command \
    bazel run //backend:skyscope@repl
