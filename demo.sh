#!/usr/bin/env bash
set -eux
nix develop \
  --extra-experimental-features flakes \
  --extra-experimental-features nix-command \
  --command bash -c "bazel build //backend:skyscope && '$PWD/bin/skyscope'"
