#!/usr/bin/env bash
set -eux
nix develop \
  --extra-experimental-features flakes \
  --extra-experimental-features nix-command \
  --command bash -c '
    bazel build //:skyscope

    # The entire graph takes too long to import for a demo,
    # so clear it and then populate a smaller subgraph.
    bazel shutdown
    bazel build //backend:skyscope

    bazel-bin/skyscope --query --aquery
  '
