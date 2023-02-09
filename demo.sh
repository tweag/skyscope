#!/bin/bash
nix-shell --run '
    bazel build //...
    (sleep 1; bazel dump --skyframe=detailed) | bazel run //backend:skyscope
'
