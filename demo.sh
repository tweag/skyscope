#!/bin/bash
nix-shell --run '
    bazel build //...
    (sleep 1; bazel dump --skyframe=detailed) | bazel run //backend:skyscope
'

# time (sleep 1; cd ~/git/bazel-codelab/; bazel build //... 1>/dev/null; bazel dump --skyframe=detailed) | bazel run //backend:skyscope
