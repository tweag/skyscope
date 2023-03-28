load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def configure_skyscope():
    http_archive(
        name = "skyscope_linux",
        urls = ["file:///skyscope/bazel-bin/release/skyscope-$PLATFORM.zip"],
        sha256 = "6c0637ab33f7408435833b9b21f8cbdf3dcc2c39c35b89c45a49bd3d3cc59e96",
    )
    http_archive(
        name = "skyscope_macos",
        urls = ["file:///skyscope/bazel-bin/release/skyscope-$PLATFORM.zip"],
        sha256 = "6c0637ab33f7408435833b9b21f8cbdf3dcc2c39c35b89c45a49bd3d3cc59e96",
    )
