# Give your project a name. :)
workspace(name = "skylight")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    sha256 = "2a07b55c30e526c07138c717b0343a07649e27008a873f2508ffab3074f3d4f3",
    strip_prefix = "rules_haskell-0.16",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v0.16.tar.gz",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

stack_snapshot(
    name = "stackage",
    extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
    packages = [
        "aeson",
        "attoparsec",
        "bytestring",
        "containers",
        "cryptohash-sha256",
        "direct-sqlite",
        "stm",
        "text",
        "time",
        "unix",
        "zlib",
    ],

    # LTS snapshot published for ghc-8.10.7 (default version used by rules_haskell)
    snapshot = "lts-18.18",

    # This uses an unpinned version of stack_snapshot, meaning that stack is invoked on every build.
    # To switch to pinned stackage dependencies, run `bazel run @stackage-unpinned//:pin` and
    # uncomment the following line.
    # stack_snapshot_json = "//:stackage_snapshot.json",
)

# Load nixpkgs_git_repository from rules_nixpkgs,
# which was already initialized by rules_haskell_dependencies above.
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_git_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

# Fetch a version of nixpkgs from GitHub.
# For more information see the documentation of rules_nixpkgs at
# https://github.com/tweag/rules_nixpkgs/blob/master/README.md
nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "21.11",
    sha256 = "c77bb41cf5dd82f4718fa789d49363f512bb6fa6bc25f8d60902fe2d698ed7cc",
)

nixpkgs_cc_configure(
    repository = "@nixpkgs",
)

nixpkgs_python_configure(
    repository = "@nixpkgs",
)

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

# Fetch a GHC binary distribution from nixpkgs and register it as a toolchain.
# For more information:
# https://api.haskell.build/haskell/nixpkgs.html#haskell_register_ghc_nixpkgs
haskell_register_ghc_nixpkgs(
    repository = "@nixpkgs",
    attribute_path = "ghc",
    version = "8.10.7",
)

# For zlib.BUILD.bazel
nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib.dev",
    build_file = "//:zlib.BUILD.bazel",
    repository = "@nixpkgs",
)
