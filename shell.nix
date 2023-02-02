with import ./nixpkgs.nix {
  config = { };
  overlays = [ ];
};
mkShell {
  nativeBuildInputs =
    [ bazel_5 entr esbuild ghcid graphviz nodejs purescript spago ];
}
