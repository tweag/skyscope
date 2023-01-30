with import ./nixpkgs.nix {
  config = { };
  overlays = [ ];
};
mkShell {
  nativeBuildInputs = [ bazel_5 entr esbuild graphviz purescript spago ];
}
