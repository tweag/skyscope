{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = inputs.nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            bazel_5
            entr
            esbuild
            ghcid
            graphviz
            nodejs
            purescript
            spago
          ];
        };
        formatter = pkgs.nixpkgs-fmt;
      });
}
