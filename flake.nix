{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = inputs.nixpkgs.legacyPackages.${system};

          mkCommand = name: command: inputs.flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              inherit name;
              text = command;
              runtimeInputs = tools;
            };
          };

          tools = with pkgs; [
            bazel_5
            entr
            esbuild
            ghcid
            graphviz
            haskellPackages.hasktags
            jq
            nodejs
            ormolu
            purescript
            spago
          ];

      in {
        devShells.default = pkgs.mkShell { nativeBuildInputs = tools; };

        apps.build = mkCommand "build"
          "bazel build //backend:skyscope --host_platform=//backend:regular_executable";

        apps.ghcid = mkCommand "ghcid"
          "LC_ALL=C.UTF-8 ghcid --allow-eval --command bazel run //backend:skyscope@repl";

        apps.purs = mkCommand "purs"
          "cd frontend; echo src/Main.purs | entr bash -c 'clear; spago bundle-app'";

        apps.format = mkCommand "format" ''
          # shellcheck disable=SC2046
          ormolu --mode inplace $(find . -name '*.hs')
        '';

        apps.tags = mkCommand "tags" ''
          hasktags -c -S '[".hs",".purs"]' .
        '';
      });
}
