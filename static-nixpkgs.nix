{...}@args:
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  nixpkgs =
    let
      src = lock.nodes.nixpkgs.locked;
    in
    assert src.type == "github";
    fetchTarball {
      url = "https://github.com/${src.owner}/${src.repo}/archive/${src.rev}.tar.gz";
      sha256 = src.narHash;
    };
  normalPkgs = import nixpkgs args;
  static-haskell-nix =
    fetchTarball {
      url = "https://github.com/nh2/static-haskell-nix/archive/bd66b86b72cff4479e1c76d5916a853c38d09837.tar.gz";
      sha256 = "sha256:0rnsxaw7v27znsg9lgqk1i4007ydqrc8gfgimrmhf24lv6galbjh";
    };
in
(import "${static-haskell-nix}/survey" { inherit normalPkgs; }).pkgsWithArchiveFiles
