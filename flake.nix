{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [
        haskellNix.overlay
          (final: prev: {
            blogProject =
              final.haskell-nix.cabalProject' {
                src = ./.;
                compiler-nix-name = "ghc964";
                # Configuration of `nix develop .` and the direnv shell
                shell = {
                  tools = {
                    cabal = {};
                    hlint = {};
                    haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    nodejs
                    python311Packages.pygments
                  ];
                  withHoogle = false;
                  exactDeps = true;
                };
              };
          })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.blogProject.flake {};
    in flake // {
      legacyPackages = pkgs;
      # Default target for `nix build .`
      packages.default = flake.packages."blog-gen:exe:site";
    });

  # Local options for `nix`
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    allow-import-from-derivation = "true";
  };
}
