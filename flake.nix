{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        # https://github.com/purenix-org/purenix/issues/34#issuecomment-981290387
        #"x86_64-linux"
        #"x86_64-darwin"
        #"aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            blogProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc964";
                # Tools to provide in `nix develop .`
                shell.tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                };
                shell.buildInputs = with pkgs; [
                  nodejs
                  python311Packages.pygments
                ];
                supportHpack = true;
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.blogProject.flake {};
      in flake // {
        legacyPackages = pkgs;
        # Built by `nix build .`
        packages.default = flake.packages."blog-gen:exe:site";
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    allow-import-from-derivation = "true";
  };
}
