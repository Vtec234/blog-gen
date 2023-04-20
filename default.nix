# default.nix
# From https://srid.ca/haskell-nix
let 
  pkgs = import <nixpkgs> { };
  compiler = pkgs.haskell.packages.ghc927;
in 
  compiler.developPackage {
    root = ./.;
    # How to specify source overrides: https://haskell4nix.readthedocs.io/frequently-asked-questions.html?highlight=developPackage#how-to-specify-source-overrides-for-your-haskell-package
    source-overrides = { };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ haskell-language-server
          cabal-install
          pkgs.nodejs
          pkgs.python310Packages.pygments
        ]);
  }
