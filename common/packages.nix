let
  nixpkgs          = import ../nix/default.nix;
  default          = import ./.; ## absent
  default-compiler = import ../nix/default-compiler.nix;
  filterSrc        = pkgs:
                     import ../nix/filtersrc.nix pkgs;
  overrides        = import ../nix/overrides.nix;
in
{ compiler         ? default-compiler
}:
let
  pkgs     = (nixpkgs {}).pkgs;
  ghcOrig  = pkgs.haskell.packages."${compiler}";   # :: nixpkgs/pkgs/development/haskell-modules/make-package-set.nix
  ghc      = ghcOrig.override { overrides = overrides pkgs; };
  extras   = [
               # ghc.ghc-events
               ghc.ghcid
               # ghc.graphmod
               ghc.cabal-install
               pkgs.graphviz
             ];
  localPackages  = with ghc; with pkgs.lib; with builtins; {};
  final-default  = ghc.callPackage default localPackages;
  final-inferred = ghc.callCabal2nix "common" ./. {};
  final          = filterSrc pkgs final-inferred;
in {
  inherit ghc;

  common = final;

  shell = ghc.shellFor {
    packages    = p: [final-inferred];
    withHoogle  = true;
    buildInputs = extras;
  };
}
