pkgs: new: old:
with new; with pkgs.haskell.lib;
let
  l = repo: path: cabalExtras:
      doJailbreak
      (old.callCabal2nixWithOptions repo path cabalExtras {});
  c = owner: repo: rev: sha256: cabalExtras:
      doJailbreak (old.callCabal2nixWithOptions repo (pkgs.fetchFromGitHub {
        inherit owner repo rev sha256;
      }) cabalExtras {});
  ds = c "deepshared";
  io = repo: rev: s: subdir: c "input-output-hk" repo rev s "--subpath ${subdir}";
  io-on  = io "ouroboros-network";
  io-onp = x: dontCheck (io-on "6cbde599eba87edf983c620fd8a9ed4015c9b50a" "0pqhradp9g5vcyrsh7ywy0wdzy8j8i6gdphscslfaa2sk5ai1n45" x);
  io-mfp = io "iohk-monitoring-framework" "6e3047f785efe874819e8654ab928b0d9e9ff499" "0jqig5csj6yqfndvx047pbyxyw40fjzp0i4wxhpdh6wjx5ykwy8w";
  # overcabal = pkgs.haskell.lib.overrideCabal;
  # hubsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
  # overc     = old:                    args: overcabal old (oldAttrs: (oldAttrs // args));
  # overhub   = old: repo: rev: sha256: args: overc old ({ src = hubsrc repo rev sha256; }       // args);
  # overhage  = old: version:   sha256: args: overc old ({ version = version; sha256 = sha256; } // args);
in {
  ## We can't override ghc-lib, since it's not buildable with Nix -- from the git repo.
  # ghc-lib   = doJailbreak (overhub old.ghc-lib "digital-asset/ghc-lib" "686d81fdda68fb4c604b94186de8a5899ec8c21c" "0dcmwaxmfanpy9fdyfi82vyf3r6kvxnadni558hzvndsmpgmq4ys" {});
  algebraic-graphs      = dontCheck (overrideCabal old.algebraic-graphs (old: { broken = false; }));
  async-timer           = dontCheck (overrideCabal old.async-timer (old: { broken = false; }));
  contra-tracer         = io-mfp "contra-tracer";
  iohk-monitoring       = io-mfp "iohk-monitoring";
  io-sim                = io-onp "io-sim";
  io-sim-classes        = io-onp "io-sim-classes";
  typed-protocols       = io-onp "typed-protocols";
  typed-protocols-cbor  = io-onp "typed-protocols-cbor";
  cborg                 = l "cborg" ../../cborg/cborg "";
  serialise             = l "serialise" ../../cborg/serialise "";

  common          = new.callCabal2nix "common" ../common {};
}
