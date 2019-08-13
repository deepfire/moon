pkgs: new: old:
with new; with pkgs.haskell.lib;
let
  c = owner: repo: rev: sha256: cabalExtras:
  doJailbreak
  (old.callCabal2nixWithOptions repo (pkgs.fetchFromGitHub {
      inherit owner repo rev sha256;
      }) cabalExtras {});
  ds = c "deepshared";
  io = repo: rev: s: subdir: c "input-output-hk" repo rev s "--subpath ${subdir}";
  io-on = io "ouroboros-network";
  io-onp = io-on "bab77eaf56093022d8860d5df4700a836a9c8a47" "1s8nchhs9vl4k5f6v57nv2yfc3prw1nn77mwhan2k5aav0f66rgr";
  # overcabal = pkgs.haskell.lib.overrideCabal;
  # hubsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
  # overc     = old:                    args: overcabal old (oldAttrs: (oldAttrs // args));
  # overhub   = old: repo: rev: sha256: args: overc old ({ src = hubsrc repo rev sha256; }       // args);
  # overhage  = old: version:   sha256: args: overc old ({ version = version; sha256 = sha256; } // args);
in {
  ## We can't override ghc-lib, since it's not buildable with Nix -- from the git repo.
  # ghc-lib   = doJailbreak (overhub old.ghc-lib "digital-asset/ghc-lib" "686d81fdda68fb4c604b94186de8a5899ec8c21c" "0dcmwaxmfanpy9fdyfi82vyf3r6kvxnadni558hzvndsmpgmq4ys" {});
  algebraic-graphs = dontCheck (overrideCabal old.algebraic-graphs (old: { broken = false; }));
  async-timer      = dontCheck (overrideCabal old.async-timer (old: { broken = false; }));
  contra-tracer   = io "iohk-monitoring-framework" "6e3047f785efe874819e8654ab928b0d9e9ff499" "0jqig5csj6yqfndvx047pbyxyw40fjzp0i4wxhpdh6wjx5ykwy8w" "contra-tracer";
  iohk-monitoring = io "iohk-monitoring-framework" "6e3047f785efe874819e8654ab928b0d9e9ff499" "0jqig5csj6yqfndvx047pbyxyw40fjzp0i4wxhpdh6wjx5ykwy8w" "iohk-monitoring";

}
