pkgs: new: old:
with new; with pkgs.haskell.lib;
let
  l = repo: path: cabalExtras:
      doJailbreak
      (old.callCabal2nixWithOptions repo path cabalExtras {});
  c = owner: repo: rev: sha256: cabalExtras:
        dontCheck
          (doJailbreak (old.callCabal2nixWithOptions repo (pkgs.fetchFromGitHub {
            inherit owner repo rev sha256;
           }) cabalExtras {}));
  ds = c "deepshared";
  io = repo: rev: s: subdir: c "input-output-hk" repo rev s "--subpath ${subdir}";
  # io-on = x: dontCheck (io "ouroboros-network" "f9469fa3885aefdfa852216a66c4e319a0d19c11" "1n0m03x5kri96lbslyc9vjvbn52b6639yhsdn2r43rhh5zmm86vq" x);
  io-on = x: l x (../../ouroboros-network + "/${x}") "";
  io-mf = io "iohk-monitoring-framework" "6e3047f785efe874819e8654ab928b0d9e9ff499" "0jqig5csj6yqfndvx047pbyxyw40fjzp0i4wxhpdh6wjx5ykwy8w";
  # overcabal = pkgs.haskell.lib.overrideCabal;
  # hubsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
  # overc     = old:                    args: overcabal old (oldAttrs: (oldAttrs // args));
  # overhub   = old: repo: rev: sha256: args: overc old ({ src = hubsrc repo rev sha256; }       // args);
  # overhage  = old: version:   sha256: args: overc old ({ version = version; sha256 = sha256; } // args);
in {
  ## We can't override ghc-lib, since it's not buildable with Nix -- from the git repo.
  # ghc-lib   = doJailbreak (overhub old.ghc-lib "digital-asset/ghc-lib" "686d81fdda68fb4c604b94186de8a5899ec8c21c" "0dcmwaxmfanpy9fdyfi82vyf3r6kvxnadni558hzvndsmpgmq4ys" {});

  # algebraic-graphs      = dontCheck (c "snowleopard" "alga"
  #                         "eb0366ffd90802b1cfc2e2d739960d5f8bba3b3c"
  #                         "0p9xv8w9iskg6lqygmf3myp892s5bq08xrgbm0zmy1isbh9rlzjv"
  #                         "");
  # (overrideCabal old.algebraic-graphs (old: { broken = false; }));
  async-timer           = dontCheck (overrideCabal old.async-timer  (old: { broken = false; }));
  cabal-install = overrideCabal old.cabal-install (drv: {
    postUnpack = "sourceRoot+=/cabal-install; echo source root reset to $sourceRoot";
    version = "3.2.0.0-git";
    editedCabalFile = null;
    src = pkgs.fetchgit {
      url = "git://github.com/haskell/cabal.git";
      rev = "9bd4cc0591616aeae78e17167338371a2542a475";
      sha256 = "005q1shh7vqgykkp72hhmswmrfpz761x0q0jqfnl3wqim4xd9dg0";
    };
  });
  # cborg                 = l "cborg" ../../cborg/cborg "";
  contra-tracer         = io-mf "contra-tracer";
  ed25519               = dontCheck (doJailbreak (overrideCabal old.ed25519 (old: {})));
  # fclabels              = doJailbreak old.fclabels;
  fclabels              = c "sebastiaanvisser" "fclabels"
                            "a8cb0cf2af7c2c54d106697559afc26fab03768c"
                            "02m3clgfij71kg9kn30gsdf2y4nbhlsbqbmhaiwhj4pbn0basd7i"
                            "";
  generic-monoid        = dontCheck (doJailbreak (overrideCabal old.generic-monoid (old: {})));
  io-sim                = io-on "io-sim";
  io-sim-classes        = io-on "io-sim-classes";
  iohk-monitoring       = io-mf "iohk-monitoring";
  lens                  = dontCheck (doJailbreak (overrideCabal old.lens_4_19_2 (old: {})));
  reflex                = dontCheck (overrideCabal old.reflex       (old: { broken = false; }));
  reflex-vty            = dontCheck (overrideCabal old.reflex-vty   (old: { broken = false; }));
  # serialise             = l "serialise" ../../cborg/serialise "";
  typed-protocols       = dontCheck (io-on "typed-protocols");
  typed-protocols-examples = dontCheck (io-on "typed-protocols-examples");
  # typed-protocols-cbor  = io-on "typed-protocols-cbor";
  md5                   = dontCheck (overrideCabal old.md5          (old: {}));
  matrix                = dontCheck (overrideCabal old.matrix       (old: {}));
  monoidal-containers   = dontCheck (doJailbreak   old.monoidal-containers);
  newtype               = dontCheck (doJailbreak   old.newtype);
  serialize             = dontCheck (doJailbreak   old.serialize);
  # zippers               = dontCheck (overrideCabal old.zippers  (old: {}));

  common          = new.callCabal2nix "common" ../common {};
}
