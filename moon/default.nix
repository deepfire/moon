{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "moon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/deepfire/moon";
  description = "Experiments";
  license = stdenv.lib.licenses.gpl3;
}
