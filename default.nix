{ mkDerivation, base, hspec, lib, parsec }:
mkDerivation {
  pname = "language-js";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [ base parsec ];
  testHaskellDepends = [ base hspec parsec ];
  description = "javascript parser for es6 and es7";
  license = lib.licenses.mit;
}
