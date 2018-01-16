{ mkDerivation
, base
, basement
, bytestring
, deepseq
, foundation
, hspec
, memory
, stdenv
, QuickCheck
}:
mkDerivation {
  pname = "sized-memory";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base
    basement
    bytestring
    deepseq
    foundation
    memory
  ];
  testHaskellDepends = [
    base
    bytestring
    hspec
    memory
    QuickCheck
  ];
  description = "";
  license = stdenv.lib.licenses.unlicense;
}
