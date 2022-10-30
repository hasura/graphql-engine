{ mkDerivation, aeson, attoparsec, base, bytestring, hashable
, insert-ordered-containers, lens, lens-aeson, lib, scientific
, text, vector
}:
mkDerivation {
  pname = "aeson-ordered";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable insert-ordered-containers
    lens lens-aeson scientific text vector
  ];
  homepage = "https://github.com/hasura/github-engine-mono#readme";
  license = lib.licenses.bsd3;
}
