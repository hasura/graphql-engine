{ mkDerivation, aeson, aeson-casing, base, base64-bytestring
, bytestring, containers, deepseq, hashable
, insert-ordered-containers, lens, lib, mtl, nonempty-containers
, pretty-simple, text, time, transformers, transformers-base
, unordered-containers, witherable
}:
mkDerivation {
  pname = "hasura-prelude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-casing base base64-bytestring bytestring containers
    deepseq hashable insert-ordered-containers lens mtl
    nonempty-containers pretty-simple text time transformers
    transformers-base unordered-containers witherable
  ];
  license = "unknown";
}
