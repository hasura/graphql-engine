{ mkDerivation, aeson, attoparsec, base, bytestring, deepseq
, hashable, hedgehog, lib, prettyprinter, scientific, tasty-bench
, template-haskell, text, text-builder, th-compat
, th-lift-instances, unordered-containers
}:
mkDerivation {
  pname = "graphql-parser";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring deepseq hashable hedgehog
    prettyprinter scientific template-haskell text text-builder
    th-compat th-lift-instances unordered-containers
  ];
  testHaskellDepends = [
    base bytestring hedgehog prettyprinter text text-builder
  ];
  benchmarkHaskellDepends = [
    base bytestring prettyprinter tasty-bench text text-builder
  ];
  homepage = "https://github.com/hasura/graphql-parser-hs";
  description = "A native Haskell GraphQL parser";
  license = lib.licenses.bsd3;
}
