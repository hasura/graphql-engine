{ mkDerivation, aeson, ansi-terminal, autodocodec
, autodocodec-openapi3, base, bytestring, case-insensitive
, data-fix, deepseq, Diff, directory, extra, file-embed, filepath
, ghc-heap-view, graphql-parser, hashable, hspec, hspec-core
, http-client, http-media, insert-ordered-containers, lens
, lens-aeson, lib, mtl, network-uri, openapi3, optparse-applicative
, scientific, servant, servant-client, servant-client-core
, servant-openapi3, text, time, unordered-containers, vector
, xml-conduit, xml-lens, yaml, zlib
}:
mkDerivation {
  pname = "dc-api";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-openapi3 base bytestring deepseq
    extra ghc-heap-view graphql-parser hashable http-media
    insert-ordered-containers lens lens-aeson openapi3 scientific
    servant servant-client servant-client-core servant-openapi3 text
    unordered-containers
  ];
  testHaskellDepends = [
    aeson ansi-terminal autodocodec autodocodec-openapi3 base
    bytestring case-insensitive data-fix deepseq Diff directory
    file-embed filepath graphql-parser hashable hspec hspec-core
    http-client lens lens-aeson mtl network-uri openapi3
    optparse-applicative scientific servant servant-client
    servant-client-core servant-openapi3 text time unordered-containers
    vector xml-conduit xml-lens yaml zlib
  ];
  license = "unknown";
}
