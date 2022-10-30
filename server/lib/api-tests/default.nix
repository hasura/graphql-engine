{ mkDerivation, aeson, base, bytestring, dc-api, fast-logger
, graphql-parser, hasura-prelude, hspec, hspec-discover
, http-conduit, http-types, HUnit, lens, lens-aeson, lib
, morpheus-graphql, pg-client, postgresql-simple, safe-exceptions
, sop-core, split, test-harness, text, unordered-containers, uuid
, vector
}:
mkDerivation {
  pname = "api-tests";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring dc-api fast-logger graphql-parser
    hasura-prelude hspec http-conduit http-types HUnit lens lens-aeson
    morpheus-graphql pg-client postgresql-simple safe-exceptions
    sop-core split test-harness text unordered-containers uuid vector
  ];
  executableToolDepends = [ hspec-discover ];
  license = "unknown";
  mainProgram = "api-tests";
}
