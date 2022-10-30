{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, conduit, containers, dc-api, ekg-core, fast-logger
, graphql-engine, haskell-src-meta, hasura-prelude, hspec
, hspec-core, http-conduit, http-types, insert-ordered-containers
, lens, lens-aeson, lib, libyaml, morpheus-graphql, mtl
, mysql-simple, network, odbc, openapi3, parsec, pg-client
, postgresql-simple, refined, resourcet, safe-exceptions
, servant-server, sop-core, Spock-core, stm, template-haskell, text
, th-lift, th-lift-instances, time, typed-process
, unordered-containers, uuid, vector, warp, websockets, yaml
}:
mkDerivation {
  pname = "test-harness";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty async base bytestring conduit containers dc-api
    ekg-core fast-logger graphql-engine haskell-src-meta hasura-prelude
    hspec hspec-core http-conduit http-types insert-ordered-containers
    lens lens-aeson libyaml morpheus-graphql mtl mysql-simple network
    odbc openapi3 parsec pg-client postgresql-simple refined resourcet
    safe-exceptions servant-server sop-core Spock-core stm
    template-haskell text th-lift th-lift-instances time typed-process
    unordered-containers uuid vector warp websockets yaml
  ];
  license = "unknown";
}
