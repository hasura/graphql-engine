{ mkDerivation, aeson, aeson-casing, aeson-ordered, aeson-pretty
, aeson-qq, ansi-wl-pprint, asn1-encoding, asn1-types, async
, attoparsec, attoparsec-iso8601, auto-update, autodocodec
, autodocodec-openapi3, barbies, base, base16-bytestring
, base64-bytestring, byteorder, bytestring, case-insensitive
, cereal, ci-info, connection, containers, cron, cryptohash-md5
, cryptonite, data-default, data-default-class, data-has, dc-api
, deepseq, dependent-map, dependent-sum, dependent-sum-template
, directory, either, ekg-core, ekg-json, ekg-prometheus, exceptions
, fast-logger, file-embed, filepath, free, ghc-heap-view
, graphql-parser, hashable, hasura-error-message, hasura-prelude
, hasura-schema-parsers, hasura-resource-pool, hedgehog, hspec, hspec-core
, hspec-discover, hspec-expectations, hspec-expectations-json
, hspec-expectations-lifted, hspec-hedgehog, http-api-data
, http-client, http-client-tls, http-conduit, http-media
, http-types, HUnit, immortal, insert-ordered-containers, jose
, kan-extensions, kriti-lang, lens, lens-aeson, lib, lifted-async
, lifted-base, list-t, memory, mime-types, mmorph, monad-control
, monad-loops, monad-validate, mtl, mustache, mysql, mysql-simple
, natural-transformation, network, network-bsd, network-uri
, nonempty-containers, odbc, openapi3, optparse-applicative, parsec
, pem, pg-client, postgresql-binary, postgresql-libpq
, pretty-simple, process, profunctors, psqueues, QuickCheck
, quickcheck-instances, random, refined, regex-tdfa, resource-pool
, retry, safe, safe-exceptions, scientific, semialign, semigroups
, semver, servant, servant-client, servant-client-core
, servant-openapi3, shakespeare, some, split, Spock-core, stm
, stm-containers, tagged, template-haskell, text, text-builder
, text-conversions, th-lift, these, time, time-compat, tls
, transformers, transformers-base, unix, unordered-containers
, uri-encode, url, utf8-string, uuid, validation, vector
, vector-instances, wai, warp, websockets, wide-word, witch
, witherable, wreq, x509, x509-store, x509-system, x509-validation
, yaml, zlib
}:
mkDerivation {
  pname = "graphql-engine";
  version = "2.13.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing aeson-ordered ansi-wl-pprint asn1-encoding
    asn1-types async attoparsec attoparsec-iso8601 auto-update
    autodocodec autodocodec-openapi3 barbies base base16-bytestring
    base64-bytestring byteorder bytestring case-insensitive cereal
    ci-info connection containers cron cryptohash-md5 cryptonite
    data-default data-default-class data-has dc-api deepseq
    dependent-map dependent-sum dependent-sum-template directory either
    ekg-core ekg-json ekg-prometheus exceptions fast-logger file-embed
    filepath free ghc-heap-view graphql-parser hashable
    hasura-error-message hasura-prelude hasura-schema-parsers
    hasura-resource-pool
    http-api-data http-client http-client-tls http-conduit http-media
    http-types immortal insert-ordered-containers jose kan-extensions
    kriti-lang lens lens-aeson lifted-async lifted-base list-t memory
    mime-types mmorph monad-control monad-loops monad-validate mtl
    mustache mysql mysql-simple network network-bsd network-uri
    nonempty-containers odbc openapi3 optparse-applicative parsec pem
    pg-client postgresql-binary postgresql-libpq pretty-simple process
    profunctors psqueues QuickCheck quickcheck-instances random refined
    regex-tdfa resource-pool retry safe-exceptions scientific semialign
    semigroups semver servant servant-client servant-client-core
    servant-openapi3 shakespeare some split Spock-core stm
    stm-containers tagged template-haskell text text-builder
    text-conversions th-lift these time time-compat tls transformers
    transformers-base unix unordered-containers uri-encode url
    utf8-string uuid validation vector vector-instances wai warp
    websockets wide-word witch witherable wreq x509 x509-store
    x509-system x509-validation yaml zlib
  ];
  executableHaskellDepends = [
    aeson-pretty base bytestring ekg-core ekg-prometheus hasura-prelude
    kan-extensions pg-client refined text text-conversions time unix
  ];
  testHaskellDepends = [
    aeson aeson-casing aeson-ordered aeson-pretty aeson-qq
    ansi-wl-pprint asn1-encoding asn1-types async attoparsec
    attoparsec-iso8601 auto-update autodocodec autodocodec-openapi3
    barbies base base16-bytestring base64-bytestring byteorder
    bytestring case-insensitive cereal ci-info connection containers
    cron cryptohash-md5 cryptonite data-default data-default-class
    data-has dc-api deepseq dependent-map dependent-sum
    dependent-sum-template directory either ekg-core ekg-json
    ekg-prometheus exceptions fast-logger file-embed filepath free
    ghc-heap-view graphql-parser hashable hasura-error-message
    hasura-prelude hasura-schema-parsers hasura-resource-pool hedgehog hspec hspec-core
    hspec-expectations hspec-expectations-json
    hspec-expectations-lifted hspec-hedgehog http-api-data http-client
    http-client-tls http-conduit http-media http-types HUnit immortal
    insert-ordered-containers jose kan-extensions kriti-lang lens
    lens-aeson lifted-async lifted-base list-t memory mime-types mmorph
    monad-control monad-loops monad-validate mtl mustache mysql
    mysql-simple natural-transformation network network-bsd network-uri
    nonempty-containers odbc openapi3 optparse-applicative parsec pem
    pg-client postgresql-binary postgresql-libpq pretty-simple process
    profunctors psqueues QuickCheck quickcheck-instances random refined
    regex-tdfa resource-pool retry safe safe-exceptions scientific
    semialign semigroups semver servant servant-client
    servant-client-core servant-openapi3 shakespeare some split
    Spock-core stm stm-containers tagged template-haskell text
    text-builder text-conversions th-lift these time time-compat tls
    transformers transformers-base unix unordered-containers uri-encode
    url utf8-string uuid validation vector vector-instances wai warp
    websockets wide-word witch witherable wreq x509 x509-store
    x509-system x509-validation yaml zlib
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://www.hasura.io";
  description = "GraphQL API over Postgres";
  license = lib.licenses.asl20;
}
