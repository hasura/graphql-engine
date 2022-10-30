{ mkDerivation, aeson, aeson-casing, async, attoparsec, base
, bytestring, ekg-core, file-embed, hashable, hashtables, hasql
, hasql-pool, hasql-transaction, hspec, lib, mmorph, monad-control
, mtl, postgresql-binary, postgresql-libpq, resource-pool, retry
, safe-exceptions, scientific, tasty-bench, template-haskell, text
, text-builder, time, transformers, transformers-base, uuid, vector
}:
mkDerivation {
  pname = "pg-client";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-casing async attoparsec base bytestring ekg-core
    hashable hashtables mmorph monad-control mtl postgresql-binary
    postgresql-libpq resource-pool retry safe-exceptions scientific
    template-haskell text text-builder time transformers
    transformers-base uuid vector
  ];
  testHaskellDepends = [
    aeson async base bytestring hspec mtl postgresql-libpq
    safe-exceptions time transformers
  ];
  benchmarkHaskellDepends = [
    base bytestring file-embed hasql hasql-pool hasql-transaction
    tasty-bench text transformers
  ];
  homepage = "https://github.com/hasura/pg-client-hs";
  license = lib.licenses.asl20;
}
