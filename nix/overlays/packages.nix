self: super: {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ${super.ghcName} = super.haskell.packages.${super.ghcName}.override {
        overrides = hself: hsuper: {
          # avoiding IFD's in favor of cabal2nix generated default.nix
          aeson-ordered = hsuper.callPackage ../../server/lib/aeson-ordered { };
          api-tests = hsuper.callPackage ../../server/lib/api-tests { };
          dc-api = hsuper.callPackage ../../server/lib/dc-api { };
          error-message = hsuper.callPackage ../../server/lib/error-message { };
          graphql-parser-hs =
            hsuper.callPackage ../../server/lib/graphql-parser-hs { };
          hasura-prelude =
            hsuper.callPackage ../../server/lib/hasura-prelude { };
          pg-client-hs = hsuper.callPackage ../../server/lib/pg-client-hs { };
          schema-parsers =
            hsuper.callPackage ../../server/lib/schema-parsers { };
          test-harness = hsuper.callPackage ../../server/lib/test-harness { };
          graphql-server = hsuper.callPackage ../../server { };
        };
      };
    };
  };
}
