self: super: {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ${super.ghcName} = super.haskell.packages.${super.ghcName}.override {
        overrides = hself: hsuper: {
          # NOTE: avoiding IFD's/automatically generated derivations in favor of cabal2nix generated default.nix
          aeson-ordered = hsuper.callPackage ../../server/lib/aeson-ordered { };
          api-tests = hsuper.callPackage ../../server/lib/api-tests { };
          # TODO: some typed hole errors ATM + tests don't compile as extra-source-files are missing
          dc-api = super.haskell.lib.dontCheck
            (hsuper.callPackage ../../server/lib/dc-api { });
          hasura-error-message =
            hsuper.callPackage ../../server/lib/error-message { };
          graphql-parser =
            hsuper.callPackage ../../server/lib/graphql-parser-hs {
              hedgehog = hself.hedgehog_1_2;
            };
          hasura-prelude =
            hsuper.callPackage ../../server/lib/hasura-prelude { };
          pg-client-hs = super.haskell.lib.dontCheck (hsuper.callPackage ../../server/lib/pg-client-hs {
            resource-pool = hself.hasura-resource-pool;
          });

          hasura-schema-parsers =
            hsuper.callPackage ../../server/lib/schema-parsers { };
          test-harness = hsuper.callPackage ../../server/lib/test-harness { };

          # FIXME: dependency issues with: ekg-json, immortal <0.3
          graphql-server = super.haskell.lib.dontCheck
            (super.haskell.lib.doJailbreak (hsuper.callPackage ../../server {
              hedgehog = hself.hedgehog_1_2;
              immortal = hself.immortal_0_2_2_1;
              pg-client = hself.pg-client-hs;
              resource-pool = hself.hasura-resource-pool;
            }));

          # FIXME: for remote repos we have to calculate their default.nix until they get upstreamed/maintained
          ekg-core = hsuper.callCabal2nix "ekg-core" (super.fetchFromGitHub {
            owner = "hasura";
            repo = "ekg-core";
            rev = "b0cdc337ca2a52e392d427916ba3e28246b396c0";
            sha256 = "5hdk6OA6fmXFYxt69mwlFDzCd/sxQIm3kc+NreJBy+s=";
          }) { };

          # https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/
          ekg-prometheus = super.haskell.lib.doJailbreak
            (hsuper.callCabal2nix "ekg-prometheus" (super.fetchFromGitHub {
              owner = "hasura";
              repo = "ekg-prometheus";
              rev = "131bd4fe72764377d2dae098df0c0e35e3b09982";
              sha256 = "15EqVL+5AWz3588Ilb9dAgb+i9LaUhBEXHZ/f8jp8/4=";
            }) { });

          kriti-lang = hsuper.callCabal2nix "kriti-lang"
            (super.fetchFromGitHub {
              owner = "hasura";
              repo = "kriti-lang";
              rev = "v0.3.3";
              sha256 = "EYdbIiPDFsPGJi7w1nBIW5LLQc9rJxgCr24BGd03eyE=";
            }) { };

          # https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/
          ekg-json = hsuper.callCabal2nix "ekg-json" (super.fetchFromGitHub {
            owner = "hasura";
            repo = "ekg-json";
            rev = "1fab6837e1dd98317a3c2f5bf9deedf4dfcac51b";
            sha256 = "V+doY0SjMqouPt6azS63hAlGFtr9m4fs0K6ghEF6JTQ=";
          }) { };

          ci-info = hsuper.callCabal2nix "ci-info" (super.fetchFromGitHub {
            owner = "hasura";
            repo = "ci-info-hs";
            rev = "be578a01979fc95137cc2c84827f9fafb99df60f";
            sha256 = "m2mxYqQphXeiu9YyZ3RgyRT9xDEIT52ScI7vSWqvYFc=";
          }) { };

          hasura-resource-pool = hsuper.callCabal2nix "resource-pool"
            (super.fetchFromGitHub {
              owner = "hasura";
              repo = "pool";
              rev = "c5faf9a358e83eaf15fef0c1e890f463d0022565";
              sha256 = "a8dzt1f/TwVG37rOsL/Bh2K90cDnGgj7HVpL0S3r59A=";
            }) { };

          # broken dependency fixe
          # openapi3-3.2.2: lens >=4.16.1 && <5.2
          openapi3 = super.haskell.lib.doJailbreak hsuper.openapi3;
          servant-openapi3 =
            super.haskell.lib.doJailbreak hsuper.servant-openapi3;
        };
      };
    };
  };
}
