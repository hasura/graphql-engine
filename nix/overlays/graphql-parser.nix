final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ghc925 = prev.haskell.packages.ghc925.override (old: {
        overrides = prev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (hfinal: hprev: {
            graphql-parser = (final.haskell.packages.ghc925.callCabal2nix "graphql-parser" ../../server/lib/graphql-parser-hs { }).overrideScope (
              final: prev: {
                hedgehog = final.hedgehog_1_2;
              }
            );
          });
      });
    };
  };
}
