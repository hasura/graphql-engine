final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ${prev.ghcName} = prev.haskell.packages.${prev.ghcName}.override (old: {
        overrides = prev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (hfinal: hprev: {
            graphql-parser = (final.haskell.packages.${prev.ghcName}.callCabal2nix "graphql-parser" ../../server/lib/graphql-parser-hs { }).overrideScope (
              final: prev: {
                hedgehog = final.hedgehog_1_2;
              }
            );
          });
      });
    };
  };
}
