final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ${prev.ghcName} = prev.haskell.packages.${prev.ghcName}.override (old: {
        overrides = prev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (hfinal: hprev: {
            pg-client = prev.haskell.lib.dontCheck (final.haskell.packages.${prev.ghcName}.callCabal2nix "pg-client" ../../server/lib/pg-client-hs { });
          });
      });
    };
  };
}
