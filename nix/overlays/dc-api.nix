final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ${prev.ghcName} = prev.haskell.packages.${prev.ghcName}.override (old: {
        overrides = prev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (hfinal: hprev: {
            # Tests don't compile as extra-source-files are missing
            dc-api = prev.haskell.lib.dontCheck (final.haskell.packages.${prev.ghcName}.callCabal2nix "dc-api" ../../server/lib/dc-api { });
          });
      });
    };
  };
}
