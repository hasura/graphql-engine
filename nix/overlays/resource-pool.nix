final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ${prev.ghcName} = prev.haskell.packages.${prev.ghcName}.override (old: {
        overrides = prev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (hfinal: hprev: {
            resource-pool = final.haskell.packages.${prev.ghcName}.callCabal2nix "resource-pool" ../../server/lib/pool { };
          });
      });
    };
  };
}
