final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ghc925 = prev.haskell.packages.ghc925.override (old: {
        overrides = prev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (hfinal: hprev: {
            resource-pool = final.haskell.packages."${prev.ghcName}".callCabal2nix "resource-pool" ../../server/lib/pool { };
          });
      });
    };
  };
}
