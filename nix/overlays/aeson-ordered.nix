final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ${prev.ghcName} = prev.haskell.packages.${prev.ghcName}.override (old: {
        overrides = prev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (hfinal: hprev: {
            aeson-ordered = final.haskell.packages.${prev.ghcName}.callCabal2nix "aeson-ordered" ../../server/lib/aeson-ordered { };
          });
      });
    };
  };
}
