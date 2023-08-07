# Fix Ormolu v0.7.0.0.
#
# The wrong versions of certain packages are supplied. This overrides them with
# the correct versions.

self: super:
{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ${self.ghcName} = super.haskell.packages.${self.ghcName}.override {
        overrides = hself: hsuper: {
          ormolu_0_7_1_0 = hsuper.ormolu_0_7_1_0.override {
            Cabal-syntax = hsuper.Cabal-syntax_3_10_1_0;
            ghc-lib-parser = hsuper.ghc-lib-parser_9_6_2_20230523;
          };
        };
      };
    };
  };
}
