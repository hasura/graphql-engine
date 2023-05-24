# Upgrade Ormolu to v0.7.0.0.
#
# We start with v0.6.0.1, upgrade its `src`, and override dependencies to make
# the version constraints happy.
#
# We build it with GHC 9.4 because it bundles text v2, which is required for
# Ormolu v0.5.3 and up. This means we can't just override the Hackage package;
# instead, we override the root-level package and use that directly.
#
# Because of this, HLS will use the wrong Ormolu version. We can resolve this
# once we upgrade to GHC 9.4 *and* a version of hls-ormolu-plugin is released
# which supports this version of Ormolu (at the time of writing, only v0.5.x
# and older are supported).

self: super:
let
  overridden = super.haskell.packages.ghc94.override {
    overrides = hself: hsuper: {
      ormolu = (super.haskell.lib.overrideSrc hsuper.ormolu_0_6_0_1 rec {
        version = "0.7.0.0";
        src = super.fetchurl {
          url = "mirror://hackage/ormolu-${version}.tar.gz";
          sha256 = "07bwcki2xp2g5q3jpll8675yawr1x6nk3zg1yns8mdw085a98g7s";
        };
      }).override {
        "Cabal-syntax" = hsuper."Cabal-syntax_3_10_1_0";
        "ghc-lib-parser" = hsuper."ghc-lib-parser_9_6_1_20230312";
      };
    };
  };
in
{
  ormolu = overridden.ormolu;
}
