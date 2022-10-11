self: super:
let
  versions = import ../versions.nix { pkgs = super; };

  ghcVersion = "9.2.4";
  ghcName = "ghc${builtins.replaceStrings ["."] [""] ghcVersion}";
  ghcPatches =
    if super.stdenv.targetPlatform.isDarwin
    then [
      # Copied from https://github.com/NixOS/nixpkgs/pull/149942
      # If the GHC version is updated, we must update the patch URL too.
      # ---
      # Reverts the linking behavior of GHC to not resolve `-libc++` to `c++`.
      # Without this, we get the following error on macOS:
      #    ghc: loadArchive: Neither an archive, nor a fat archive: `/path/to/clang++'
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/input-output-hk/haskell.nix/4b6ee9767daaf5fc1d5419e07733ab006b95ec93/overlays/patches/ghc/ghc-9.2-macOS-loadArchive-fix.patch";
        sha256 = "qOfIOqI5oM9695iWtK//OoP7NeF9K6ykGsdSOWJQN/I=";
      })
    ] else [ ];
in
{
  haskell = super.haskell // {
    compiler = super.haskell.compiler // {
      ${ghcName} = (versions.ensureVersion super.haskell.compiler.${ghcName}).overrideAttrs (oldAttrs: {
        patches = (if oldAttrs ? patches then oldAttrs.patches else [ ]) ++ ghcPatches;
      });
    };

    packages = super.haskell.packages // {
      ${ghcName} = super.haskell.packages.${ghcName}.override {
        overrides = hself: hsuper:
          if super.stdenv.targetPlatform.isDarwin
          then
          # macOS-specific overrides:
            let
              # Ormolu v0.5.0.1 doesn't build correctly on aarch64-darwin.
              # Disabling the "fixity-th" flag seems to fix it.
              # https://github.com/tweag/ormolu/issues/927
              fixOrmolu = p: super.lib.pipe p [
                (super.haskell.lib.compose.addExtraLibrary hself.file-embed)
                (super.haskell.lib.compose.disableCabalFlag "fixity-th")
              ];
            in
            {
              # On aarch64-darwin, this creates a cycle for some reason; didn't look too much into it.
              ghcid = super.haskell.lib.overrideCabal hsuper.ghcid (drv: { enableSeparateBinOutput = false; });
              # See above.
              ormolu = hself.ormolu_0_5_0_1;
              ormolu_0_5_0_1 = fixOrmolu hsuper.ormolu_0_5_0_1;
              fourmolu = hself.fourmolu_0_8_2_0;
              fourmolu_0_8_2_0 = fixOrmolu hsuper.fourmolu_0_8_2_0;
            }
          else
          # We don't need to override anything on Linux:
            { };
      };
    };
  };

  inherit ghcVersion ghcName;
}
