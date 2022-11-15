self: super:
let
  versions = import ../versions.nix { pkgs = super; };

  ghcVersion = "9.2.5";
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
          {
            # This is due to a GHC 9.2.5 regression.
            # see https://gitlab.haskell.org/ghc/ghc/-/issues/22425
            ListLike = super.haskell.lib.dontCheck hsuper.ListLike;
          } //
          (if super.stdenv.targetPlatform.isDarwin
          then
          # macOS-specific overrides:
            {
              # On aarch64-darwin, this creates a cycle for some reason; didn't look too much into it.
              ghcid = super.haskell.lib.overrideCabal hsuper.ghcid (drv: { enableSeparateBinOutput = false; });
            }
          else
          # We don't need to override anything on Linux:
            { });
      };
    };
  };

  inherit ghcVersion ghcName;
}
