{ nixpkgs }:
self: super:
let
  versions = import ../versions.nix { pkgs = super; };

  ghcName = "ghc${builtins.replaceStrings ["."] [""] versions.ghcVersion}";
  ghcPatches =
    if super.stdenv.targetPlatform.isDarwin
    then [
      # Copied from https://github.com/NixOS/nixpkgs/pull/149942
      # If the GHC version is updated, we must update the patch too.
      # ---
      # Reverts the linking behavior of GHC to not resolve `-libc++` to `c++`.
      # Without this, we get the following error on macOS:
      #    ghc: loadArchive: Neither an archive, nor a fat archive: `/path/to/clang++'
      ./ghc-9.4-macOS-loadArchive-fix.patch
      # On macOS + aarch64, there's an issue where the `bin` output depends on 
      # the `out` output, and vice versa. This patch fixes the problem.
      # For some reason we don't apply it for GHC 9.4 in nixpkgs, even though
      # it's needed.
      "${nixpkgs}/pkgs/development/compilers/ghc/Cabal-3.6-paths-fix-cycle-aarch64-darwin.patch"
    ] else [ ];
in
{
  haskell = super.haskell // {
    compiler = super.haskell.compiler // {
      ${ghcName} = (versions.ensureVersion super.haskell.compiler.${ghcName}).overrideAttrs (oldAttrs: {
        patches = (if oldAttrs ? patches then oldAttrs.patches else [ ]) ++ ghcPatches;
      });
    };
  };

  inherit (versions) ghcVersion;
  inherit ghcName;
}
