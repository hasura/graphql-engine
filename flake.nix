# This Nix flake defines the dependencies of the Hasura GraphQL Engine.
# Currently, we do not use it for building anything, just for providing a shell.
#
# The shell provides tooling for the Server, CLI, Console, and other components.
# You can start it with `nix develop`, or by using `direnv` (see .envrc).
#
# If you are using macOS, note that we apply a patch to GHC, and therefore all
# tooling needs to be built from scratch. If you're averse to waiting several
# hours for your environment to become usable, you can add @SamirTalwar's Cachix
# repository to save yourself the trouble:
#
#     https://app.cachix.org/cache/hasura-samirtalwar#pull
{
  description = "Hasura GraphQL Engine";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
    };

    nixpkgs = {
      url = github:NixOS/nixpkgs;
    };
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
        overlays = [
          (import ./nix/overlays/ghc.nix)
          (import ./nix/overlays/msodbcsql18.nix)
        ];
      };
    in
    {
      packages.graphql-parser = (pkgs.haskell.packages.${pkgs.ghcName}.callCabal2nix "graphql-parser" ./server/lib/graphql-parser-hs { }).overrideScope (
        self: super: {
          hedgehog = self.hedgehog_1_1_1;
        }
      );

      formatter = pkgs.nixpkgs-fmt;

      devShells.default = import ./nix/shell.nix { inherit pkgs; };
    }
    );
}
