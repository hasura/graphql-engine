{ system ? builtins.currentSystem
, nixpkgs ? (builtins.getFlake (builtins.toString ../.)).inputs.nixpkgs
}:
import nixpkgs {
  inherit system;
  config = {
    allowUnfree = true;
    permittedInsecurePackages = [
      "nodejs-16.20.0" # until we upgrade our node.js version in .nvmrc
    ];
  };
  overlays = [
    (import ./overlays/ghc.nix)
    (import ./overlays/ormolu.nix)
    (import ./overlays/msodbcsql18.nix)
    (import ./overlays/graphql-parser.nix)
    (import ./overlays/resource-pool.nix)
    (import ./overlays/dc-api.nix)
    (import ./overlays/pg-client-hs.nix)
    (import ./overlays/aeson-ordered.nix)
  ];
}
