{ system ? builtins.currentSystem
, nixpkgs ? (builtins.getFlake (builtins.toString ../.)).inputs.nixpkgs
}:
import nixpkgs {
  inherit system;
  config = {
    allowUnfree = true;
  };
  overlays = [
    (import ./overlays/ghc.nix)
    (import ./overlays/msodbcsql18.nix)
    (import ./overlays/graphql-parser.nix)
  ];
}
