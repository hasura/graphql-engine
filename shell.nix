# See flake.nix for commentary before using this.
{ system ? builtins.currentSystem }:
(builtins.getFlake (toString ./.)).devShells.${system}
