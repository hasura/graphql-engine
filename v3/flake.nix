{
  description = "DDN Engine";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;

    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, crane, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (localSystem:
      let
        pkgs = import nixpkgs {
          system = localSystem;
          overlays = [ rust-overlay.overlays.default ];
        };

        rust = import ./nix/rust.nix {
          inherit nixpkgs rust-overlay crane localSystem;
        };
      in
      {
        formatter = pkgs.nixpkgs-fmt;

        packages = {
          # a binary for whichever is the local computer
          default = rust.callPackage ./nix/app.nix {
            version = if self ? "dirtyRev" then self.dirtyShortRev else self.shortRev;
          };
        };

        apps = {
          default = self.apps.${localSystem}.engine;
          engine = flake-utils.lib.mkApp {
            drv = self.packages.${localSystem}.default;
            name = "engine";
          };
          custom-connector = flake-utils.lib.mkApp {
            drv = self.packages.${localSystem}.default;
            name = "custom-connector";
          };
        };

        devShells = {
          default = pkgs.mkShell {
            # include dependencies of the default package
            inputsFrom = [ self.packages.${localSystem}.default ];

            # build-time inputs
            nativeBuildInputs = [
              # Development
              pkgs.just
              pkgs.nixpkgs-fmt

              # Rust
              pkgs.cargo-edit
              pkgs.cargo-expand
              pkgs.cargo-flamegraph
              pkgs.cargo-insta
              pkgs.cargo-machete
              pkgs.cargo-nextest
              pkgs.cargo-watch
              rust.rustToolchain
            ];
          };
        };
      });
}
