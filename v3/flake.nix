{
  description = "DDN Engine";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, crane, rust-overlay }:
    flake-utils.lib.eachDefaultSystem
      (localSystem:
        let
          version = pkgs.lib.strings.fileContents ./nix/version;

          pkgs = import nixpkgs {
            system = localSystem;
            overlays = [ rust-overlay.overlays.default ];
          };

          rust = import ./nix/rust.nix {
            inherit nixpkgs rust-overlay crane localSystem;
          };

          rust-x86_64-linux = (import ./nix/rust.nix {
            inherit nixpkgs rust-overlay crane localSystem;
            crossSystem = "x86_64-linux";
          });

          rust-aarch64-linux = (import ./nix/rust.nix {
            inherit nixpkgs rust-overlay crane localSystem;
            crossSystem = "aarch64-linux";
          });
        in
        {
          formatter = pkgs.nixpkgs-fmt;

          packages = {
            ###### ENGINE

            # engine binary for whichever is the local machine
            engine = rust.callPackage ./nix/app.nix {
              inherit version;
              pname = "engine";
            };

            # engine binary for x86_64-linux
            engine-x86_64-linux = rust-x86_64-linux.callPackage ./nix/app.nix {
              inherit version;
              pname = "engine";
            };

            # engine binary for aarch64-linux
            engine-aarch64-linux = rust-aarch64-linux.callPackage ./nix/app.nix {
              inherit version;
              pname = "engine";
            };

            # engine docker files for whichever is the local machine
            engine-docker = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.engine;
              image-name = "ghcr.io/hasura/v3-engine";
              tag = "dev";
              port = "3000";
            };

            # engine docker for x86_64-linux
            engine-docker-x86_64-linux = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.engine-x86_64-linux;
              architecture = "amd64";
              image-name = "ghcr.io/hasura/v3-engine";
              port = "3000";
            };

            # engine docker for aarch64-linux
            engine-docker-aarch64-linux = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.engine-aarch64-linux;
              architecture = "arm64";
              image-name = "ghcr.io/hasura/v3-engine";
              port = "3000";
            };

            default = self.packages.${localSystem}.engine;

            ###### CUSTOM_CONNECTOR

            # custom-connector binary for whichever is the local machine
            custom-connector = rust.callPackage ./nix/app.nix {
              inherit version;
              pname = "custom-connector";
            };

            # custom-connector binary for x86_64-linux
            custom-connector-x86_64-linux = rust-x86_64-linux.callPackage ./nix/app.nix {
              inherit version;
              pname = "custom-connector";
            };

            # custom-connector binary for aarch64-linux
            custom-connector-aarch64-linux = rust-aarch64-linux.callPackage ./nix/app.nix {
              inherit version;
              pname = "custom-connector";
            };

            # custom-connector docker files for whichever is the local machine
            custom-connector-docker = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.custom-connector;
              image-name = "ghcr.io/hasura/v3-custom-connector";
              tag = "dev";
              port = "8181";
            };

            # custom-connector docker for x86_64-linux
            custom-connector-docker-x86_64-linux = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.custom-connector-x86_64-linux;
              architecture = "amd64";
              image-name = "ghcr.io/hasura/v3-custom-connector";
              port = "8181";
            };

            # custom-connector docker for aarch64-linux
            custom-connector-docker-aarch64-linux = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.custom-connector-aarch64-linux;
              architecture = "arm64";
              image-name = "ghcr.io/hasura/v3-custom-connector";
              port = "8181";
            };

            ###### DEV-AUTH-WEBHOOK

            # dev-auth-webhook binary for whichever is the local machine
            dev-auth-webhook = rust.callPackage ./nix/app.nix {
              inherit version;
              pname = "dev-auth-webhook";
            };

            # dev-auth-webhook binary for x86_64-linux
            dev-auth-webhook-x86_64-linux = rust-x86_64-linux.callPackage ./nix/app.nix {
              inherit version;
              pname = "dev-auth-webhook";
            };

            # dev-auth-webhook binary for aarch64-linux
            dev-auth-webhook-aarch64-linux = rust-aarch64-linux.callPackage ./nix/app.nix {
              inherit version;
              pname = "dev-auth-webhook";
            };

            # dev-auth-webhook docker files for whichever is the local machine
            dev-auth-webhook-docker = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.dev-auth-webhook;
              image-name = "ghcr.io/hasura/v3-dev-auth-webhook";
              tag = "dev";
              port = "3050";
            };

            # dev-auth-webhook docker for x86_64-linux
            dev-auth-webhook-docker-x86_64-linux = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.dev-auth-webhook-x86_64-linux;
              architecture = "amd64";
              image-name = "ghcr.io/hasura/v3-dev-auth-webhook";
              port = "3050";
            };

            # dev-auth-webhook docker for aarch64-linux
            dev-auth-webhook-docker-aarch64-linux = pkgs.callPackage ./nix/docker.nix {
              package = self.packages.${localSystem}.dev-auth-webhook-aarch64-linux;
              architecture = "arm64";
              image-name = "ghcr.io/hasura/v3-dev-auth-webhook";
              port = "3050";
            };

            ### SCRIPTS

            publish-docker-image = pkgs.writeShellApplication {
              name = "publish-docker-image";
              runtimeInputs = with pkgs; [ coreutils skopeo ];
              text = builtins.readFile ./.github/scripts/deploy.sh;
            };
          };

          apps = {
            default = self.apps.${localSystem}.engine;
            engine = flake-utils.lib.mkApp {
              drv = self.packages.${localSystem}.engine;
              name = "engine";
            };
            dev-auth-webhook = flake-utils.lib.mkApp {
              drv = self.packages.${localSystem}.dev-auth-webhook;
              name = "dev-auth-webhook";
            };
            custom-connector = flake-utils.lib.mkApp {
              drv = self.packages.${localSystem}.custom-connector;
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
                pkgs.moreutils
                pkgs.nixpkgs-fmt
                pkgs.nodePackages.prettier

                # Rust
                pkgs.bacon
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
