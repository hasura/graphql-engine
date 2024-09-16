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
    flake-utils.lib.eachDefaultSystem (localSystem:
      let
        version = pkgs.lib.strings.fileContents ./nix/version;

        targetSystems = flake-utils.lib.defaultSystems;

        dockerArchitectures = {
          "x86_64-linux" = "amd64";
          "aarch64-linux" = "arm64";
        };

        pkgs = import nixpkgs {
          system = localSystem;
          overlays = [ rust-overlay.overlays.default ];
        };

        rust = pkgs.lib.attrsets.genAttrs targetSystems (crossSystem: import ./nix/rust.nix {
          inherit nixpkgs rust-overlay crane localSystem crossSystem;
        });

        binaries = [
          "engine"
          "custom-connector"
          "dev-auth-webhook"
        ];

        binaryPackages = {
        };

        defaultBinary = "engine";

        dockerConfig = {
          "engine" = {
            ExposedPorts = { "3000/tcp" = { }; };
          };
          "custom-connector" = {
            ExposedPorts = { "8102/tcp" = { }; };
          };
          "dev-auth-webhook" = {
            ExposedPorts = { "3050/tcp" = { }; };
          };
        };
      in
      {
        formatter = pkgs.nixpkgs-fmt;

        # Generate packages in a tree structure.
        #
        # The path is of the form:
        #
        #   local system -> binary name -> target system -> output
        #
        # For example, to cross-compile the engine to aarch64-linux on a
        # x86_64-linux machine, you will want to access:
        #
        #   targets.x86_64-linux.engine.aarch64-linux.binary
        #
        # Docker images are named "docker", and live next to the binaries. They
        # are not provided for all systems; if there is no Docker image, the
        # value will be `null`.
        #
        # The tree looks something like this:
        #
        #   targets
        #     - x86_64-linux
        #        - engine
        #          - x86_64-linux
        #            - binary
        #            - docker
        #          - aarch64-linux
        #            - binary
        #            - docker
        #          - x86_64-darwin
        #            - binary
        #            - docker = null
        #          - aarch64-darwin
        #            - binary
        #            - docker = null
        #        - custom-connector
        #          - ...
        #     - aarch64-linux
        #        - ...
        #     - x86_64-darwin
        #        - ...
        #     - aarch64-darwin
        #        - ...
        targets = pkgs.lib.attrsets.genAttrs binaries (binaryName:
          pkgs.lib.attrsets.genAttrs targetSystems (targetSystem: {
            binary = rust.${targetSystem}.callPackage ./nix/app.nix {
              inherit version;
              pname = binaryName;
              packageName = binaryPackages.${binaryName} or binaryName;
            };
            docker =
              if dockerArchitectures ? ${targetSystem}
              then
                pkgs.callPackage ./nix/docker.nix
                  {
                    package = self.targets.${localSystem}.${binaryName}.${targetSystem}.binary;
                    architecture = dockerArchitectures.${targetSystem};
                    image-name = "build.internal/${binaryName}-${targetSystem}";
                    extraConfig = dockerConfig.${binaryName} or { };
                  }
              else null;
          })
        );

        packages =
          # binaries for the local system
          pkgs.lib.attrsets.genAttrs binaries (binaryName: self.targets.${localSystem}.${binaryName}.${localSystem}.binary) // {
            # `nix build` will build the default binary
            default = self.packages.${localSystem}.${defaultBinary};

            # a handy script to publish Docker images
            publish-docker-image = pkgs.writeShellApplication {
              name = "publish-docker-image";
              runtimeInputs = with pkgs; [ coreutils skopeo ];
              text = builtins.readFile ./.github/scripts/deploy.sh;
            };
          };

        # build an app per binary
        apps = pkgs.lib.attrsets.genAttrs binaries
          (binaryName:
            flake-utils.lib.mkApp {
              drv = self.packages.${localSystem}.${binaryName};
              name = binaryName;
            }
          ) // {
          # the default app is the app wrapping the default binary
          default = self.apps.${localSystem}.${defaultBinary};
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
              pkgs.nodejs_22

              # Rust
              pkgs.bacon
              pkgs.cargo-edit
              pkgs.cargo-expand
              pkgs.cargo-flamegraph
              pkgs.cargo-insta
              pkgs.cargo-machete
              pkgs.cargo-nextest
              pkgs.cargo-watch
              rust.${localSystem}.rustToolchain
            ];
          };
        };
      });
}
