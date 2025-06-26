# Sets up our Rust toolchain and Crane for cross-compilation.
# This is mostly a copy of the example provided at:
# https://crane.dev/examples/cross-rust-overlay.html
{ nixpkgs
, rust-overlay
, crane
, localSystem
, crossSystem ? localSystem
}:
let
  pkgs = import nixpkgs {
    inherit crossSystem localSystem;
    overlays = [ rust-overlay.overlays.default ];
  };

  lib = pkgs.pkgsBuildHost.lib;

  # Converts host system string for use in environment variable names
  envCase = triple: lib.strings.toUpper (builtins.replaceStrings [ "-" ] [ "_" ] triple);

  # `hostPlatform` is the cross-compilation output platform;
  # `buildPlatform` is the platform we are compiling on
  buildPlatform = pkgs.stdenv.buildPlatform;
  hostPlatform = pkgs.stdenv.hostPlatform;

  # nixpkgs decided to change the name of the darwin targets which wrecks everything
  hostPlatformName =
    if hostPlatform.config == "arm64-apple-darwin"
    then "aarch64-apple-darwin"
    else hostPlatform.config;

  # When possibly cross-compiling we get several versions of nixpkgs of the
  # form, `pkgs.pkgs<where it runs><platform it produces outputs for>`. We use
  # `pkgs.pkgsBuildHost` to get packages that run at build time (so run on the
  # build platform), and that produce outputs for the host platform which is the
  # cross-compilation target.
  rustBin = pkgs.pkgsBuildHost.rust-bin.fromRustupToolchainFile ../rust-toolchain.toml;
  rustToolchain = rustBin.override { targets = [ hostPlatformName ]; };
  craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

  buildEnv = {
    CARGO_BUILD_TARGET = hostPlatformName;
    "CARGO_TARGET_${envCase hostPlatformName}_LINKER" = "${pkgs.stdenv.cc.targetPrefix}cc";

    # This environment variable may be necessary if any of your dependencies use
    # a build-script which invokes the `cc` crate to build some other code. The
    # `cc` crate should automatically pick up on our target-specific linker
    # above, but this may be necessary if the build script needs to compile and
    # run some extra code on the build system.
    HOST_CC = "${pkgs.stdenv.cc.nativePrefix}cc";
  };
in
{
  inherit rustToolchain;

  callPackage = (package: args:
    # Call the package, providing `craneLib` as an extra.
    let crate = pkgs.callPackage package (args // { inherit craneLib; });
    in
    # Override the derivation to add cross-compilation environment variables.
    crate.overrideAttrs (previous: buildEnv // {
      # We also have to override the `cargoArtifacts` derivation with the same changes.
      cargoArtifacts = previous.cargoArtifacts.overrideAttrs (previous: buildEnv);
    }));
}
