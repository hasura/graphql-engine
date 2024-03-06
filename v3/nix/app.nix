# This is a function that returns a derivation for the compiled Rust project.
{ craneLib
, lib
, stdenv
, openssl
, libiconv
, pkg-config
, protobuf
, darwin
}:
let
  buildArgs = {
    pname = "v3-engine";

    src =
      let
        isJsonFile = path: _type: builtins.match ".*json" path != null;
        isGraphqlFile = path: _type: builtins.match ".*graphql" path != null;
        isSourceFile = path: type:
          isJsonFile path type
          || isGraphqlFile path type
          || craneLib.filterCargoSources path type;
      in
      lib.cleanSourceWith { src = craneLib.path ./..; filter = isSourceFile; };

    buildInputs = [
      openssl
    ] ++ lib.optionals stdenv.hostPlatform.isDarwin [
      libiconv
      darwin.apple_sdk.frameworks.Security
      darwin.apple_sdk.frameworks.SystemConfiguration
    ];

    nativeBuildInputs = [
      pkg-config # required for non-static builds
      protobuf # required by opentelemetry-proto, a dependency of axum-tracing-opentelemetry
    ];
  };

  # Build the dependencies first.
  cargoArtifacts = craneLib.buildDepsOnly buildArgs;
in
# Then build the crate.
craneLib.buildPackage
  (buildArgs // {
    inherit cargoArtifacts;
    doCheck = false;
  })
