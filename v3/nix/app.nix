# This is a function that returns a derivation for the compiled Rust project.
{ craneLib
, lib
, version
, stdenv
, openssl
, libiconv
, pkg-config
, protobuf
, darwin
, pname
, packageName ? pname
}:
let
  buildArgs = {
    inherit pname;

    strictDeps = true;

    src =
      let
        isGraphqlFile = path: _type: builtins.match ".*graphql" path != null;
        isHtmlFile = path: _type: builtins.match ".*html" path != null;
        isJsonFile = path: _type: builtins.match ".*json" path != null;
        isSourceFile = path: type:
          isGraphqlFile path type
          || isHtmlFile path type
          || isJsonFile path type
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

  # only build the binary we care about
  cargoExtraArgs = "--package ${packageName}";

  # Build the dependencies first.
  cargoArtifacts = craneLib.buildDepsOnly (buildArgs //
    {
      inherit cargoExtraArgs;
      doCheck = false;
    }

  );
in
# Then build the crate.
craneLib.buildPackage
  (buildArgs // {
    inherit cargoArtifacts cargoExtraArgs;
    doCheck = false;
    RELEASE_VERSION = version;
  })
