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
        isJsonLinesFile = path: _type: builtins.match ".*jsonl" path != null;
        isSourceFile = path: type:
          isGraphqlFile path type
          || isHtmlFile path type
          || isJsonFile path type
          || isJsonLinesFile path type
          || craneLib.filterCargoSources path type;
      in
      lib.cleanSourceWith { src = craneLib.path ./..; filter = isSourceFile; };

    buildInputs = [
      openssl
    ] ++ lib.optionals stdenv.hostPlatform.isDarwin [
      libiconv
    ];

    nativeBuildInputs = [
      pkg-config # required for non-static builds
      protobuf # required by opentelemetry-proto, a dependency of axum-tracing-opentelemetry
    ];
  };

  # helpers function for filtering files for a build to increase cache hits
  filterSrcWithRegexes = regexes: src:
    let
      basePath = toString src + "/";
    in
    lib.cleanSourceWith {
      filter = (path: type:
        let
          relPath = lib.removePrefix basePath (toString path);
          includePath =
            (type == "directory") ||
            lib.any
              (re: builtins.match re relPath != null)
              regexes;
        in
        # uncomment to debug:
          # builtins.trace "${relPath}: ${lib.boolToString includePath}"
        includePath
      );
      inherit src;
    };

  # the default `craneLib.buildDepsOnly` behaviour includes workspace crates (ie
  # `metadata-resolve`) which mean nearly every change invalidates it and it's
  # all built again. Instead, we use the filter to only build external crates,
  # which are less likely to change.
  filterWorkspaceDepsBuildFiles = src: filterSrcWithRegexes [
    "Cargo.lock"
    "Cargo.toml"
    ".cargo"
    ".cargo/.*"
    ".*/Cargo.toml"
  ]
    src;

  # Build the dependencies first.
  cargoArtifacts = craneLib.buildDepsOnly (buildArgs //
    {
      src = filterWorkspaceDepsBuildFiles ../.;
      buildPhaseCargoCommand = "cargo build --profile $CARGO_PROFILE --package ${packageName} --locked";
      doCheck = false;
    }

  );
in
# Then build the crate.
craneLib.buildPackage
  (buildArgs // {
    inherit cargoArtifacts;
    cargoExtraArgs = "--package ${packageName} --locked";
    doCheck = false;
    RELEASE_VERSION = version;
  })
