{ pkgs }:
let
  versions = import ./versions.nix { inherit pkgs; };

  # Unix ODBC Support
  freetdsWithODBC = pkgs.freetds.override {
    odbcSupport = true;
    inherit (pkgs) unixODBC;
  };

  # Verify this works by running `odbcinst -q -d`.
  # The output should be the headings from the odbcinst.ini file.
  # (You can easily see the generated file by running `cat $ODBCINSTINI`.)
  # If you see any errors, please contact your friendly MSSQL and/or Nix expert.
  odbcinstFile =
    let msodbcsql = pkgs.unixODBCDrivers.msodbcsql17;
    in
    pkgs.writeTextFile {
      name = "odbcinst.ini";
      text = ''
        [${msodbcsql.fancyName}]
        Description = ${msodbcsql.meta.description}
        Driver = ${msodbcsql}/${msodbcsql.driver}
      '';
    };

  baseInputs = [
    pkgs.stdenv
    pkgs.jq
  ];

  consoleInputs = [
    pkgs.google-cloud-sdk
    pkgs."nodejs-${versions.nodejsVersion}_x"
  ];

  docsInputs = [
    pkgs.yarn
  ];

  integrationTestInputs = [
    pkgs.python3
  ];

  # The version of GHC in `ghcName` is set in nix/overlays/ghc.nix.
  #
  # We list top-level packages before packages scoped to the GHC version, so
  # that they appear first in the PATH. Otherwise we might end up with older
  # versions of transitive dependencies (e.g. HLS depending on Ormolu).
  haskellInputs = [
    pkgs.cabal2nix

    # The correct version of GHC.
    pkgs.haskell.compiler.${pkgs.ghcName}

    # We use the default versions of these packages.
    (versions.ensureVersion pkgs.haskellPackages.hlint)
    (versions.ensureVersion pkgs.haskellPackages.hpack)
    (versions.ensureVersion pkgs.haskellPackages.ormolu)

    # We build these packages using our custom GHC.
    pkgs.haskell.packages.${pkgs.ghcName}.alex
    pkgs.haskell.packages.${pkgs.ghcName}.apply-refact
    pkgs.haskell.packages.${pkgs.ghcName}.cabal-install
    pkgs.haskell.packages.${pkgs.ghcName}.ghcid
    pkgs.haskell.packages.${pkgs.ghcName}.happy
    pkgs.haskell.packages.${pkgs.ghcName}.haskell-language-server
    pkgs.haskell.packages.${pkgs.ghcName}.hoogle
    pkgs.haskell.packages.${pkgs.ghcName}.hspec-discover
  ];

  devInputs = [
    pkgs.nixpkgs-fmt
    pkgs.shellcheck
  ];

  ciInputs = [
    pkgs.go
  ];

  dynamicLibraries = [
    pkgs.gmp
    pkgs.libkrb5 # Includes required `gssapi` headers.
    pkgs.ncurses
    pkgs.openssl_3
    pkgs.pcre
    pkgs.zlib
    pkgs.zstd

    # PostgreSQL, Microsoft SQL Server, & MySQL dependencies.
    freetdsWithODBC
    pkgs.libmysqlclient
    pkgs.mariadb
    pkgs.postgresql
    pkgs.unixODBC
  ]
  # Linux-specific libraries.
  ++ pkgs.lib.optionals pkgs.stdenv.targetPlatform.isLinux [
    pkgs.stdenv.cc.cc.lib
    # Microsoft SQL Server drivers don't work on aarch64 yet.
    pkgs.unixODBCDrivers.msodbcsql17
  ];

  includeLibraries = [
    pkgs.libkrb5.dev
    pkgs.ncurses.dev
    pkgs.openssl_3.dev
    pkgs.pcre.dev
    pkgs.zlib.dev
  ];

  serverDeps =
    haskellInputs
    ++ dynamicLibraries
    ++ includeLibraries
    ++ integrationTestInputs;
in
pkgs.mkShell {
  buildInputs = baseInputs ++ consoleInputs ++ docsInputs ++ serverDeps ++ devInputs ++ ciInputs;

  # We set the ODBCINSTINI to the file defined above, which points to the MSSQL ODBC driver.
  # The path is relative to `ODBCSYSINI`, which we set to empty.
  # Microsoft SQL Server drivers don't work on aarch64 yet, so we also disable this.
  ODBCSYSINI = "";
  ODBCINSTINI = pkgs.lib.strings.optionalString pkgs.stdenv.targetPlatform.isLinux "${odbcinstFile}";

  LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath dynamicLibraries;
  shellHook = pkgs.lib.strings.optionalString pkgs.stdenv.targetPlatform.isDarwin ''
    # Without this, GHC will use the system `libcrypto` and `libssl` libraries, which fail.
    export DYLD_LIBRARY_PATH="$LD_LIBRARY_PATH";
  '';
}
