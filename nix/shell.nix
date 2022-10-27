{ pkgs }:
let
  versions = import ./versions.nix { inherit pkgs; };

  # Unix ODBC Support
  freetdsWithODBC = pkgs.freetds.override {
    odbcSupport = true;
    inherit (pkgs) unixODBC;
  };

  msodbcsql = pkgs.unixODBCDrivers.msodbcsql18;

  # Verify this works by running `odbcinst -q -d`.
  # The output should be the headings from the odbcinst.ini file.
  # (You can easily see the generated file by running `cat $ODBCINSTINI`.)
  # If you see any errors, please contact your friendly MSSQL and/or Nix expert.
  odbcinstFile = pkgs.writeTextFile {
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
    (versions.ensureVersion pkgs.haskellPackages.ormolu)

    # We build these packages using our custom GHC.
    pkgs.haskell.packages.${pkgs.ghcName}.alex
    pkgs.haskell.packages.${pkgs.ghcName}.apply-refact
    pkgs.haskell.packages.${pkgs.ghcName}.cabal-install
    pkgs.haskell.packages.${pkgs.ghcName}.ghcid
    pkgs.haskell.packages.${pkgs.ghcName}.happy
    pkgs.haskell.packages.${pkgs.ghcName}.haskell-language-server
    (versions.ensureVersion pkgs.haskell.packages.${pkgs.ghcName}.hlint)
    (versions.ensureVersion pkgs.haskell.packages.${pkgs.ghcName}.hpack)
    pkgs.haskell.packages.${pkgs.ghcName}.hoogle
    pkgs.haskell.packages.${pkgs.ghcName}.hspec-discover
  ];

  devInputs = [
    pkgs.nixpkgs-fmt
    pkgs.shellcheck
  ];

  ciInputs = [
    pkgs.go
    pkgs.gox
  ];

  dynamicLibraries = [
    pkgs.gmp
    pkgs.libkrb5 # Includes required `gssapi` headers.
    pkgs.libiconv
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
    msodbcsql
  ]
  # Linux-specific libraries.
  ++ pkgs.lib.optionals pkgs.stdenv.targetPlatform.isLinux [
    pkgs.stdenv.cc.cc.lib
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
  ODBCSYSINI = "";
  ODBCINSTINI = "${odbcinstFile}";

  LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath dynamicLibraries;
  shellHook = pkgs.lib.strings.optionalString pkgs.stdenv.targetPlatform.isDarwin ''
    # Without this, GHC will use the system `libcrypto` and `libssl` libraries, which fail.
    export DYLD_LIBRARY_PATH="$LD_LIBRARY_PATH";
  '';
}
