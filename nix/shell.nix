{ pkgs, system }:
let
  versions = import ./versions.nix { inherit pkgs; };

  # empty package, for shenanigans
  empty = builtins.derivation {
    inherit system;
    name = "empty";
    builder = pkgs.writeShellScript "null.sh" "${pkgs.coreutils}/bin/mkdir $out";
  };

  # Unix ODBC Support
  freetdsWithODBC = pkgs.freetds.override {
    odbcSupport = true;
    inherit unixODBC;
  };

  msodbcsql = pkgs.unixODBCDrivers.msodbcsql18;

  # Verify this works by running `odbcinst -q -d`.
  # The output should be the headings from the odbcinst.ini file.
  # (You can easily see the generated file by running `cat $ODBCINSTINI`.)
  # If you see any errors, please contact your friendly MSSQL and/or Nix expert.
  odbcConfiguration = pkgs.writeTextFile {
    name = "odbc-configuration";
    text = ''
      [${msodbcsql.fancyName}]
      Description = ${msodbcsql.meta.description}
      Driver = ${msodbcsql}/${msodbcsql.driver}
    '';
    destination = "/odbcinst.ini";
  };

  unixODBC = pkgs.unixODBC.overrideAttrs (oldAttrs: {
    configureFlags = [ "--disable-gui" "--sysconfdir=${odbcConfiguration}" ];
  });

  # Ensure that GHC and HLS have access to all the dynamic libraries we have kicking around.
  ghc =
    let original = pkgs.haskell.compiler.${pkgs.ghcName};
    in pkgs.stdenv.mkDerivation
      {
        name = original.name;
        src = empty;
        buildInputs = [ original pkgs.makeWrapper ];
        installPhase = ''
          mkdir -p "$out/bin"
          for bin in ${original}/bin/*; do
            if [[ -x "$bin" ]]; then
              makeWrapper "$bin" "$out/bin/$(basename "$bin")" \
                --set LD_LIBRARY_PATH ${pkgs.lib.strings.makeLibraryPath dynamicLibraries} \
                --set DYLD_LIBRARY_PATH ${pkgs.lib.strings.makeLibraryPath dynamicLibraries}
            fi
          done
        '';
      };

  hls =
    let original = pkgs.haskell.packages.${pkgs.ghcName}.haskell-language-server;
    in pkgs.stdenv.mkDerivation
      {
        name = original.name;
        src = empty;
        buildInputs = [ original pkgs.makeWrapper ];
        installPhase = ''
          mkdir -p "$out/bin"
          makeWrapper ${original}/bin/haskell-language-server "$out/bin/haskell-language-server" \
            --set LD_LIBRARY_PATH ${pkgs.lib.strings.makeLibraryPath dynamicLibraries} \
            --set DYLD_LIBRARY_PATH ${pkgs.lib.strings.makeLibraryPath dynamicLibraries}
          makeWrapper ${original}/bin/haskell-language-server-wrapper "$out/bin/haskell-language-server-wrapper" \
            --set LD_LIBRARY_PATH ${pkgs.lib.strings.makeLibraryPath dynamicLibraries} \
            --set DYLD_LIBRARY_PATH ${pkgs.lib.strings.makeLibraryPath dynamicLibraries}
        '';
      };

  baseInputs = [
    pkgs.stdenv
    pkgs.jq
  ];

  consoleInputs = [
    pkgs.google-cloud-sdk
    pkgs."nodejs-${versions.nodejsVersion}_x"
    pkgs."nodejs-${versions.nodejsVersion}_x".pkgs.typescript-language-server
  ];

  docsInputs = [
    pkgs.yarn
  ];

  integrationTestInputs = [
    pkgs.python3
    pkgs.pyright # Python type checker
  ];

  # The version of GHC in `ghcName` is set in nix/overlays/ghc.nix.
  haskellInputs = [
    pkgs.cabal2nix

    ghc
    hls

    pkgs.haskell.packages.${pkgs.ghcName}.alex
    # pkgs.haskell.packages.${pkgs.ghcName}.apply-refact
    (versions.ensureVersion pkgs.haskell.packages.${pkgs.ghcName}.cabal-install)
    (pkgs.haskell.lib.dontCheck (pkgs.haskell.packages.${pkgs.ghcName}.ghcid))
    pkgs.haskell.packages.${pkgs.ghcName}.happy
    (versions.ensureVersion pkgs.haskell.packages.${pkgs.ghcName}.hlint)
    pkgs.haskell.packages.${pkgs.ghcName}.hoogle
    pkgs.haskell.packages.${pkgs.ghcName}.hspec-discover
    (versions.ensureVersion pkgs.haskell.packages.${pkgs.ghcName}.ormolu_0_7_1_0)
  ];

  devInputs = [
    pkgs.nixpkgs-fmt
    pkgs.shellcheck
    pkgs.terraform
    pkgs.gopls
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
    pkgs.postgresql_16
    unixODBC
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
}
