# Copied from nixpkgs/pkgs/development/libraries/unixODBCDrivers/default.nix and modified heavily.
{ fetchurl, stdenv, lib, libiconv, unixODBC, fixDarwinDylibNames }:
stdenv.mkDerivation rec {
  pname = "msodbcsql${versionMajor}";
  version = "${versionMajor}.${versionMinor}.${versionAdditional}-1";

  versionMajor = "18";
  versionMinor = "1";
  versionAdditional = "1.1";

  src =
    if stdenv.targetPlatform.isAarch64
    then
      fetchurl
        {
          url = "https://download.microsoft.com/download/6/4/0/64006503-51e3-44f0-a6cd-a9b757d0d61b/msodbcsql18-18.1.1.1-arm64.tar.gz";
          sha256 = "116xl8r2apr5b48jnq6myj9fwqs88yccw5176yfyzh4534fznj5x";
        }
    else
      fetchurl
        {
          url = "https://download.microsoft.com/download/6/4/0/64006503-51e3-44f0-a6cd-a9b757d0d61b/msodbcsql18-18.1.1.1-amd64.tar.gz";
          sha256 = "1fn80byn1yihflznxcm9cpj42mpllnz54apnk9n46vzm2ng2lj6d";
        };

  nativeBuildInputs = [
    # Fix up the names encoded into the dylib, and make them absolute.
    fixDarwinDylibNames
  ];

  installPhase = ''
    mkdir -p $out
    tar xf $src --strip-components=1 -C $out
  '';

  # Replace the hard-coded paths in the dylib with nixpkgs equivalents.
  fixupPhase = ''
    ${stdenv.cc.bintools.targetPrefix}install_name_tool \
      -change /usr/lib/libiconv.2.dylib ${libiconv}/lib/libiconv.2.dylib \
      -change /opt/homebrew/lib/libodbcinst.2.dylib ${unixODBC}/lib/libodbcinst.2.dylib \
      $out/lib/libmsodbcsql.${versionMajor}.dylib
  '';

  passthru = {
    fancyName = "ODBC Driver ${versionMajor} for SQL Server";
    driver = "lib/libmsodbcsql.${versionMajor}.dylib";
  };

  meta = with lib; {
    description = "ODBC Driver ${versionMajor} for SQL Server";
    homepage = "https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-2017";
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    platforms = platforms.darwin;
    license = licenses.unfree;
  };
}
