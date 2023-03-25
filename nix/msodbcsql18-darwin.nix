# Copied from nixpkgs/pkgs/development/libraries/unixODBCDrivers/default.nix and modified heavily.
{ fetchurl, stdenv, lib, libiconv, unixODBC, fixDarwinDylibNames }:
stdenv.mkDerivation rec {
  pname = "msodbcsql${versionMajor}";
  version = "${versionMajor}.${versionMinor}.${versionAdditional}";

  versionMajor = "18";
  versionMinor = "2";
  versionAdditional = "1.1";

  src =
    if stdenv.targetPlatform.isAarch64
    then
      fetchurl
        {
          url = "https://download.microsoft.com/download/0/9/7/0976a30f-1710-4754-9445-bead058dd0c8/${pname}-${version}-arm64.tar.gz";
          sha256 = "0q892l90vj6dvk20dvl84a84b28sh2l4a1kyrnxg4lxj5qm3kjbd";
        }
    else
      fetchurl
        {
          url = "https://download.microsoft.com/download/0/9/7/0976a30f-1710-4754-9445-bead058dd0c8/${pname}-${version}-amd64.tar.gz";
          sha256 = "07s2rbkf1n2b0wkzqbx657k0n2y9xl9k5fahr0vwb45xhz2i5ibs";
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
