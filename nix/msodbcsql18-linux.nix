# Copied from nixpkgs/pkgs/development/libraries/unixODBCDrivers/default.nix and modified.
{ fetchurl, stdenv, unixODBC, dpkg, lib, openssl, libkrb5, libuuid, patchelf }:
stdenv.mkDerivation rec {
  pname = "msodbcsql${versionMajor}";
  version = "${versionMajor}.${versionMinor}.${versionAdditional}-1";

  versionMajor = "18";
  versionMinor = "2";
  versionAdditional = "1.1";

  src =
    if stdenv.targetPlatform.isAarch64
    then
      fetchurl
        {
          url = "https://packages.microsoft.com/debian/11/prod/pool/main/m/${pname}/${pname}_${version}_arm64.deb";
          sha256 = "0j8zn0s7289nkygqvyl1jmgpbgax64q6x9646xbhfg0j14pjf737";
        }
    else
      fetchurl
        {
          url = "https://packages.microsoft.com/debian/11/prod/pool/main/m/${pname}/${pname}_${version}_amd64.deb";
          sha256 = "0rl2v1cfj2spykhb7nwq0hqxl8izlbzcrx8z4r0xrb12ajb1lz31";
        };

  nativeBuildInputs = [ dpkg patchelf ];

  unpackPhase = "dpkg -x $src ./";
  buildPhase = "";

  installPhase = ''
    mkdir -p $out
    mkdir -p $out/lib
    cp -r opt/microsoft/msodbcsql${versionMajor}/lib64 opt/microsoft/msodbcsql${versionMajor}/share $out/
  '';

  postFixup = ''
    patchelf --set-rpath ${lib.makeLibraryPath [ unixODBC openssl libkrb5 libuuid stdenv.cc.cc ]} \
      $out/lib/libmsodbcsql-${versionMajor}.${versionMinor}.so.${versionAdditional}
  '';

  passthru = {
    fancyName = "ODBC Driver ${versionMajor} for SQL Server";
    driver = "lib/libmsodbcsql-${versionMajor}.${versionMinor}.so.${versionAdditional}";
  };

  meta = with lib; {
    description = "ODBC Driver ${versionMajor} for SQL Server";
    homepage = "https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-2017";
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    platforms = platforms.linux;
    license = licenses.unfree;
  };
}
