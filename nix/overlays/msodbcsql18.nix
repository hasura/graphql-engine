self: super: {
  unixODBCDrivers = super.unixODBCDrivers // {
    msodbcsql18 =
      if super.stdenv.targetPlatform.isDarwin
      then super.callPackage ../msodbcsql18-darwin.nix { }
      else super.callPackage ../msodbcsql18-linux.nix { };
  };
}
