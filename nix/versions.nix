{ pkgs }:
let
  versions = pkgs.lib.trivial.importJSON ../server/VERSIONS.json;
in
{
  ensureVersion = package:
    let expected = versions.${package.pname};
    in
    if expected == package.version
    then package
    else throw "Invalid version for package ${package.pname}: expected ${expected}, got ${package.version}";

  ghcVersion = pkgs.lib.strings.fileContents ../.ghcversion;

  nodejsVersion = pkgs.lib.strings.fileContents ../.nvmrc;
}
