# This is a function that returns a derivation for a docker image.
{ dockerTools
, lib
, package
, image-name
, pkgs
, architecture ? null
, tag ? null # defaults to the output hash
, extraConfig ? { } # see config options at: https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-field-descriptions
}:

let
  args = {
    name = image-name;
    created = "now";
    contents = [ pkgs.cacert pkgs.bash pkgs.coreutils package ];
    config = {
      Entrypoint = [
        "/bin/${package.pname}"
      ];
    } // extraConfig;
  }
  // lib.optionalAttrs (tag != null) {
    inherit tag;
  } // lib.optionalAttrs (architecture != null) {
    inherit architecture;
  };
in
dockerTools.buildLayeredImage args
