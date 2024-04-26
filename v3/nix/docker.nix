# This is a function that returns a derivation for a docker image.
{ dockerTools
, lib
, package
, image-name
, port
, architecture ? null
, tag ? null # defaults to the output hash
, extraConfig ? { } # see config options at: https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-field-descriptions
}:

let
  seconds = 1000 * 1000 * 1000; # nanoseconds in 1 second
  args = {
    name = image-name;
    created = "now";
    contents = [ package ];
    config = {
      Entrypoint = [
        "/bin/${package.pname}"
      ];
      ExposedPorts = { "${port}/tcp" = { }; };
    } // extraConfig;
  }
  // lib.optionalAttrs (tag != null) {
    inherit tag;
  } // lib.optionalAttrs (architecture != null) {
    inherit architecture;
  };
in
dockerTools.buildLayeredImage args
