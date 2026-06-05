# This is a function that returns a derivation for a docker image.
{ dockerTools
, lib
, runCommand
, busybox
, jq
, inotify-tools
, package
, image-name
, pkgs
, architecture ? null
, tag ? null # defaults to the output hash
, extraContents ? [ ] # extra packages to include in this Docker image
, extraConfig ? { } # see config options at: https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-field-descriptions
, envLoader ? false # when true, wrap the entrypoint with the env-loader script
}:

let
  # A derivation that places the vendored docker-entrypoint.sh at the path the
  # upstream lux-env-loader-entrypoint image uses (/usr/local/bin). Vendored so
  # we don't need to pull a remote image at build time.
  envLoaderEntrypoint = runCommand "env-loader-entrypoint" { } ''
    mkdir -p $out/usr/local/bin
    install -m 0755 ${./docker-entrypoint.sh} $out/usr/local/bin/docker-entrypoint.sh
  '';

  # When env-loader is enabled, auto-include the runtime deps the script needs:
  # busybox supplies /bin/sh, jq parses the secret JSON files, and inotify-tools
  # provides inotifywait for ENABLE_AUTO_RESTART_ON_SECRET_CHANGE.
  envLoaderContents = lib.optionals envLoader [
    envLoaderEntrypoint
    busybox
    jq
    inotify-tools
  ];

  defaultEntrypoint =
    if envLoader
    then [ "/usr/local/bin/docker-entrypoint.sh" "/bin/${package.pname}" ]
    else [ "/bin/${package.pname}" ];

  args = {
    name = image-name;
    created = "now";
    contents = [ package ] ++ extraContents ++ envLoaderContents;
    config = {
      Entrypoint = defaultEntrypoint;
    } // extraConfig;
  }
  // lib.optionalAttrs (tag != null) {
    inherit tag;
  } // lib.optionalAttrs (architecture != null) {
    inherit architecture;
  };
in
dockerTools.buildLayeredImage args
