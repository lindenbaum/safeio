{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-20.09-small.tar.gz) { }
, run ? "bash"
}:
(pkgs.buildFHSUserEnv {
  name = "safeio-build-env";
  targetPkgs = p:
    with p;
    [
      erlang
      rebar3
      gnumake
      gcc
      coreutils
    ];
  runScript = ''    
    ${run}
  '';
  profile = ''
    export XDG_CONFIG_HOME=/tmp/safeio-config
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    export GIT_SSL_CAINFO="$SSL_CERT_FILE"
    export LANG=C.UTF-8
  '';

}).env

