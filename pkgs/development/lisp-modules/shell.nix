with import ../../../default.nix {};
let
self = rec {
  name = "ql-to-nix";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    stdenv libffi
    lispPackages.quicklisp-to-nix lispPackages.quicklisp-dists-to-nix
  ];
  LD_LIBRARY_PATH = "${libffi}/lib";
};
in stdenv.mkDerivation self
