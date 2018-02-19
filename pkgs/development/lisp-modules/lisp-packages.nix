{stdenv, clwrapper, pkgs, sbcl, coreutils, nix, asdf}:
let lispPackages = rec {
  inherit pkgs clwrapper stdenv;
  nixLib = pkgs.lib;
  callPackage = nixLib.callPackageWith lispPackages;

  buildLispPackage =  callPackage ./define-package.nix;

  quicklisp = buildLispPackage rec {
    baseName = "quicklisp";
    version = "2017-03-06";

    buildSystems = [];

    description = "The Common Lisp package manager";
    deps = [];
    src = pkgs.fetchgit {
      url = "https://github.com/quicklisp/quicklisp-client/";
      rev = "refs/tags/version-${version}";
      sha256 = "11ywk7ggc1axivpbqvrd7m1lxsj4yp38d1h9w1d8i9qnn7zjpqj4";
    };
    overrides = x: rec {
      inherit clwrapper;
      quicklispdist = pkgs.fetchurl {
        # Will usually be replaced with a fresh version anyway, but needs to be
        # a valid distinfo.txt
        url = "http://beta.quicklisp.org/dist/quicklisp/2018-01-31/distinfo.txt";
        sha256 = "0z28yz205cl8pa8lbflw9072mywg69jx0gf091rhx2wjjf9h14qy";
      };
      buildPhase = '' true; '';
      postInstall = ''
        substituteAll ${./quicklisp.sh} "$out"/bin/quicklisp
        chmod a+x "$out"/bin/quicklisp
        cp "${quicklispdist}" "$out/lib/common-lisp/quicklisp/quicklisp-distinfo.txt"
      '';
    };
  };

  quicklisp-dists-to-nix = stdenv.mkDerivation rec {
    name = "quicklisp-dists-to-nix";
    version = "1.0.0";

    coreutils = pkgs.coreutils;
    inherit quicklisp sbcl;
    src = ./quicklisp-dists-to-nix;

    buildPhase = ''
      mkdir -p $out/bin
      substituteAll $src/generate.sh $out/bin/quicklisp-dists-to-nix
      chmod a+x $out/bin/quicklisp-dists-to-nix
    '';
    installPhase = "true";
  };

  dists = (import ./dists.nix { inherit pkgs clwrapper stdenv buildLispPackage; fetchurl = pkgs.fetchurl; });

  quicklisp-to-nix-for-closure = closure: stdenv.mkDerivation rec {
    name = "quicklisp-to-nix-${version}";
    version = "1.0.0";

    # We can't depend on all the quicklisp releases directly.  There
    # are too many!  Instead we'll depend on a file containing the
    # path to all the releases.  quicklisp-to-nix will read this file
    # and modify the ASDF search path itself.
    releaseListInput = stdenv.lib.concatMapStrings (release: ''
      ${release}
    '') (builtins.attrValues closure.byRelease);
    systemListInput = stdenv.lib.concatMapStrings (systemName: ''
      ${systemName}
    '') (builtins.attrNames closure.bySystem);
    passAsFile = [ "releaseListInput" "systemListInput" ];

    src = ./quicklisp-to-nix;
    buildPhase = ''
      systemListPath="$out/etc/quicklisp-to-nix/system-list"
      export systemListPath
      releaseListPath="$out/etc/quicklisp-to-nix/release-list"
      export releaseListPath
      substituteAll $src/quicklisp-to-nix.lisp quicklisp-to-nix.lisp
      ${sbcl}/bin/sbcl --load quicklisp-to-nix.lisp
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp quicklisp-to-nix $out/bin
      mkdir -p "$(dirname "$systemListPath")"
      cp $systemListInputPath $systemListPath
      mkdir -p "$(dirname "releaseListPath")"
      cp $releaseListInputPath $releaseListPath
    '';
    dontStrip = true;
  };
  quicklisp-to-nix = quicklisp-to-nix-for-closure dists.quicklisp;

  quicklispClosure = { systems, propagatedBuildInputs ? [] }: stdenv.mkDerivation rec {
    name = "quicklisp-closure";
    inherit propagatedBuildInputs;
    unpackPhase = "true";
    buildPhase = ''
      echo init
      ${quicklisp}/bin/quicklisp --quicklisp-dir ./quicklisp --no-update --noninteractive -- init
      echo install
      find
      ${quicklisp}/bin/quicklisp --quicklisp-dir ./quicklisp --no-update --noninteractive -- install $systems
    '';
    installPhase = ''
      cp -r ./quicklisp/dists/quicklisp/software/ $out
    '';
  };
};
in lispPackages
