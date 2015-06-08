{ pkgs ? import <nixpkgs> {} }:
let
  stdenv = pkgs.stdenv;
  clwrapper = pkgs.lispPackages.clwrapper;
  lispAdd = rec {
    inherit clwrapper stdenv;
    callPackage = pkgs.lib.callPackageWith lispAdd;
    buildLispPackage = callPackage <nixpkgs/pkgs/development/lisp-modules/define-package.nix>;


    cl-utilities = buildLispPackage rec {
      baseName = "cl-utilities";
      version = "1.2.4";
      description = "A collection of Common Lisp utility functions";
      deps = [];
      # Source type: http
      src = pkgs.fetchurl {
        url = ''http://common-lisp.net/project/cl-utilities/cl-utilities-${version}.tar.gz'';
        sha256 = "1z2ippnv2wgyxpz15zpif7j7sp1r20fkjhm4n6am2fyp6a3k3a87";
      };
    };

    fiveam = buildLispPackage rec {
      baseName = "fiveam";
      description = "a common lisp testing framework";
      version = "1.2";
      deps = [];
      src = pkgs.fetchgit {
        url = https://github.com/sionescu/fiveam;
        sha256 = "1yx9716mk8pq9076q6cjx4c9lyax3amiccy37sh0913k2x8gsm4l";
        rev = "678db1b9a79ea4789c77c53d7aa95d704780e3bc";
      };
    };

    split-sequence = buildLispPackage rec {
      baseName = "split-sequence";
      description = "";
      version = "1.1";
      deps = [];
      src = pkgs.fetchgit {
        url = https://github.com/sharplispers/split-sequence;
        rev = "ccd34181d571c3d41f32855903f295f849209737";
        sha256 = "0p0cd31334bizw1z6wajk3iskxaaszpfmqdl2v157ljmja6v5vd1";
      };
    };

    cl-cli = buildLispPackage rec {
      baseName = "cl-cli";
      description = "";
      version = "0.2";
      deps = [ split-sequence ];
      src = pkgs.fetchgit {
        url = https://github.com/renard/cl-cli;
        sha256 = "080zig806r588hsxjq0si6gp1gw5ks7kvihzc93cx5pnjklhxpm3";
        rev = "810f2e4a9f6ff3ff39b299cd328dacc86337f706";
      };
    };

    unix-options = buildLispPackage rec {
      baseName = "unix-options";
      description = "";
      version = "0.3.1";
      deps = [];
      src = pkgs.fetchgit {
        url = https://github.com/astine/unix-options;
        rev = "501849bd7d24748e52fe2cd6734f137f78019256";
        sha256 = "1bzchf4grb9r6n69l92hwj7m57jknws086fykd4nzrlc38hascn3";
      };
    };

    cffi = buildLispPackage rec {
      baseName = "cffi";
      version = "0.14.0";
      description = "The Common Foreign Function Interface";
      deps = [alexandria babel trivial-features];
      # Source type: http
      src = pkgs.fetchurl {
        url = ''http://common-lisp.net/project/cffi/releases/cffi_${version}.tar.gz'';
        sha256 = "155igjh096vrp7n71c0xcg9qbcpj6547qjvzi9shxbpi6piw6fkw";
      };
    };

    babel = buildLispPackage rec {
      baseName = "babel";
      version = "git-20141113";
      description = "Babel, a charset conversion library.";
      deps = [alexandria trivial-features];
      # Source type: git
      src = pkgs.fetchgit {
        url = ''https://github.com/cl-babel/babel'';
        sha256 = "abe7150f25ceb7eded520d95f1665a46f4233cf13b577fd02c3f6be54c32facc";
        rev = ''74b35ea79b769c4f9aefad933923778ffa7915ab'';
      };
    };

    trivial-features = buildLispPackage rec {
      baseName = "trivial-features";
      version = "git-20141112";
      description = "Ensures consistent *FEATURES* across multiple CLs.";
      deps = [];
      # Source type: git
      src = pkgs.fetchgit {
        url = ''https://github.com/trivial-features/trivial-features'';
        sha256 = "2006aebe0c2bfed1c39a2195639e221fdc52a443b6c8522e535cbef2370a07fc";
        rev = ''2b7cdc3b8073eb33655850b51223770b535da6d9'';
      };
    };

    alexandria = pkgs.lispPackages.buildLispPackage rec {
      baseName = "alexandria";
      version = "git-20150427";
      description = "A collection of portable public domain utilities";
      deps = [];
      src = pkgs.fetchgit {
        url = "https://gitlab.common-lisp.net/alexandria/alexandria.git";
        sha256 = "0q3x3x7y6ahrv9j6h76l8lmwb8kn4wvwv7xk79b9nssi6xdvw3as";
        rev = ''5a17c072970cf50213f7f896c40e6e640638391f'';
      };
    };

    iterate = buildLispPackage rec {
      baseName = "iterate";
      version = "darcs-2014-11-01";
      description = "Iteration package for Common Lisp";
      deps = [];
      src = (pkgs.lib.overrideDerivation (pkgs.fetchdarcs {
        url = "http://common-lisp.net/project/iterate/darcs/iterate";
        sha256 = "0gm05s3laiivsqgqjfj1rkz83c2c0jyn4msfgbv6sz42znjpam25";
      }) (x: {SSL_CERT_FILE=pkgs.cacert + "/etc/ca-bundle.crt";}));
      overrides = x: {
        configurePhase="buildPhase(){ true; }";
      };
    };

    cl-sqlite = buildLispPackage rec {
      baseName = "cl-sqlite";
      description = "";
      version = "0.2";
      deps = [ iterate cffi ];
      src = pkgs.fetchurl {
        url = http://common-lisp.net/project/cl-sqlite/releases/cl-sqlite-0.2.tar.gz;
        sha256 = "0vbcv5wcdm2imzkb2zb6kg2xdcxa1rcpj8cy12rcr1n4bqjjhdbq";
      };
      overrides = x:  {
        patchPhase = ''
          sed -i 's|libsqlite3.so|${pkgs.sqlite}/lib/libsqlite3.so|g' sqlite-ffi.lisp
          sed -i 's|libsqlite3.so.0|${pkgs.sqlite}/lib/libsqlite3.so.0|g' sqlite-ffi.lisp
        '';
      };
    };

    nibbles = buildLispPackage rec {
      baseName = "nibbles";
      version = "git-20141116";
      description = "A library for accessing octet-addressed blocks of data";
      deps = [];
      # Source type: git
      src = pkgs.fetchgit {
        url = ''https://github.com/froydnj/nibbles'';
        sha256 = "39ad95be2b9f9ea80dbccd205a0ed6f9c5ef175a10da6eec55b7ba09a8f1a76a";
        rev = ''ace095d85e48b18bf9cf9e21249ba7fb57e3efe2'';
      };
    };

    ironclad = buildLispPackage rec {
      baseName = "ironclad";
      version = "0.33.0";
      description = "A cryptographic toolkit written in pure Common Lisp";
      deps = [nibbles];
      src = pkgs.fetchurl {
        url = ''http://method-combination.net/lisp/files/ironclad_0.33.0.tar.gz'';
        sha256 = "1ld0xz8gmi566zxl1cva5yi86aw1wb6i6446gxxdw1lisxx3xwz7";
      };
    };

    external-program = buildLispPackage rec {
      baseName = "external-program";
      description = "";
      version = "0.0.6";
      deps = [];
      src = pkgs.fetchgit {
        url = https://github.com/sellout/external-program;
        rev = "8ad9f5a1ea84289378464bd00fc0a7bde470c210";
        sha256 = "0d9bccd5i85mian98xw336a7fk7vv9fi3rvh3h86fzw3z6ay08gc";
      };
    };

    osicat = buildLispPackage rec {
      baseName = "osicat";
      description = "A lightweight operating system interface";
      version = "0449e33a";
      deps = [ cffi alexandria trivial-features ];
      src = pkgs.fetchgit {
        url = https://github.com/osicat/osicat;
        rev = "0449e33ad92ff4d46f1f69082d3f71b3fe3d2dfd";
        sha256 = "1rgxw6caj339ihpxks6f1q80ahd3ama3gfisbd7l3xc996s3s4ag";
      };
    };

    anaphora = buildLispPackage rec {
      baseName = "anaphora";
      description = "";
      version = "0.9.4";
      deps = [];
      src = pkgs.fetchurl {
        url = https://common-lisp.net/project/anaphora/files/anaphora-0.9.4.tar.gz;
        sha256 = "0gwq8gp1ycbdmvydg03grjaii8765dz6yzvnrw0lrkwqnkh38wsy";
      };
    };

    let-plus = buildLispPackage rec {
      baseName = "let-plus";
      description = "Destructuring extension of LET*.";
      version = "master-079730bf";
      deps = [ alexandria anaphora ];
      src = pkgs.fetchgit {
        url = https://github.com/tpapp/let-plus;
        rev = "079730bfca755b4dcaf5295e380eb5adf3fc6a7c";
        sha256 = "0xzg7gavcxrh43vzdzj68c6ixsnmpk0aip9j5yx78xxwv1nr09f0";
      };
    };

    cl-colors = buildLispPackage rec {
      baseName = "cl-colors";
      description = "This is a very simple color library for Common Lisp.";
      version = "master-9340ccdf";
      deps = [ let-plus alexandria ];
      src = pkgs.fetchgit {
        url = https://github.com/tpapp/cl-colors;
        rev = "9340ccdffe1e52beafa8b13deb6e4f71c44fdfa0";
        sha256 = "1kiriylbj81f5h2dhl4396a4fhbyd2xrss340v6j918wjn5fka98";
      };
      overrides = x: {
        patchPhase = ''
          rm Makefile
        '';
      };
    };

    cl-ansi-text = buildLispPackage rec {
      baseName = "cl-ansi-text";
      description = "Because color in your terminal is nice.";
      version = "master-6dca2239";
      deps = [ cl-colors alexandria ];
      src = pkgs.fetchgit {
        url = https://github.com/pnathan/cl-ansi-text;
        rev = "6dca2239033c628015a7d12a63d673314c316374";
        sha256 = "07izgb0c511fzcbfic0n7zxgwhbnjfga3igm0q5rawpihpf0caws";
      };
    };

  };
in
pkgs.stdenv.mkDerivation rec {
  version = "0.0.1";

  name = "dlm-${version}";

  src = ./..;

  patchPhase = ''
    sed -i 's|LISP=sbcl|LISP=${pkgs.sbcl}/bin/sbcl|g' build/Makefile
    sed -i 's|*fetch-default-bin* "curl"|*fetch-default-bin* "${pkgs.curl}/bin/curl"|g' src/dlm.lisp
    sed -i 's|*youtube-dl-bin* "youtube-dl"|*youtube-dl-bin* "${pkgs.youtube-dl}/bin/youtube-dl"|g' src/dlm.lisp
    sed -i 's|*scp-bin* "scp"|*scp-bin* "${pkgs.openssh}/bin/scp"|g' src/dlm.lisp
  '';

  buildPhase = ''
    cd build
    make test && make dlm
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp dlm* $out/bin
    ln -s $out/bin/dlm-${version} $out/bin/dlm
  '';

  dontStrip = true;

  buildInputs = with lispAdd; [
    fiveam
    unix-options
    ironclad
    cl-sqlite
    cl-ansi-text
    external-program
    pkgs.sqlite

    # the following are needed for "make release"
    pkgs.git
    pkgs.gnupg1
    # additionally for make push-release"
    pkgs.emacs
    pkgs.openssh
  ];
}
