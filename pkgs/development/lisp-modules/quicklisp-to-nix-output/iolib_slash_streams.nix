args @ { fetchurl, ... }:
rec {
  baseName = ''iolib_slash_streams'';
  version = ''iolib-v0.8.3'';

  parasites = [ "iolib" "iolib/multiplex" "iolib/os" "iolib/pathnames" "iolib/sockets" "iolib/syscalls" "iolib/trivial-sockets" "iolib/zstreams" ];

  description = ''Gray streams.'';

  deps = [ args."alexandria" args."cffi" args."iolib_dot_asdf" args."iolib_dot_base" args."iolib_dot_conf" args."iolib_slash_multiplex" args."split-sequence" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/iolib/2017-06-30/iolib-v0.8.3.tgz'';
    sha256 = ''12gsvsjyxmclwidcjvyrfvd0773ib54a3qzmf33hmgc9knxlli7c'';
  };

  packageName = "iolib/streams";

  asdFilesToKeep = ["iolib/streams.asd"];
  overrides = x: x;
}
/* (SYSTEM iolib/streams DESCRIPTION Gray streams. SHA256
    12gsvsjyxmclwidcjvyrfvd0773ib54a3qzmf33hmgc9knxlli7c URL
    http://beta.quicklisp.org/archive/iolib/2017-06-30/iolib-v0.8.3.tgz MD5
    fc28d4cad6f8e43972df3baa6a8ac45c NAME iolib/streams FILENAME
    iolib_slash_streams DEPS
    ((NAME alexandria FILENAME alexandria) (NAME cffi FILENAME cffi)
     (NAME iolib.asdf FILENAME iolib_dot_asdf)
     (NAME iolib.base FILENAME iolib_dot_base)
     (NAME iolib.conf FILENAME iolib_dot_conf)
     (NAME iolib/multiplex FILENAME iolib_slash_multiplex)
     (NAME split-sequence FILENAME split-sequence))
    DEPENDENCIES
    (alexandria cffi iolib.asdf iolib.base iolib.conf iolib/multiplex
     split-sequence)
    VERSION iolib-v0.8.3 SIBLINGS
    (iolib iolib.asdf iolib.base iolib.common-lisp iolib.conf iolib.examples
     iolib.grovel iolib.tests)
    PARASITES
    (iolib iolib/multiplex iolib/os iolib/pathnames iolib/sockets
     iolib/syscalls iolib/trivial-sockets iolib/zstreams)) */
