args @ { fetchurl, ... }:
rec {
  baseName = ''iolib'';
  version = ''v0.8.3'';

  parasites = [ "iolib/multiplex" "iolib/os" "iolib/pathnames" "iolib/sockets" "iolib/streams" "iolib/syscalls" "iolib/trivial-sockets" "iolib/zstreams" ];

  description = ''I/O library.'';

  deps = [ args."alexandria" args."bordeaux-threads" args."cffi" args."idna" args."iolib_dot_asdf" args."iolib_dot_base" args."iolib_dot_conf" args."iolib_slash_multiplex" args."iolib_slash_sockets" args."iolib_slash_streams" args."split-sequence" args."swap-bytes" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/iolib/2017-06-30/iolib-v0.8.3.tgz'';
    sha256 = ''12gsvsjyxmclwidcjvyrfvd0773ib54a3qzmf33hmgc9knxlli7c'';
  };

  packageName = "iolib";

  asdFilesToKeep = ["iolib.asd"];
  overrides = x: x;
}
/* (SYSTEM iolib DESCRIPTION I/O library. SHA256
    12gsvsjyxmclwidcjvyrfvd0773ib54a3qzmf33hmgc9knxlli7c URL
    http://beta.quicklisp.org/archive/iolib/2017-06-30/iolib-v0.8.3.tgz MD5
    fc28d4cad6f8e43972df3baa6a8ac45c NAME iolib FILENAME iolib DEPS
    ((NAME alexandria FILENAME alexandria)
     (NAME bordeaux-threads FILENAME bordeaux-threads)
     (NAME cffi FILENAME cffi) (NAME idna FILENAME idna)
     (NAME iolib.asdf FILENAME iolib_dot_asdf)
     (NAME iolib.base FILENAME iolib_dot_base)
     (NAME iolib.conf FILENAME iolib_dot_conf)
     (NAME iolib/multiplex FILENAME iolib_slash_multiplex)
     (NAME iolib/sockets FILENAME iolib_slash_sockets)
     (NAME iolib/streams FILENAME iolib_slash_streams)
     (NAME split-sequence FILENAME split-sequence)
     (NAME swap-bytes FILENAME swap-bytes))
    DEPENDENCIES
    (alexandria bordeaux-threads cffi idna iolib.asdf iolib.base iolib.conf
     iolib/multiplex iolib/sockets iolib/streams split-sequence swap-bytes)
    VERSION v0.8.3 SIBLINGS
    (iolib.asdf iolib.base iolib.common-lisp iolib.conf iolib.examples
     iolib.grovel iolib.tests)
    PARASITES
    (iolib/multiplex iolib/os iolib/pathnames iolib/sockets iolib/streams
     iolib/syscalls iolib/trivial-sockets iolib/zstreams)) */
