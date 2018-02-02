args @ { fetchurl, ... }:
rec {
  baseName = ''iolib_slash_sockets'';
  version = ''iolib-v0.8.3'';

  parasites = [ "iolib" "iolib/multiplex" "iolib/os" "iolib/pathnames" "iolib/streams" "iolib/syscalls" "iolib/trivial-sockets" "iolib/zstreams" ];

  description = ''Socket library.'';

  deps = [ args."alexandria" args."babel" args."bordeaux-threads" args."cffi" args."idna" args."iolib_dot_asdf" args."iolib_dot_base" args."iolib_dot_conf" args."iolib_dot_grovel" args."iolib_slash_streams" args."iolib_slash_syscalls" args."split-sequence" args."swap-bytes" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/iolib/2017-06-30/iolib-v0.8.3.tgz'';
    sha256 = ''12gsvsjyxmclwidcjvyrfvd0773ib54a3qzmf33hmgc9knxlli7c'';
  };

  packageName = "iolib/sockets";

  asdFilesToKeep = ["iolib/sockets.asd"];
  overrides = x: x;
}
/* (SYSTEM iolib/sockets DESCRIPTION Socket library. SHA256
    12gsvsjyxmclwidcjvyrfvd0773ib54a3qzmf33hmgc9knxlli7c URL
    http://beta.quicklisp.org/archive/iolib/2017-06-30/iolib-v0.8.3.tgz MD5
    fc28d4cad6f8e43972df3baa6a8ac45c NAME iolib/sockets FILENAME
    iolib_slash_sockets DEPS
    ((NAME alexandria FILENAME alexandria) (NAME babel FILENAME babel)
     (NAME bordeaux-threads FILENAME bordeaux-threads)
     (NAME cffi FILENAME cffi) (NAME idna FILENAME idna)
     (NAME iolib.asdf FILENAME iolib_dot_asdf)
     (NAME iolib.base FILENAME iolib_dot_base)
     (NAME iolib.conf FILENAME iolib_dot_conf)
     (NAME iolib.grovel FILENAME iolib_dot_grovel)
     (NAME iolib/streams FILENAME iolib_slash_streams)
     (NAME iolib/syscalls FILENAME iolib_slash_syscalls)
     (NAME split-sequence FILENAME split-sequence)
     (NAME swap-bytes FILENAME swap-bytes))
    DEPENDENCIES
    (alexandria babel bordeaux-threads cffi idna iolib.asdf iolib.base
     iolib.conf iolib.grovel iolib/streams iolib/syscalls split-sequence
     swap-bytes)
    VERSION iolib-v0.8.3 SIBLINGS
    (iolib iolib.asdf iolib.base iolib.common-lisp iolib.conf iolib.examples
     iolib.grovel iolib.tests)
    PARASITES
    (iolib iolib/multiplex iolib/os iolib/pathnames iolib/streams
     iolib/syscalls iolib/trivial-sockets iolib/zstreams)) */
