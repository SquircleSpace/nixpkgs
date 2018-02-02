args @ { fetchurl, ... }:
rec {
  baseName = ''cl-async-base'';
  version = ''cl-async-20171130-git'';

  parasites = [ "cl-async" "cl-async-util" ];

  description = ''Base system for cl-async.'';

  deps = [ args."alexandria" args."bordeaux-threads" args."cffi" args."cffi-grovel" args."cl-async" args."cl-libuv" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/cl-async/2017-11-30/cl-async-20171130-git.tgz'';
    sha256 = ''0z3bxnzknb9dbisn9d0z1nw6qpswf8cn97v3mfrfq48q9hz11nvm'';
  };

  packageName = "cl-async-base";

  asdFilesToKeep = ["cl-async-base.asd"];
  overrides = x: x;
}
/* (SYSTEM cl-async-base DESCRIPTION Base system for cl-async. SHA256
    0z3bxnzknb9dbisn9d0z1nw6qpswf8cn97v3mfrfq48q9hz11nvm URL
    http://beta.quicklisp.org/archive/cl-async/2017-11-30/cl-async-20171130-git.tgz
    MD5 4e54a593f8c7f02a2c7f7e0e07247c05 NAME cl-async-base FILENAME
    cl-async-base DEPS
    ((NAME alexandria FILENAME alexandria)
     (NAME bordeaux-threads FILENAME bordeaux-threads)
     (NAME cffi FILENAME cffi) (NAME cffi-grovel FILENAME cffi-grovel)
     (NAME cl-async FILENAME cl-async) (NAME cl-libuv FILENAME cl-libuv))
    DEPENDENCIES
    (alexandria bordeaux-threads cffi cffi-grovel cl-async cl-libuv) VERSION
    cl-async-20171130-git SIBLINGS
    (cl-async-repl cl-async-ssl cl-async-test cl-async) PARASITES
    (cl-async cl-async-util)) */
