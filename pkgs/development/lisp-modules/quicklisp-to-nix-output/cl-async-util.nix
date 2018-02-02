args @ { fetchurl, ... }:
rec {
  baseName = ''cl-async-util'';
  version = ''cl-async-20171130-git'';

  parasites = [ "cl-async" "cl-async-base" ];

  description = ''Internal utilities for cl-async.'';

  deps = [ args."alexandria" args."bordeaux-threads" args."cffi" args."cffi-grovel" args."cl-async" args."cl-async-base" args."cl-libuv" args."cl-ppcre" args."fast-io" args."static-vectors" args."trivial-gray-streams" args."vom" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/cl-async/2017-11-30/cl-async-20171130-git.tgz'';
    sha256 = ''0z3bxnzknb9dbisn9d0z1nw6qpswf8cn97v3mfrfq48q9hz11nvm'';
  };

  packageName = "cl-async-util";

  asdFilesToKeep = ["cl-async-util.asd"];
  overrides = x: x;
}
/* (SYSTEM cl-async-util DESCRIPTION Internal utilities for cl-async. SHA256
    0z3bxnzknb9dbisn9d0z1nw6qpswf8cn97v3mfrfq48q9hz11nvm URL
    http://beta.quicklisp.org/archive/cl-async/2017-11-30/cl-async-20171130-git.tgz
    MD5 4e54a593f8c7f02a2c7f7e0e07247c05 NAME cl-async-util FILENAME
    cl-async-util DEPS
    ((NAME alexandria FILENAME alexandria)
     (NAME bordeaux-threads FILENAME bordeaux-threads)
     (NAME cffi FILENAME cffi) (NAME cffi-grovel FILENAME cffi-grovel)
     (NAME cl-async FILENAME cl-async)
     (NAME cl-async-base FILENAME cl-async-base)
     (NAME cl-libuv FILENAME cl-libuv) (NAME cl-ppcre FILENAME cl-ppcre)
     (NAME fast-io FILENAME fast-io)
     (NAME static-vectors FILENAME static-vectors)
     (NAME trivial-gray-streams FILENAME trivial-gray-streams)
     (NAME vom FILENAME vom))
    DEPENDENCIES
    (alexandria bordeaux-threads cffi cffi-grovel cl-async cl-async-base
     cl-libuv cl-ppcre fast-io static-vectors trivial-gray-streams vom)
    VERSION cl-async-20171130-git SIBLINGS
    (cl-async-repl cl-async-ssl cl-async-test cl-async) PARASITES
    (cl-async cl-async-base)) */
