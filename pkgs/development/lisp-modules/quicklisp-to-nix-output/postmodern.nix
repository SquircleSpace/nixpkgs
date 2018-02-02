args @ { fetchurl, ... }:
rec {
  baseName = ''postmodern'';
  version = ''20180131-git'';

  parasites = [ "postmodern/tests" ];

  description = ''PostgreSQL programming API'';

  deps = [ args."alexandria" args."bordeaux-threads" args."cl-postgres" args."closer-mop" args."md5" args."s-sql" args."split-sequence" args."usocket" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/postmodern/2018-01-31/postmodern-20180131-git.tgz'';
    sha256 = ''0mz5pm759py1iscfn44c00dal2fijkyp5479fpx9l6i7wrdx2mki'';
  };

  packageName = "postmodern";

  asdFilesToKeep = ["postmodern.asd"];
  overrides = x: x;
}
/* (SYSTEM postmodern DESCRIPTION PostgreSQL programming API SHA256
    0mz5pm759py1iscfn44c00dal2fijkyp5479fpx9l6i7wrdx2mki URL
    http://beta.quicklisp.org/archive/postmodern/2018-01-31/postmodern-20180131-git.tgz
    MD5 a3b7bf25eb342cd49fe144fcd7ddcb16 NAME postmodern FILENAME postmodern
    DEPS
    ((NAME alexandria FILENAME alexandria)
     (NAME bordeaux-threads FILENAME bordeaux-threads)
     (NAME cl-postgres FILENAME cl-postgres)
     (NAME closer-mop FILENAME closer-mop) (NAME md5 FILENAME md5)
     (NAME s-sql FILENAME s-sql) (NAME split-sequence FILENAME split-sequence)
     (NAME usocket FILENAME usocket))
    DEPENDENCIES
    (alexandria bordeaux-threads cl-postgres closer-mop md5 s-sql
     split-sequence usocket)
    VERSION 20180131-git SIBLINGS (cl-postgres s-sql simple-date) PARASITES
    (postmodern/tests)) */
