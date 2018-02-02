args @ { fetchurl, ... }:
rec {
  baseName = ''simple-date'';
  version = ''postmodern-20180131-git'';

  parasites = [ "simple-date/postgres-glue" "simple-date/tests" ];

  description = '''';

  deps = [ ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/postmodern/2018-01-31/postmodern-20180131-git.tgz'';
    sha256 = ''0mz5pm759py1iscfn44c00dal2fijkyp5479fpx9l6i7wrdx2mki'';
  };

  packageName = "simple-date";

  asdFilesToKeep = ["simple-date.asd"];
  overrides = x: x;
}
/* (SYSTEM simple-date DESCRIPTION NIL SHA256
    0mz5pm759py1iscfn44c00dal2fijkyp5479fpx9l6i7wrdx2mki URL
    http://beta.quicklisp.org/archive/postmodern/2018-01-31/postmodern-20180131-git.tgz
    MD5 a3b7bf25eb342cd49fe144fcd7ddcb16 NAME simple-date FILENAME simple-date
    DEPS NIL DEPENDENCIES NIL VERSION postmodern-20180131-git SIBLINGS
    (cl-postgres postmodern s-sql) PARASITES
    (simple-date/postgres-glue simple-date/tests)) */
