args @ { fetchurl, ... }:
rec {
  baseName = ''cxml-test'';
  version = ''cxml-20110619-git'';

  parasites = [ "cxml" "cxml-dom" "cxml-klacks" "cxml-xml" ];

  description = '''';

  deps = [ args."alexandria" args."babel" args."closure-common" args."cxml" args."cxml-dom" args."cxml-klacks" args."cxml-xml" args."puri" args."trivial-features" args."trivial-gray-streams" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/cxml/2011-06-19/cxml-20110619-git.tgz'';
    sha256 = ''04k6syn9p7qsazi84kab9n9ki2pb5hrcs0ilw7wikxfqnbabm2yk'';
  };

  packageName = "cxml-test";

  asdFilesToKeep = ["cxml-test.asd"];
  overrides = x: x;
}
/* (SYSTEM cxml-test DESCRIPTION NIL SHA256
    04k6syn9p7qsazi84kab9n9ki2pb5hrcs0ilw7wikxfqnbabm2yk URL
    http://beta.quicklisp.org/archive/cxml/2011-06-19/cxml-20110619-git.tgz MD5
    587755dff60416d4f716f4e785cf747e NAME cxml-test FILENAME cxml-test DEPS
    ((NAME alexandria FILENAME alexandria) (NAME babel FILENAME babel)
     (NAME closure-common FILENAME closure-common) (NAME cxml FILENAME cxml)
     (NAME cxml-dom FILENAME cxml-dom) (NAME cxml-klacks FILENAME cxml-klacks)
     (NAME cxml-xml FILENAME cxml-xml) (NAME puri FILENAME puri)
     (NAME trivial-features FILENAME trivial-features)
     (NAME trivial-gray-streams FILENAME trivial-gray-streams))
    DEPENDENCIES
    (alexandria babel closure-common cxml cxml-dom cxml-klacks cxml-xml puri
     trivial-features trivial-gray-streams)
    VERSION cxml-20110619-git SIBLINGS (cxml) PARASITES
    (cxml cxml-dom cxml-klacks cxml-xml)) */
