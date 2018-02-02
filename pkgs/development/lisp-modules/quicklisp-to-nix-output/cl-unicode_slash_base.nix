args @ { fetchurl, ... }:
rec {
  baseName = ''cl-unicode_slash_base'';
  version = ''cl-unicode-20180131-git'';

  parasites = [ "cl-unicode" "cl-unicode/build" "cl-unicode/test" ];

  description = '''';

  deps = [ args."cl-ppcre" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/cl-unicode/2018-01-31/cl-unicode-20180131-git.tgz'';
    sha256 = ''113hsx22pw4ydwzkyr9y7l8a8jq3nkhwazs03wj3mh2dczwv28xa'';
  };

  packageName = "cl-unicode/base";

  asdFilesToKeep = ["cl-unicode/base.asd"];
  overrides = x: x;
}
/* (SYSTEM cl-unicode/base DESCRIPTION NIL SHA256
    113hsx22pw4ydwzkyr9y7l8a8jq3nkhwazs03wj3mh2dczwv28xa URL
    http://beta.quicklisp.org/archive/cl-unicode/2018-01-31/cl-unicode-20180131-git.tgz
    MD5 653ba12d595599b32aa2a8c7c8b65c28 NAME cl-unicode/base FILENAME
    cl-unicode_slash_base DEPS ((NAME cl-ppcre FILENAME cl-ppcre)) DEPENDENCIES
    (cl-ppcre) VERSION cl-unicode-20180131-git SIBLINGS (cl-unicode) PARASITES
    (cl-unicode cl-unicode/build cl-unicode/test)) */
