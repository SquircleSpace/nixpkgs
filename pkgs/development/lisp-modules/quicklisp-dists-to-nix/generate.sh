#!/bin/sh

tempdir="$(@coreutils@/bin/mktemp -d)" || {
    echo "Couldn't make temporary directory" >&2
    exit 1
}

cleanup() {
    rm -rd "$tempdir"
}

@quicklisp@/bin/quicklisp --no-update --quicklisp-dir "$tempdir" -- init || {
    cleanup
    echo "Failed to init quicklisp" >&2
    exit 1
}

@sbcl@/bin/sbcl --load "$tempdir/setup.lisp" --load "@src@/generate-package.lisp" --eval '(uiop:quit 0)' || {
    cleanup
    echo "Failed to generate quicklisp package" >&2
    exit 1
}

cleanup
