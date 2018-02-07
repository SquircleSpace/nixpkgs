(unless (find-package :ql-to-nix-util)
  (load "util.lisp"))
(unless (find-package :ql-to-nix-quicklisp-bootstrap)
  (load "quicklisp-bootstrap.lisp"))
(defpackage :ql-to-nix-system-info
  (:use :common-lisp :ql-to-nix-quicklisp-bootstrap :ql-to-nix-util)
  (:export #:dump-image))
(in-package :ql-to-nix-system-info)

(declaim (optimize (debug 3) (speed 0) (space 0) (compilation-speed 0) (safety 3)))

;; This file cannot have any dependencies beyond quicklisp and asdf.
;; Otherwise, we'll miss some dependencies!

;; We can't load quicklisp until runtime (at which point we'll create
;; an isolated quicklisp installation).  These wrapper functions are
;; nicer than funcalling intern'd symbols every time we want to talk
;; to quicklisp.
(wrap :ql apply-load-strategy)
(wrap :ql compute-load-strategy)
(wrap :ql show-load-strategy)
(wrap :ql quicklisp-systems)
(wrap :ql ensure-installed)
(wrap :ql quicklisp-releases)
(wrap :ql-dist archive-md5)
(wrap :ql-dist archive-url)
(wrap :ql-dist ensure-local-archive-file)
(wrap :ql-dist find-system)
(wrap :ql-dist local-archive-file)
(wrap :ql-dist name)
(wrap :ql-dist provided-systems)
(wrap :ql-dist release)
(wrap :ql-dist short-description)
(wrap :ql-dist system-file-name)
(wrap :ql-impl-util call-with-quiet-compilation)

(defvar *version* (uiop:getenv "version")
  "The version number of this program")

(defvar *main-system* nil
  "The name of the system we're trying to extract info from.")

(defvar *found-parasites* (make-hash-table :test #'equalp)
  "Names of systems which have been identified as parasites.

A system is parasitic if its name doesn't match the name of the file
it is defined in.  So, for example, if foo and foo-bar are both
defined in a file named foo.asd, foo would be the host system and
foo-bar would be a parasitic system.

Parasitic systems are not generally loaded without loading the host
system first.

Keys are system names.  Values are unspecified.")

(defvar *found-dependencies* (make-hash-table :test #'equalp)
  "Hash table containing the set of dependencies discovered while installing a system.

Keys are system names.  Values are unspecified.")

(defun decode-asdf-dependency (name)
  "Translates an asdf system dependency description into a system name.

For example, translates (:version :foo \"1.0\") into \"foo\"."
  (etypecase name
    (symbol
     (setf name (symbol-name name)))
    (string)
    (cons
     (ecase (first name)
       (:version
        (warn "Discarding version information ~A" name)
        ;; There's nothing we can do about this.  If the version we
        ;; have around is good enough, then we're golden.  If it isn't
        ;; good enough, then we'll error out and let a human figure it
        ;; out.
        (setf name (second name))
        (return-from decode-asdf-dependency
          (decode-asdf-dependency name)))

       (:feature
        (if (find (second name) *features*)
            (return-from decode-asdf-dependency
              (decode-asdf-dependency (third name)))
            (progn
              (warn "Dropping dependency due to missing feature: ~A" name)
              (return-from decode-asdf-dependency nil))))

       (:require
        ;; This probably isn't a dependency we can satisfy using
        ;; quicklisp, but we might as well try anyway.
        (return-from decode-asdf-dependency
          (decode-asdf-dependency (second name)))))))
  (string-downcase name))

(defun found-new-parasite (system-name)
  "Record that the given system has been identified as a parasite."
  (setf system-name (decode-asdf-dependency system-name))
  (setf (gethash system-name *found-parasites*) t)
  (when (nth-value 1 (gethash system-name *found-dependencies*))
    (error "Found dependency on parasite")))

(defun known-parasite-p (system-name)
  "Have we previously identified this system as a parasite?"
  (nth-value 1 (gethash system-name *found-parasites*)))

(defun found-parasites ()
  "Return a vector containing all identified parasites."
  (let ((systems (make-array (hash-table-size *found-parasites*) :fill-pointer 0)))
    (loop :for system :being :the :hash-keys :of *found-parasites* :do
       (vector-push system systems))
    systems))

(defvar *track-dependencies* nil
  "When this variable is nil, found-new-dependency will not record
depdendencies.")

(defun parasitic-relationship-p (potential-host potential-parasite)
  "Returns t if potential-host and potential-parasite have a parasitic relationship.

See `*found-parasites*'."
  (let ((host-ql-system (find-system potential-host))
        (parasite-ql-system (find-system potential-parasite)))
    (and host-ql-system parasite-ql-system
         (not (equal (name host-ql-system)
                     (name parasite-ql-system)))
         (equal (system-file-name host-ql-system)
                (system-file-name parasite-ql-system)))))

(defun found-new-dependency (name)
  "Record that the given system has been identified as a dependency.

The named system may not be recorded as a dependency.  It may be left
out for any number of reasons.  For example, if `*track-dependencies*'
is nil then this function does nothing.  If the named system isn't a
quicklisp system, this function does nothing."
  (setf name (decode-asdf-dependency name))
  (unless name
    (return-from found-new-dependency))
  (unless *track-dependencies*
    (return-from found-new-dependency))
  (unless (find-system name)
    (return-from found-new-dependency))
  (setf (gethash name *found-dependencies*) t))

(defun forget-dependency (name)
  "Whoops.  Did I say that was a dependency?  My bad.

Be very careful using this function!  You can remove a system from the
dependency list, but you can't remove other effects associated with
this system.  For example, transitive dependencies might still be in
the dependency list."
  (setf name (decode-asdf-dependency name))
  (remhash name *found-dependencies*))

(defun found-dependencies ()
  "Return a vector containing all identified dependencies."
  (let ((systems (make-array (hash-table-size *found-dependencies*) :fill-pointer 0)))
    (loop :for system :being :the :hash-keys :of *found-dependencies* :do
       (vector-push system systems))
    systems))

(defun get-loaded (system)
  "Try to load the named system using quicklisp and record any
dependencies quicklisp is aware of.

Unlike `our-quickload', this function doesn't attempt to install
missing dependencies."
  ;; Let's get this party started!
  (let* ((strategy (compute-load-strategy system))
         (ql-systems (quicklisp-systems strategy)))
    (dolist (dep ql-systems)
      (found-new-dependency (name dep)))
    (show-load-strategy strategy)
    (labels
        ((make-go ()
           (apply-load-strategy strategy)))
      (call-with-quiet-compilation #'make-go)
      (let ((asdf-system (asdf:find-system system)))
        ;; If ASDF says that it needed a system, then we should
        ;; probably track that.
        (dolist (asdf-dep (asdf:component-sideway-dependencies asdf-system))
          (found-new-dependency asdf-dep))
        (dolist (asdf-dep (asdf:system-defsystem-depends-on asdf-system))
          (found-new-dependency asdf-dep))))))

(defun our-quickload (system)
  "Attempt to install a package like quicklisp would, but record any
dependencies that are detected during the install."
  (setf system (string-downcase system))
  ;; Load it quickly, but do it OUR way.  Turns out our way is very
  ;; similar to the quicklisp way...
  (let ((already-tried (make-hash-table :test #'equalp))) ;; Case insensitive
    (tagbody
     retry
       (handler-case
           (get-loaded system)
         (asdf/find-component:missing-dependency (e)
           (let ((required-by (asdf/find-component:missing-required-by e))
                 (missing (asdf/find-component:missing-requires e)))
             (unless (typep required-by 'asdf:system)
               (error e))
             (when (gethash missing already-tried)
               (error "Dependency loop? ~A" missing))
             (setf (gethash missing already-tried) t)
             (found-new-dependency missing)
             (let ((*track-dependencies* nil))
               (our-quickload missing))
             (format t "Attempting to load ~A again~%" system)
             (go retry)))))))

(defun host-system (system-name)
  "Returns the name of the system that defines the given system.

For parasites, this returns the host.  For hosts, this returns, well,
the host."
  (let* ((system (find-system system-name))
         (host-file (system-file-name system)))
    host-file))

(defun host-system-p (system-name)
  "Returns t iff the given system has the same name as its asd file."
  (equalp system-name (host-system system-name)))

(defun find-parasitic-systems (system)
  (let* ((asdf-system (asdf:find-system system))
         (source-file (asdf:system-source-file asdf-system))
         (result (make-array 0 :adjustable t :fill-pointer t)))
    (cond
      (source-file
       (loop :for system-name :being :the :hash-keys :of asdf/find-system::*registered-systems* :do
          (when (parasitic-relationship-p system system-name)
            (vector-push-extend system-name result))))
      (t
       (unless (or (equal "uiop" system)
                   (equal "asdf" system))
         (warn "No source file for system ~A.  Can't identify parasites." system))))
    result))

(defun determine-dependencies (system)
  "Load the named system and return a sorted vector containing all the
quicklisp systems that were loaded to satisfy dependencies.

This function should probably only be called once per process!
Subsequent calls will miss dependencies identified by earlier calls."
  (tagbody
   retry
     (restart-case
         (let ((*standard-output* (make-broadcast-stream))
               (*trace-output* (make-broadcast-stream))
               (*main-system* system)
               (*track-dependencies* t))
           (handler-case
               (our-quickload system)
             (asdf/find-component:missing-component (e)
               (cond
                 ((host-system-p system)
                  ;; Game over
                  (error e))

                 ((equal (asdf/find-component:missing-requires e) system)
                  ;; One last chance!  There are misbehaving parasites
                  ;; that are named incorrectly (e.g. cl-async.asd
                  ;; hosts cl-async-base) and so ASDF can't find them
                  ;; until the host system is loaded.  The owner of
                  ;; the system should fix their broken code, but we
                  ;; still have a chance to generate a working package
                  ;; out of their mess.  If we claim a dependency on
                  ;; the host, we mimick the behavior that the clients
                  ;; of this library must have -- i.e. load the host
                  ;; before the evil parasite.
                  (found-new-dependency (host-system system))
                  (let ((*track-dependencies* nil))
                    (our-quickload (host-system system)))
                  (our-quickload system))))))
       (try-again ()
         :report "Start the quickload over again"
         (go retry))
       (die ()
         :report "Just give up and die"
         (uiop:quit 1))))

  ;; Systems can't depend on themselves!
  (forget-dependency system)
  (values))

(defun system-data (system)
  "Produce a plist describing a system.

The plist is only meant to be consumed by other parts of
quicklisp-to-nix."
  (determine-dependencies system)
  (let*
      ((dependencies (sort (found-dependencies) #'string<))
       (parasites (coerce (sort (find-parasitic-systems system) #'string<) 'list))
       (ql-system (find-system system))
       (ql-release (release ql-system))
       (ql-sibling-systems (provided-systems ql-release))
       (url (archive-url ql-release))
       (local-archive (local-archive-file ql-release))
       (archive-sha256
        (progn
          (ensure-local-archive-file ql-release)
          ;; Stuff this archive into the nix store.  It was almost
          ;; certainly going to end up there anyway (since it will
          ;; probably be fetchurl'd for a nix package).  Also, putting
          ;; it into the store also gives us the SHA we need.
          (nix-hash (namestring local-archive))))
       (ideal-md5 (archive-md5 ql-release))
       (raw-dependencies (coerce dependencies 'list))
       (name (string-downcase (format nil "~a" system)))
       (ql-sibling-names
        (remove name (mapcar 'name ql-sibling-systems)
                :test 'equal))
       (dependencies raw-dependencies)
       (description (asdf:system-description (asdf:find-system system)))
       (release-name (short-description ql-release)))
    (list
     :system system
     :description description
     :sha256 archive-sha256
     :url url
     :md5 ideal-md5
     :name name
     :dependencies dependencies
     :siblings ql-sibling-names
     :release-name release-name
     :parasites parasites)))

(defvar *error-escape-valve* *error-output*
  "When `*error-output*' is rebound to inhibit spew, this stream will
still produce output.")

(defun print-usage-and-quit ()
  "Describe how to use this program... and then exit."
  (format *error-output* "Usage:
    ~A [--cacheDir <dir>] [--silent] [--debug] [--help|-h] <system-name>
Arguments:
    --cacheDir Store (and look for) compiled lisp files in the given directory
    --verbose Show compilation output
    --debug Enter the debugger when a fatal error is encountered
    --help Print usage and exit
    <system-name> The quicklisp system to examine
" (or (uiop:argv0) "quicklisp-to-nix-system-info"))
  (uiop:quit 2))

(defun main ()
  "Make it go."
  (let ((argv (uiop:command-line-arguments))
        cache-dir
        target-system
        verbose-p
        debug-p)
    (handler-bind
        ((warning
          (lambda (w)
            (format *error-escape-valve* "~A~%" w)))
         (error
          (lambda (e)
            (if debug-p
                (invoke-debugger e)
                (progn
                  (format *error-escape-valve* "~
Failed to extract system info. Details are below. ~
Run with --debug and/or --verbose for more info.
~A~%" e)
                  (uiop:quit 1))))))
      (loop :while argv :do
         (cond
           ((equal "--cacheDir" (first argv))
            (pop argv)
            (unless argv
              (error "--cacheDir expects an argument"))
            (setf cache-dir (first argv))
            (pop argv))

           ((equal "--verbose" (first argv))
            (setf verbose-p t)
            (pop argv))

           ((equal "--debug" (first argv))
            (setf debug-p t)
            (pop argv))

           ((or (equal "--help" (first argv))
                (equal "-h" (first argv)))
            (print-usage-and-quit))

           (t
            (setf target-system (pop argv))
            (when argv
              (error "Can only operate on one system")))))

      (unless target-system
        (print-usage-and-quit))

      (when cache-dir
        (setf cache-dir (pathname-as-directory (parse-namestring cache-dir))))

      (with-quicklisp (dir) (:cache-dir (or cache-dir :temp))
        (declare (ignore dir))

        (let (system-data)
          (let ((*error-output* (if verbose-p
                                    *error-output*
                                    (make-broadcast-stream)))
                (*standard-output* (if verbose-p
                                       *standard-output*
                                       (make-broadcast-stream)))
                (*trace-output* (if verbose-p
                                    *trace-output*
                                    (make-broadcast-stream))))
            (format *error-output*
                    "quicklisp-to-nix-system-info ~A~%ASDF ~A~%Quicklisp ~A~%Compiler ~A ~A~%"
                    *version*
                    (asdf:asdf-version)
                    (funcall (intern "CLIENT-VERSION" :ql))
                    (lisp-implementation-type)
                    (lisp-implementation-version))
            (setf system-data (system-data target-system)))

          (cond
            (system-data
             (format t "~W~%" system-data)
             (uiop:quit 0))
            (t
             (format *error-output* "Failed to determine system data~%")
             (uiop:quit 1))))))))

(defun dump-image ()
  "Make an executable"
  (setf uiop:*image-entry-point* #'main)
  (setf uiop:*lisp-interaction* nil)
  (uiop:dump-image "quicklisp-to-nix-system-info" :executable t))
