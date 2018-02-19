(defpackage :quicklisp-package-to-nix
  (:use :common-lisp))
(in-package :quicklisp-package-to-nix)
(require :asdf)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))

(defvar *releases-path* "@releaseListPath@")
(defvar *systems-path* "@systemListPath@")

(defvar *closure-systems* nil)

(defun closure-systems ()
  (when *closure-systems*
    (return-from closure-systems *closure-systems*))
  (setf *closure-systems* (make-hash-table :test 'equal))
  (with-open-file (stream *systems-path*)
    (loop :for system-name = (read-line stream nil :eof)
       :until (eq :eof system-name) :do
       (setf (gethash system-name *closure-systems*) t)))
  *closure-systems*)

(defun system-in-closure-p (system-name)
  (nth-value 1 (gethash system-name (closure-systems))))

(defun host-system-for (system-name)
  (let ((system (asdf:find-system system-name)))
    (asdf:find-system (pathname-name (asdf:system-source-file system)) nil)))

(defun closure-system-for-system (system)
  (let (host)
    (cond
      ((system-in-closure-p (asdf:component-name system))
       (asdf:component-name system))
      ((and (setf host (host-system-for system))
            (system-in-closure-p (asdf:component-name host)))
       (asdf:component-name host))
      (t
       (error "Couldn't find ~A in the system closure" (asdf:component-name system))))))

(defvar *indent* 0)
(defvar *should-indent* t)

(defmacro with-indent (&body body)
  `(let ((*indent* (+ *indent* 2)))
     ,@body))

(defun indent-format (out format &rest args)
  (when *should-indent*
    (dotimes (i *indent*)
      (format out " "))
    (setf *should-indent* nil))
  (let ((str (apply #'format nil format args)))
    (write-string str out)
    (when (equal #\linefeed (aref str (1- (length str))))
      (setf *should-indent* t))))

(defun change-asdf-search-path ()
  (asdf:clear-source-registry)
  (let ((paths
         (with-open-file (file *releases-path*)
           (loop :for line = (read-line file nil :eof)
              :until (eq line :eof) :collect
              `(:tree ,(merge-pathnames "lib/common-lisp/" (uiop:ensure-directory-pathname line)))))))
    (asdf:initialize-source-registry `(:source-registry :ignore-inherited-configuration ,@paths))))

(defun escape-filename (s)
  (format 
   nil "~a~{~a~}"
   (if (or (alpha-char-p (aref s 0))
           (equal (aref s 0) #\_))
       ""
       "_")
   (loop
      for x in (map 'list 'identity s)
      collect
        (case x
          (#\/ "_slash_")
          (#\\ "_backslash_")
          (#\_ "__")
          (#\. "_dot_")
          (t x)))))

(defvar *memoized-dependencies* (make-hash-table :test 'equal))

(defun dependencies-for (system)
  (let ((memoized-result (gethash (asdf:component-name system) *memoized-dependencies*)))
    (when memoized-result
      (return-from dependencies-for memoized-result)))
  (let ((raw-dependencies (asdf:system-depends-on system))
        (found-dependencies (make-hash-table :test 'equal)))
    (labels ((found (name)
               (when (system-in-closure-p name)
                 (setf (gethash name found-dependencies) t))))
      (dolist (dep raw-dependencies)
        (let* ((dep-name (if (consp dep) (second dep) dep))
               (dep (asdf:find-system dep-name))
               (transitive-deps (dependencies-for dep)))
          (etypecase dep-name
            (symbol
             (setf dep-name (symbol-name dep-name)))
            (string))
          (found dep-name)
          (loop :for transitive-dep :across transitive-deps :do
             (found transitive-dep)))))
    (let ((result (make-array (hash-table-count found-dependencies))))
      (loop :for name :being :the :hash-keys :of found-dependencies
         :for index :from 0 :do
         (setf (aref result index) name))
      (setf (gethash (asdf:component-name system) *memoized-dependencies*) result)
      result)))

(defun nix-derivation-for (system &key (out *standard-output*))
  (let ((dependencies (sort (dependencies-for system) #'string<)))
    (indent-format out "{ stdenv, dist, lispPackages }:~%")
    (indent-format out "stdenv.mkDerivation {~%")
    (with-indent
        (indent-format out "meta = {~%")
      (with-indent
          (let ((description (asdf:system-description system)))
            (when description
              (indent-format out "description = ''~A'';~%" description))))
      (indent-format out "};~%")
      (let ((name (or (asdf:component-name system) "anonymous"))
            (version (or (asdf:component-version system) "latest")))
        (indent-format out "version = \"~A\";~%" version)
        (indent-format out "name = \"~A-~A\";~%" name version))

      (indent-format out "propagatedBuildInputs = [ dist.\"~A\" " (closure-system-for-system system))
      (loop :for dep :across dependencies :do
         (indent-format out "lispPackages.\"~A\" " (escape-filename dep)))
      (indent-format out "];~%")

      ;; No source for these packages
      (indent-format out "unpackPhase = \"true\";~%")
      (indent-format out "installPhase = \"true\";~%"))
    (indent-format out "}~%")))

(defun umbrella-for (systems &key (out *standard-output*) )
  (indent-format out "{ stdenv, dist, closure ? {} }:~%")
  (indent-format out "let lispPackages = closure // {~%")
  (with-indent
      (loop :for system-name :across systems :do
         (let ((name (escape-filename system-name)))
           (indent-format out "\"~A\" = import ./quicklisp-to-nix-output/~A.nix { inherit stdenv, dist, lispPackages; };~%" name name))))
  (indent-format out "in lispPackages~%"))

(defun print-usage-and-quit (exit-code)
  (format *error-output* "Usage: quicklisp-package-to-nix [options]
Available options:
-o file
    Specify where the resulting nix packages should be written.  Defaults to current directory..
-i file
    Specify the file containing the list of systems to generate packages for.  Defaults to ~A.
--help
    Print usage and exit.
quicklisp-to-nix outputs a nix expression for each lisp system in the input file.
"
          *systems-path*)
  (uiop:quit exit-code))

(defmacro equal-case (key-form &body cases)
  (let ((key (gensym "KEY")))
    `(let ((,key ,key-form))
       (cond
         ,@(mapcar (lambda (case) (if (eq (car case) 'otherwise)
                                      `(t ,@(cdr case))
                                      `((equal ,key ,(car case)) ,@(cdr case))))
                   cases)))))

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((out-dir *default-pathname-defaults*)
        (input-file *systems-path*))
    (loop :while args :do
       (let ((arg (car args)))
         (equal-case arg

                     ("--help"
                      (print-usage-and-quit 0))

                     ("-o"
                      (pop args)
                      (setf out-dir (pop args))
                      (unless out-dir
                        (format *error-output* "-o requires an argument~%")
                        (print-usage-and-quit 2))
                      (setf out-dir (uiop:ensure-directory-pathname out-dir)))

                     ("-i"
                      (pop args)
                      (setf input-file (pop args))
                      (unless input-file
                        (format *error-output* "-i requires an argument~%")
                        (print-usage-and-quit 2))
                      (setf input-file (parse-namestring input-file)))

                     ("--debug"
                      (pop args)
                      (setf uiop:*lisp-interaction* t))

                     (otherwise
                      (format *error-output* "Unexpected argument ~A~%" arg)
                      (print-usage-and-quit 2)))))

    (change-asdf-search-path)

    (let ((system-names (make-array 0 :adjustable t :fill-pointer t))
          (workable-system-names (make-array 0 :adjustable t :fill-pointer t))
          (failed-system-names (make-array 0 :adjustable t :fill-pointer t))
          (systems-dir (merge-pathnames #P"quicklisp-to-nix-output/" out-dir)))
      (ensure-directories-exist systems-dir)
      (with-open-file (stream input-file)
        (loop :for system-name = (read-line stream nil :eof)
           :until (eq system-name :eof) :do
           (vector-push-extend system-name system-names)))

      (loop :for system-name :across system-names :do
         (handler-case
             (let ((output-path (make-pathname :type "nix" :defaults (merge-pathnames (escape-filename system-name) systems-dir)))
                   (system (asdf:find-system system-name)))
               (with-open-file (out output-path :direction :output :if-exists :supersede)
                 (nix-derivation-for system :out out)
                 (vector-push-extend system-name workable-system-names)))
           ((or asdf:missing-component
             asdf:load-system-definition-error
             asdf::formatted-system-definition-error)
               (e)
             (warn "~A" e)
            (vector-push-extend system-name failed-system-names))))

      (let ((out-path (merge-pathnames #P"quicklisp-to-nix.nix" out-dir)))
        (with-open-file (out out-path :direction :output :if-exists :supersede)
          (umbrella-for workable-system-names :out out)))

      (unless (zerop (length failed-system-names))
        (format *error-output* "Failed to produce the following systems~%")
        (loop :for name :across failed-system-names :do
           (format *error-output* "~A~%" name))))

    (uiop:quit 0)))

(setf uiop:*image-entry-point* #'main)
(setf uiop:*lisp-interaction* nil)
(uiop:dump-image #P"quicklisp-to-nix" :executable t)
(uiop:quit 0)
