(defpackage :quicklisp-dists-to-nix
  (:use :common-lisp))
(in-package :quicklisp-dists-to-nix)

(declaim (optimize (debug 3) (safety 3)))

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

(defun sha-char-p (char)
  (or (digit-char-p char)
      (find char "abcdef")))

(defun sha256 (path)
  (let* ((expected-sha-length (/ 256 4)) ;; 512 bits, 4 bits per hex digit
         (sha-string (uiop:run-program `("sha256sum" ,(namestring path)) :output '(:string :stripped t))))
    (unless (> (length sha-string) expected-sha-length)
      (error "sha256sum output not long enough to contain a sha256.  Got~%~A" sha-string))

    ;; Copy just the SHA out and validate that every character is a hex digit
    (let ((result (make-array expected-sha-length :element-type 'character)))
      (loop :for index :below expected-sha-length :do
         (progn
           (unless (sha-char-p (aref sha-string index))
             (error "non-hex char found in sha: ~A.  Found in:~%~A" (aref sha-string index) sha-string))
           (setf (aref result index) (aref sha-string index))))
      result)))

(defun fetchurl-for-release-src (release out)
  (let* ((path (ql-dist:ensure-local-archive-file release))
         (sha256 (sha256 path)))
    (indent-format out "fetchurl {~%")
    (with-indent
      (indent-format out "url = \"~A\";~%" (ql-dist:archive-url release))
      (indent-format out "sha256 = \"~A\";~%" sha256))
    (indent-format out "}")))

(defun nix-package-for-release (release out)
  (indent-format out "buildLispPackage {~%")
  (with-indent
    (indent-format out "inherit clwrapper stdenv;~%")
    (indent-format out "baseName = \"~A\";~%" (ql-dist:project-name release))
    (indent-format out "version = \"~A\";~%" (ql-dist:version (ql-dist:dist release)))
    (indent-format out "buildSystems = [];~%")
    (indent-format out "description = \"~A\";~%" (ql-dist:short-description release))
    (indent-format out "deps = [];~%")
    (progn
      (indent-format out "src = ")
      (fetchurl-for-release-src release out)
      (indent-format out ";~%"))
    (progn
      (indent-format out "asdFilesToKeep = [")
      (dolist (asd-file (ql-dist:system-files release))
        (indent-format out " \"~A\"" (namestring asd-file)))
      (indent-format out " ];~%")))
  (indent-format out "}"))

(defun attrset-for-dist (dist out)
  (indent-format out "rec {~%")
  (with-indent
    (indent-format out "byRelease = {~%")
    (with-indent
      (dolist (release (ql-dist:provided-releases dist))
        (indent-format out "\"~A\" = " (ql-dist:project-name release))
        (nix-package-for-release release out)
        (indent-format out ";~%")))
    (indent-format out "};~%")
    (indent-format out "bySystem = {~%")
    (with-indent
      (dolist (system (ql-dist:provided-systems dist))
        (let* ((release (ql-dist:release system))
               (release-name (ql-dist:project-name release))
               (system-name (ql-dist:name system)))
          (indent-format out "\"~A\" = byRelease.\"~A\";~%" system-name release-name))))
    (indent-format out "};~%"))
  (indent-format out "}"))

(defun attrset-for-all-dists (out)
  (indent-format out "{~%")
  (with-indent
    (dolist (dist (ql-dist:all-dists))
      (indent-format out "\"~A\" = " (ql-dist:name dist))
      (attrset-for-dist dist out)
      (indent-format out ";~%")))
  (indent-format out "}"))

(with-open-file (out #P"dists.nix" :if-exists :supersede :direction :output)
  (indent-format out "{ pkgs, clwrapper, stdenv, fetchurl, buildLispPackage }:~%")
  (attrset-for-all-dists out)
  (indent-format out "~%"))
