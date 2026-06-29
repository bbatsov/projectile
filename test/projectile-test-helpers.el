;;; projectile-test-helpers.el --- Shared helpers for Projectile's test suite -*- lexical-binding: t -*-

;; Copyright © 2011-2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; Macros and utilities shared across Projectile's Buttercup test files.
;; Every `projectile-*-test.el' file requires this module, so the test
;; suite can be split into focused files without duplicating the setup.

;;; Code:

;; needed for the tests to work with native compilation
(with-eval-after-load 'comp
  (push 'insert-file-contents
        native-comp-never-optimize-functions))

(require 'projectile)
(require 'buttercup)

;; The sandbox rewrites the same paths (e.g. `.projectile') repeatedly.  On
;; some Emacs builds `write-region' then decides the file changed on disk and
;; raises a supersession threat, which in batch mode can't be answered and
;; aborts the test with "Cannot resolve conflict in batch mode".  Neutralise
;; the prompt for the test run; the function name differs across Emacs
;; versions, so override whichever is bound.
(dolist (fn '(ask-user-about-supersession-threat
              userlock--ask-user-about-supersession-threat))
  (when (fboundp fn)
    (advice-add fn :override #'ignore)))

;; Useful debug information
(message "Running tests on Emacs %s" emacs-version)

;; TODO: Revise this init logic
(defvar projectile-test-path (let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
                                    (source-directory (locate-dominating-file current-file "Eldev"))
                                    ;; Do not load outdated byte code for tests
                                    (load-prefer-newer t))
                               ;; Load the file under test
                               (load (expand-file-name "projectile" source-directory))
                               (expand-file-name "test" source-directory)))

;;; Test Utilities
(defmacro projectile-test-with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory.

Path-keyed caches (`projectile-project-vcs-cache' etc.) are bound to
fresh hash tables for the duration of BODY so that one test's view of
the sandbox doesn't leak into the next via the cache - the sandbox
directory is reused across tests, so a cached VCS or project-type
answer from a previous test would otherwise still be live."
  (declare (indent 0) (debug (&rest form)))
  `(let ((sandbox (expand-file-name
                   (convert-standard-filename "test/sandbox/")
                   (file-name-directory (locate-library "projectile.el" t))))
         (projectile-project-vcs-cache (make-hash-table :test 'equal))
         ;; The sandbox path is reused across tests, so repeatedly writing
         ;; the same files (e.g. `.projectile') can trip Emacs's file-lock
         ;; supersession machinery on some builds - it calls
         ;; `expand-file-name' on the non-file-visiting temp buffer's nil
         ;; `buffer-file-name' and errors out.  We never want lock files in
         ;; the throwaway sandbox anyway.
         (create-lockfiles nil))
     (when (file-directory-p sandbox)
       (delete-directory sandbox t))
     (make-directory sandbox t)
     (let ((default-directory sandbox))
       ,@body)))

(defmacro projectile-test-with-files (files &rest body)
  "Evaluate BODY in the presence of FILES.

You'd normally combine this with `projectile-test-with-sandbox'."
  (declare (indent 1) (debug (sexp &rest form)))
  `(progn
     ,@(mapcar (lambda (file)
                 (if (string-suffix-p "/" file)
                     `(make-directory ,file t)
                   `(let ((dir (file-name-directory ,file)))
                      (when dir
                        (make-directory dir t))
                      (with-temp-file ,file))))
               files)
     ,@body))

(defmacro projectile-test-with-files-using-custom-project (files project-options &rest body)
  "Evaluate BODY with the custom project having PROJECT-OPTIONS with FILES."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  `(let ((projectile-indexing-method 'native)
         (projectile-projects-cache (make-hash-table :test 'equal))
         (projectile-projects-cache-time (make-hash-table :test 'equal))
         (projectile-enable-caching 'persistent))
     ,@(mapcar (lambda (file)
                 (let* ((path (concat "project/" file))
                        (dir (file-name-directory path)))
                   (if (string-suffix-p "/" file)
                       `(make-directory ,path t)
                     `(progn
                        (make-directory ,dir t)
                        (with-temp-file ,path)))))
               files)
     (projectile-register-project-type 'sample-project '("somefile") ,@project-options)
     (spy-on 'projectile-project-type :and-return-value 'sample-project)
     (spy-on 'projectile-project-root :and-return-value (file-truename (expand-file-name "project/")))
     ,@body))

(defmacro projectile-test-with-stub-root (root files &rest body)
  "Evaluate BODY in a sandbox project rooted at ROOT containing FILES.

ROOT is a directory name relative to the sandbox.  FILES are created
relative to ROOT (so you don't have to repeat the root prefix).
`projectile-project-root' is stubbed to return ROOT's resolved absolute
name for the duration of BODY.

This bundles the very common sandbox + files + `projectile-project-root'
spy dance into a single form.

Like `projectile-test-with-files', ROOT and FILES are inspected at
macro-expansion time and must therefore be literals, not variables."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let* ((root-dir (file-name-as-directory root))
         (rooted (cons root-dir
                       (mapcar (lambda (f) (concat root-dir f)) files))))
    `(projectile-test-with-sandbox
       (projectile-test-with-files ,rooted
         ;; `file-truename' so the stubbed root matches what callers get
         ;; when they resolve paths under the (symlinked, on macOS) sandbox.
         (spy-on 'projectile-project-root
                 :and-return-value (file-truename (expand-file-name ,root-dir)))
         ,@body))))

(defmacro assert-friendly-error-when-no-project (fn)
  "Write a test that ensures FN throws a friendly error when called without a project."
  (let ((description (concat "when calling " (symbol-name fn) " without a project")))
    `(describe
      ,description
      :var ((fn ',fn))
      (it "throws a friendly error"
          (projectile-test-with-sandbox
           (projectile-test-with-files
            ("index.html")
            (find-file-noselect "index.html" t)
            ;; Avoid "the current buffer is not visiting a file" error
            (write-file "index.html")
            (spy-on 'projectile-project-root :and-return-value nil)
            (let ((projectile-require-project-root t))
              (expect (call-interactively fn)
                      :to-throw
                      'error
                      (list (concat "Projectile cannot find a project definition in "
                                    default-directory))))))))))

(defun projectile-test-tmp-file-path ()
  "Return a filename suitable to save data to in the test temp directory."
  (concat projectile-test-path
          "/tmp/temporary-file-" (format "%d" (random))
          ".eld"))

(defun projectile-test-wait-for (predicate &optional timeout)
  "Block until PREDICATE returns non-nil or TIMEOUT seconds elapse.
Pumps the event loop with `accept-process-output' so asynchronous
process filters and sentinels get a chance to run.  TIMEOUT defaults to
10 seconds.  Returns the predicate's last value (nil on timeout)."
  (let ((deadline (+ (float-time) (or timeout 10)))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    result))

(defun file-handler-for-tests (operation &rest args)
  "Handler for # files.
Just delegates OPERATION and ARGS for all operations except for
`shell-command' / `process-file-shell-command' (the entry points used
by `projectile-files-via-ext-command')."
  (let ((inhibit-file-name-handlers
         (cons 'file-handler-for-tests
               (and (eq inhibit-file-name-operation operation)
                  inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (cond ((memq operation '(shell-command process-file process-file-shell-command))
           ;; Re-run in a real directory so the underlying call doesn't
           ;; choke on the synthetic `#magic#' path.  We always emit
           ;; "magic" regardless of the requested command - the tests
           ;; that hit this handler only care that the magic dispatch
           ;; happened at all.
           (let ((default-directory temporary-file-directory))
             (process-file-shell-command "echo magic" nil t)))
          (t (apply operation args)))))

(add-to-list 'file-name-handler-alist (cons "^#" 'file-handler-for-tests))

(provide 'projectile-test-helpers)

;;; projectile-test-helpers.el ends here
