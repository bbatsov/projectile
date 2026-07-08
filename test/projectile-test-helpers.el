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
  `(cl-progv projectile--project-cache-vars
       ;; Rebind every registered per-project cache to a fresh table so
       ;; one test's view of the sandbox doesn't leak into the next.
       (mapcar (lambda (_) (make-hash-table :test 'equal))
               projectile--project-cache-vars)
     (let ((sandbox (expand-file-name
                     (convert-standard-filename "test/sandbox/")
                     (file-name-directory (locate-library "projectile.el" t))))
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
         ,@body))))

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

(defmacro projectile-test-with-temp-files (bindings &rest body)
  "Bind each VAR in BINDINGS to a fresh temp file (or directory) for BODY.
Each binding is (VAR) or (VAR SUFFIX) for an empty temp file with the
optional SUFFIX (e.g. \".txt\"), or (VAR :dir) for a temp directory.
Every created path is removed when BODY exits, normally or via error -
files with `delete-file', directories recursively - so a test doesn't
have to hand-roll the `unwind-protect'/`delete-file' dance."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((vars (mapcar #'car bindings)))
    `(let ,(mapcar
            (lambda (b)
              (let ((var (car b)) (arg (cadr b)))
                (cond
                 ((eq arg :dir) `(,var (file-name-as-directory
                                        (make-temp-file "projectile-test-" t))))
                 (arg           `(,var (make-temp-file "projectile-test-" nil ,arg)))
                 (t             `(,var (make-temp-file "projectile-test-"))))))
            bindings)
       (unwind-protect
           (progn ,@body)
         (dolist (projectile-test--tmp (list ,@vars))
           (ignore-errors
             (if (file-directory-p projectile-test--tmp)
                 (delete-directory projectile-test--tmp t)
               (delete-file projectile-test--tmp))))))))

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


;;; Custom matchers

(buttercup-define-matcher :to-have-same-items-as (actual expected)
  "Match when ACTUAL and EXPECTED hold the same items, ignoring order.
A readable stand-in for `(sort ...) :to-equal (sort ...)' - the failure
message reports exactly which items are missing and which are extra."
  (let* ((actual (funcall actual))
         (expected (funcall expected))
         (missing (cl-set-difference expected actual :test #'equal))
         (extra (cl-set-difference actual expected :test #'equal)))
    (cons (and (null missing) (null extra))
          (format "Expected %S to have the same items as %S (missing: %S, extra: %S)"
                  actual expected missing extra))))

(defconst projectile-test--match-accessors
  '(:file projectile-replace--match-file
    :buffer projectile-replace--match-buffer
    :line projectile-replace--match-line
    :column projectile-replace--match-column
    :string projectile-replace--match-string
    :context projectile-replace--match-context
    :enabled projectile-replace--match-enabled)
  "Map of field keyword to accessor for `:to-be-a-match-with'.")

(buttercup-define-matcher :to-be-a-match-with (match plist)
  "Match when MATCH's `projectile-replace--match' fields equal PLIST.
PLIST maps field keywords (`:file' `:line' `:column' `:string' `:context'
`:buffer' `:enabled') to their expected values; only the listed fields
are checked.  Collapses a stack of per-field `expect' forms into one, and
the failure message names every field that didn't match."
  (let* ((m (funcall match))
         (plist (funcall plist))
         (mismatches nil))
    (if (null m)
        (cons nil "Expected a match struct but got nil")
      (cl-loop for (k v) on plist by #'cddr
               for acc = (plist-get projectile-test--match-accessors k)
               for actual = (funcall acc m)
               unless (equal actual v)
               do (push (format "%s: expected %S, got %S" k v actual) mismatches))
      (cons (null mismatches)
            (format "Expected match to have %S but %s" plist
                    (string-join (nreverse mismatches) "; "))))))

;;; Match helpers (reviewable search/replace)

(defun projectile-test-find-match (matches file-suffix)
  "Return the match in MATCHES whose file name ends with FILE-SUFFIX."
  (cl-find-if (lambda (m)
                (string-suffix-p file-suffix (projectile-replace--match-file m)))
              matches))

(defun projectile-test-match-sig (matches root)
  "Return a stable, comparable signature for MATCHES relative to ROOT.
Each `projectile-replace--match' becomes (RELPATH LINE COLUMN STRING);
the list is sorted so two gatherings of the same matches compare `equal'
regardless of the order they were collected in."
  (sort (mapcar (lambda (m)
                  (list (file-relative-name (projectile-replace--match-file m) root)
                        (projectile-replace--match-line m)
                        (projectile-replace--match-column m)
                        (projectile-replace--match-string m)))
                matches)
        (lambda (a b) (string< (format "%S" a) (format "%S" b)))))

;;; Sandbox project helpers

(defun projectile-test-project-root ()
  "Return the truename'd root of the sandbox `project/' subdirectory.
The `projectile-test-with-files-using-custom-project' macro and the
search/replace `--with-project' macros all build the project under a
`project/' directory and stub `projectile-project-root' to this value."
  (file-truename (expand-file-name "project/")))

(defun projectile-test-kill-project-buffers (root)
  "Kill buffers visiting files under ROOT, plus the results buffers.
Discards modifications first (the replace commands leave buffers dirty),
and also kills the `*projectile-search*'/`*projectile-replace*' results
buffers so they don't leak into the next spec."
  (dolist (buffer (buffer-list))
    (when-let* ((file (buffer-file-name buffer)))
      (when (string-prefix-p root (file-truename file))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (let (kill-buffer-query-functions)
          (kill-buffer buffer)))))
  (dolist (name (list projectile-search-buffer-name
                      projectile-replace-buffer-name))
    (when-let* ((buf (get-buffer name)))
      (kill-buffer buf))))

(defun projectile-test-use-plain-grep ()
  "Force `projectile-files-with-string' to shell out to plain grep.
Skips the calling spec when grep isn't available.  Pinning the tool makes
the specs exercise the external listing pipeline deterministically no
matter which of rg/ag/ack happens to be installed."
  (assume (projectile-unixy-system-p) "needs unixy text utilities")
  (spy-on 'executable-find :and-call-fake
          (lambda (command &rest _)
            ;; keep `projectile-unixy-system-p' truthy, hide rg/ag/ack/git
            (member command '("grep" "cut" "uniq")))))

(defun projectile-test-disk (file)
  "Return the on-disk contents of project FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (buffer-string)))

(defun projectile-test-disk-raw (file)
  "Return the raw (unconverted) on-disk bytes of project FILE as a string."
  (with-temp-buffer
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents (expand-file-name file)))
    (buffer-string)))

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
