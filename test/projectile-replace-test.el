;;; projectile-replace-test.el --- Tests for the replace commands -*- lexical-binding: t -*-

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

;; Tests for `projectile-replace' and `projectile-replace-regexp'.

;;; Code:

(require 'projectile-test-helpers)

(defmacro projectile-replace-test--with-project (files &rest body)
  "Like `projectile-test-with-project' but with the replace smart-case bindings.
BODY additionally runs with `search-upper-case' and `case-replace' set, so
the case-handling expectations are independent of the environment: a
lower-case search folds case, a mixed-case one doesn't."
  (declare (indent 1) (debug (sexp &rest form)))
  `(projectile-test-with-project ,files
     (let ((search-upper-case t)
           (case-replace t))
       ,@body)))

(defun projectile-replace-test--replace (old new &optional regexp-p)
  "Replace OLD with NEW in the current project, auto-confirming everything.

Drives `projectile-replace', or `projectile-replace-regexp' when
REGEXP-P is non-nil, with the prompts stubbed out and the query loop of
`perform-replace' short-circuited."
  (let ((inputs (list old new))
        (real-perform-replace (symbol-function 'perform-replace)))
    (spy-on 'read-string :and-call-fake (lambda (&rest _) (pop inputs)))
    (spy-on 'perform-replace :and-call-fake
            (lambda (from to _query-flag &rest args)
              ;; Perform the replacement without querying, and return
              ;; non-nil so `fileloop-continue' moves on to the next file
              ;; (the non-query `perform-replace' returns nil, which
              ;; `fileloop' would take as a request to stop).
              (apply real-perform-replace from to nil args)
              t))
    ;; `fileloop-continue' leaves the last processed file's buffer
    ;; current, which would hijack the spec's `default-directory'
    (save-current-buffer
      (condition-case nil
          (call-interactively (if regexp-p #'projectile-replace-regexp #'projectile-replace))
        ;; `fileloop-continue' signals "All files processed" via
        ;; `user-error' when it runs out of files - that's how the loop
        ;; normally terminates, not a failure.
        (user-error nil)))))

(defun projectile-replace-test--contents (file)
  "Return the current contents of project FILE.
Prefers a live buffer over the file on disk, since the replace commands
modify buffers without saving them."
  (let ((file (expand-file-name file)))
    (if-let* ((buffer (get-file-buffer file)))
        (with-current-buffer buffer (buffer-string))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(describe "projectile-replace"
  (it "replaces a literal string across multiple project files"
    (projectile-replace-test--with-project
        (("a.txt" . "foo one foo\n")
         ("lib/b.txt" . "start foo end\n"))
      (projectile-test-use-plain-grep)
      (projectile-replace-test--replace "foo" "bar")
      (expect (projectile-replace-test--contents "a.txt")
              :to-equal "bar one bar\n")
      (expect (projectile-replace-test--contents "lib/b.txt")
              :to-equal "start bar end\n")))

  (it "treats the input literally rather than as a regexp"
    (projectile-replace-test--with-project
        (("code.txt" . "call f(x) + 1\n")
         ("decoy.txt" . "fx is not a match\n"))
      (projectile-test-use-plain-grep)
      (projectile-replace-test--replace "f(x)" "g(y)")
      (expect (projectile-replace-test--contents "code.txt")
              :to-equal "call g(y) + 1\n")
      ;; as a regexp "f(x)" would also match the "fx" here
      (expect (projectile-replace-test--contents "decoy.txt")
              :to-equal "fx is not a match\n")))

  (it "leaves files without a match untouched"
    (projectile-replace-test--with-project
        (("match.txt" . "some foo here\n")
         ("nomatch.txt" . "nothing to see\n"))
      (projectile-test-use-plain-grep)
      (projectile-replace-test--replace "foo" "bar")
      (expect (projectile-replace-test--contents "match.txt")
              :to-equal "some bar here\n")
      (expect (projectile-replace-test--contents "nomatch.txt")
              :to-equal "nothing to see\n")
      ;; the no-match file is only scanned, never really visited
      (expect (get-file-buffer (expand-file-name "nomatch.txt")) :to-be nil)))

  (it "replaces matches before point in already-visited buffers (#1677)"
    (projectile-replace-test--with-project
        (("open.txt" . "first foo\nmiddle\nlast foo\n")
         ("closed.txt" . "foo here\n"))
      (projectile-test-use-plain-grep)
      (with-current-buffer (find-file-noselect (expand-file-name "open.txt"))
        (goto-char (point-max)))
      (projectile-replace-test--replace "foo" "bar")
      (expect (projectile-replace-test--contents "open.txt")
              :to-equal "first bar\nmiddle\nlast bar\n")
      (expect (projectile-replace-test--contents "closed.txt")
              :to-equal "bar here\n")))

  (it "replaces matches that differ in case from a lower-case input (#1115)"
    (projectile-replace-test--with-project
        (("case.txt" . "Foo bar\n"))
      ;; the listing must go through a real (case-sensitive by default)
      ;; external tool for this to prove anything
      (projectile-test-use-plain-grep)
      (projectile-replace-test--replace "foo" "qux")
      ;; standard `query-replace' semantics: the match is folded and the
      ;; replacement preserves its case
      (expect (projectile-replace-test--contents "case.txt")
              :to-equal "Qux bar\n")))

  (it "is case-sensitive when the input contains upper-case characters"
    (projectile-replace-test--with-project
        (("case.txt" . "Foo bar foo\n")
         ("lower.txt" . "foo only\n"))
      (projectile-test-use-plain-grep)
      (projectile-replace-test--replace "Foo" "Qux")
      (expect (projectile-replace-test--contents "case.txt")
              :to-equal "Qux bar foo\n")
      ;; listed by the case-insensitive grep, then skipped by the
      ;; case-sensitive Emacs-side search
      (expect (projectile-replace-test--contents "lower.txt")
              :to-equal "foo only\n")))

  (it "works when the project root is in abbreviated form (#1115)"
    (projectile-replace-test--with-project
        (("a.txt" . "hello foo world\n"))
      (projectile-test-use-plain-grep)
      ;; roots commonly come back as "~/..." because `find-file' sets an
      ;; abbreviated `default-directory'; the external listing must still
      ;; line up with the expanded project file list
      (spy-on 'projectile-acquire-root
              :and-return-value (abbreviate-file-name default-directory))
      (projectile-replace-test--replace "foo" "bar")
      (expect (projectile-replace-test--contents "a.txt")
              :to-equal "hello bar world\n"))))

(describe "projectile-replace-regexp"
  (it "replaces regexp matches across multiple project files"
    (projectile-replace-test--with-project
        (("a.txt" . "num 123 and 456\n")
         ("b.txt" . "just 7\n")
         ("c.txt" . "no digits\n"))
      (projectile-replace-test--replace "[0-9]+" "N" 'regexp)
      (expect (projectile-replace-test--contents "a.txt")
              :to-equal "num N and N\n")
      (expect (projectile-replace-test--contents "b.txt")
              :to-equal "just N\n")
      (expect (projectile-replace-test--contents "c.txt")
              :to-equal "no digits\n")))

  (it "replaces matches before point in already-visited buffers (#1677)"
    (projectile-replace-test--with-project
        (("open.txt" . "alpha 1\nbeta 2\n"))
      (with-current-buffer (find-file-noselect (expand-file-name "open.txt"))
        (goto-char (point-max)))
      (projectile-replace-test--replace "[0-9]" "N" 'regexp)
      (expect (projectile-replace-test--contents "open.txt")
              :to-equal "alpha N\nbeta N\n"))))

;; These pin the exact command string each search tool's constructor
;; emits, guarding the descriptor-driven implementation against drift.
;; SEARCH-TERM is passed pre-quoted, matching how `projectile-files-with-string'
;; calls these.
(describe "projectile search-command constructors"
  (describe "without a file-extension filter"
    (it "builds the rg command"
      (expect (projectile--rg-construct-command "foo")
              :to-equal "rg -liF --no-heading --color never foo"))
    (it "builds the ag command"
      (expect (projectile--ag-construct-command "foo")
              :to-equal "ag --literal --ignore-case --nocolor --noheading -l foo"))
    (it "builds the ack command"
      (expect (projectile--ack-construct-command "foo")
              :to-equal "ack --literal --ignore-case --nocolor -l foo"))
    (it "builds the git-grep command"
      (expect (projectile--git-grep-construct-command "foo")
              :to-equal "git grep -HlIiF foo"))
    (it "builds the grep command"
      (expect (projectile--grep-construct-command "foo")
              :to-equal "grep -rHlIiF foo .")))

  (describe "with a file-extension filter"
    (it "builds the rg command with a glob"
      (expect (projectile--rg-construct-command "foo" "*.el")
              :to-equal "rg -liF --no-heading --color never -g '*.el' foo"))
    (it "builds the ag command with an anchored regexp"
      (expect (projectile--ag-construct-command "foo" "*.el")
              :to-equal "ag --literal --ignore-case --nocolor --noheading -l -G \\.el$ foo"))
    (it "builds the ack command as a piped listing"
      (expect (projectile--ack-construct-command "foo" "*.el")
              :to-equal "ack -g '\\.el$' | ack --literal --ignore-case --nocolor -l -x foo"))
    (it "builds the git-grep command with a pathspec"
      (expect (projectile--git-grep-construct-command "foo" "*.el")
              :to-equal "git grep -HlIiF foo  -- '*.el'"))
    (it "builds the grep command with an include"
      (expect (projectile--grep-construct-command "foo" "*.el")
              :to-equal "grep -rHlIiF foo . --include '*.el'"))))

(describe "projectile-files-with-string"
  (it "lists files case-insensitively, leaving case handling to the replace itself (#1115)"
    (projectile-replace-test--with-project
        (("exact.txt" . "foo\n")
         ("cased.txt" . "FOO\n")
         ("none.txt" . "bar\n"))
      (projectile-test-use-plain-grep)
      (let ((files (projectile-files-with-string "foo" default-directory)))
        (expect (sort files #'string<)
                :to-equal (list (expand-file-name "cased.txt")
                                (expand-file-name "exact.txt")))))))

(describe "projectile-files-from-cmd"
  (it "returns canonical expanded file names even for non-canonical directories (#1115)"
    (projectile-test-with-sandbox
      (projectile-test-with-files
          ("project/a.txt")
        (assume (projectile-unixy-system-p) "needs unixy text utilities")
        ;; a valid but non-canonical spelling of the project directory
        (let ((dir (concat default-directory "./project/")))
          (expect (projectile-files-from-cmd "echo ./a.txt" dir)
                  :to-equal (list (expand-file-name "project/a.txt"))))))))

;;; projectile-replace-test.el ends here
