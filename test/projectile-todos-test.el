;;; projectile-todos-test.el --- Tests for the project TODO collector -*- lexical-binding: t -*-

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

;; Tests for `projectile-todos', the annotation collector built on top of the
;; read-only search reviewer.  The keyword regexp gets the closest scrutiny:
;; a pattern that matches `MASTODON' would make the command useless.

;;; Code:

(require 'projectile-test-helpers)
(require 'crm)

(defun projectile-todos-test--matches-p (regexp string)
  "Return non-nil when REGEXP matches STRING case-sensitively."
  (let ((case-fold-search nil))
    (and (string-match-p regexp string) t)))

(defun projectile-todos-test--run (&optional arg)
  "Run `projectile-todos' with ARG and return the results buffer."
  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
    (projectile-todos arg))
  (get-buffer projectile-search-buffer-name))

(defun projectile-todos-test--strings (buf)
  "Return the matched strings collected in BUF, in order."
  (with-current-buffer buf
    (mapcar #'projectile-replace--match-string projectile-replace--matches)))

(describe "projectile-todos--regexp"
  (let ((regexp (projectile-todos--regexp '("TODO" "FIXME"))))
    (it "matches a keyword followed by a colon"
      (expect (projectile-todos-test--matches-p regexp ";; TODO: fix this")
              :to-be-truthy))

    (it "matches a keyword followed by whitespace"
      (expect (projectile-todos-test--matches-p regexp "# FIXME this is wrong")
              :to-be-truthy)
      (expect (projectile-todos-test--matches-p regexp "//\tTODO\tlater")
              :to-be-truthy))

    (it "matches a keyword at the end of the line"
      (expect (projectile-todos-test--matches-p regexp ";; TODO")
              :to-be-truthy))

    (it "matches without a comment character in front"
      (expect (projectile-todos-test--matches-p regexp "print(\"TODO: soon\")")
              :to-be-truthy)
      (expect (projectile-todos-test--matches-p regexp "TODO: at line start")
              :to-be-truthy))

    (it "does not match a longer word starting with the keyword"
      (expect (projectile-todos-test--matches-p regexp "my TODOS list")
              :to-be nil)
      (expect (projectile-todos-test--matches-p regexp "TODOS: many")
              :to-be nil))

    (it "does not match a keyword embedded in another word"
      (expect (projectile-todos-test--matches-p regexp "MASTODON: a bird")
              :to-be nil)
      (expect (projectile-todos-test--matches-p regexp "unFIXME: nope")
              :to-be nil))

    (it "is case-sensitive"
      (expect (projectile-todos-test--matches-p regexp ";; todo: later")
              :to-be nil)
      (expect (projectile-todos-test--matches-p regexp ";; Fixme: later")
              :to-be nil))

    (it "matches only the keywords it was given"
      (expect (projectile-todos-test--matches-p regexp ";; HACK: nope")
              :to-be nil))))

(describe "projectile-todos--rg-pattern"
  (it "spells the same fencing in ripgrep's syntax"
    (expect (projectile-todos--rg-pattern '("TODO" "FIXME"))
            :to-equal "\\b(?:TODO|FIXME)\\b(?::|[[:blank:]]|$)"))

  (it "escapes punctuation that Rust regex would take as syntax"
    (expect (projectile-todos--rg-pattern '("@TODO" "TODO?"))
            :to-equal "\\b(?:\\@TODO|TODO\\?)\\b(?::|[[:blank:]]|$)")))

(describe "projectile-todos"
  (it "collects the default keywords across the project, ignoring near-misses"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n;; TODOS are not annotations\n;; todo: nor is this\n")
         ("lib/b.py" . "# FIXME broken\nMASTODON = 1\n# HACK\n")
         ("c.txt" . "nothing to see here\n"))
      (let ((buf (projectile-todos-test--run)))
        ;; file order is the indexer's, so compare as a set
        (expect (sort (projectile-todos-test--strings buf) #'string<)
                :to-equal '("FIXME " "HACK" "TODO:"))
        (with-current-buffer buf
          (expect major-mode :to-equal 'projectile-search-mode)
          ;; annotation keywords are uppercase by convention
          (expect projectile-replace--case-fold :to-be nil)
          (expect projectile-replace--literal :to-be nil)
          ;; the whole-word fence would break the already-fenced pattern
          (expect projectile-replace--word :to-be nil)
          (expect (buffer-string) :to-match "a\\.el (1)")
          (expect (buffer-string) :to-match "lib/b\\.py (2)")
          (expect (buffer-string) :not :to-match "c\\.txt")))))

  (it "reports nothing to review when the project is clean"
    (projectile-test-with-project
        (("a.el" . ";; all good here\n"))
      (spy-on 'message)
      (expect (projectile-todos-test--run) :to-be nil)))

  (it "seeds the ripgrep-syntax equivalent of the pattern"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n"))
      (with-current-buffer (projectile-todos-test--run)
        (expect projectile-replace--rg-pattern
                :to-equal (projectile-todos--rg-pattern
                           projectile-todo-keywords))
        ;; toggling the search's shape drops the ripgrep spelling, since it
        ;; describes the old search
        (projectile-replace--toggle-regexp)
        (expect projectile-replace--rg-pattern :to-be nil))))

  (it "honors a customized keyword list"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n;; REVIEW: this\n"))
      (let* ((projectile-todo-keywords '("REVIEW"))
             (buf (projectile-todos-test--run)))
        (expect (projectile-todos-test--strings buf) :to-equal '("REVIEW:")))))

  (it "refuses to search with an empty keyword list"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n"))
      (let ((projectile-todo-keywords nil))
        (expect (projectile-todos-test--run) :to-throw 'user-error)))))

(describe "projectile-todos with a prefix argument"
  (it "searches only the keywords picked at the prompt"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n;; FIXME: two\n;; HACK: three\n"))
      (spy-on 'completing-read-multiple :and-return-value '("FIXME" "HACK"))
      (let ((buf (projectile-todos-test--run '(4))))
        (expect (projectile-todos-test--strings buf)
                :to-equal '("FIXME:" "HACK:"))
        (expect (spy-calls-count 'completing-read-multiple) :to-equal 1))))

  (it "accepts a keyword that is not in the customized list"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n;; REVIEW: two\n"))
      (spy-on 'completing-read-multiple :and-return-value '("REVIEW"))
      (expect (projectile-todos-test--strings (projectile-todos-test--run '(4)))
              :to-equal '("REVIEW:"))))

  (it "refuses an empty selection"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n"))
      (spy-on 'completing-read-multiple :and-return-value '(""))
      (expect (projectile-todos-test--run '(4)) :to-throw 'user-error))))

(describe "projectile-todos ripgrep fast-path"
  (before-each
    (spy-on 'executable-find :and-call-fake
            (lambda (command &rest _) (and (equal command "rg") "rg"))))

  (it "opens the fast-path to a non-literal search carrying an rg pattern"
    (let ((noninteractive nil) (projectile-search-use-ripgrep t))
      (expect (projectile-search--rg-fastpath-p nil "\\bTODO\\b") :to-be-truthy)
      ;; ... but not to a plain regexp search
      (expect (projectile-search--rg-fastpath-p nil) :to-be nil))
    (let ((noninteractive nil) (projectile-search-use-ripgrep nil))
      (expect (projectile-search--rg-fastpath-p nil "\\bTODO\\b") :to-be nil)))

  (it "searches the pattern as a regexp rather than a fixed string"
    (let* ((pattern (projectile-todos--rg-pattern '("TODO")))
           (cmd (projectile-search--rg-command "ignored" nil nil nil pattern)))
      (expect (member "--fixed-strings" cmd) :to-be nil)
      (expect (member "--case-sensitive" cmd) :to-be-truthy)
      (let ((tail (cdr (member "--" cmd))))
        (expect (car tail) :to-equal pattern)
        (expect (cadr tail) :to-equal "./"))))

  (it "routes a todos buffer to the ripgrep gather"
    (projectile-test-with-project
        (("a.el" . ";; TODO: one\n"))
      (spy-on 'projectile-search--gather-rg)
      (spy-on 'projectile-replace--gather-async)
      (let ((noninteractive nil) (projectile-search-use-ripgrep t))
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (projectile-todos))
        (expect (spy-calls-count 'projectile-search--gather-rg) :to-equal 1)
        (expect (spy-calls-count 'projectile-replace--gather-async)
                :to-equal 0)))))

(provide 'projectile-todos-test)

;;; projectile-todos-test.el ends here
