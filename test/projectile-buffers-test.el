;;; projectile-buffers-test.el --- Tests for project buffers -*- lexical-binding: t -*-

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

;; Tests for project buffers.

;;; Code:

(require 'projectile-test-helpers)

(describe "projectile-find-file-hook-function"
  ;; The hook fans out into several operations; the contract under test
  ;; is which ones are gated behind the remote check.  Cheap ones must
  ;; run for remote buffers too; the slow mode-line update must not.
  :var (called)
  (before-each
    (setq called nil)
    (cl-flet ((track (name) (lambda (&rest _) (push name called))))
      (spy-on 'projectile-maybe-limit-project-file-buffers
              :and-call-fake (track 'limit))
      (spy-on 'projectile-cache-files-find-file-hook
              :and-call-fake (track 'cache))
      (spy-on 'projectile-track-known-projects-find-file-hook
              :and-call-fake (track 'track))
      (spy-on 'projectile-update-mode-line
              :and-call-fake (track 'mode-line))))

  (it "runs every operation for local buffers"
    (let ((default-directory "/tmp/")
          (projectile-auto-update-cache t)
          (projectile-dynamic-mode-line t))
      (projectile-find-file-hook-function))
    (expect (memq 'limit called) :to-be-truthy)
    (expect (memq 'cache called) :to-be-truthy)
    (expect (memq 'track called) :to-be-truthy)
    (expect (memq 'mode-line called) :to-be-truthy))

  (it "runs cheap operations for remote buffers but skips the mode-line update"
    (let ((default-directory "/ssh:host:/proj/")
          (projectile-auto-update-cache t)
          (projectile-dynamic-mode-line t))
      (projectile-find-file-hook-function))
    ;; cheap ones still run
    (expect (memq 'limit called) :to-be-truthy)
    (expect (memq 'cache called) :to-be-truthy)
    (expect (memq 'track called) :to-be-truthy)
    ;; the slow one is skipped
    (expect (memq 'mode-line called) :not :to-be-truthy)))

(describe "projectile-ignored-buffer-p"
  (it "checks if buffer should be ignored"
    (let ((projectile-globally-ignored-buffers '("*nrepl messages*" "*something*")))
      (expect (projectile-ignored-buffer-p (get-buffer-create "*nrepl messages*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "*something*")) :to-be-truthy)
      (expect (projectile-ignored-buffer-p (get-buffer-create "test")) :not :to-be-truthy)))

  ;; The entries are matched with `string-match-p', so a buffer name has to
  ;; be spelled as a regexp - an unescaped `*scratch*' matched only by
  ;; accident (leading `*' literal, trailing `h*' a repetition).
  (it "ignores the buffers its default value names"
    (expect (projectile-ignored-buffer-p (get-buffer-create "*scratch*")) :to-be-truthy)
    (expect (projectile-ignored-buffer-p (get-buffer-create "*lsp-log*")) :to-be-truthy)
    (expect (projectile-ignored-buffer-p (get-buffer-create "scratch.el"))
            :not :to-be-truthy))

  (it "has a default value of valid regexps"
    (dolist (re projectile-globally-ignored-buffers)
      (expect (ignore-errors (string-match-p re "") t) :to-be-truthy))))

(describe "projectile-process-current-project-buffers-current"
  (it "expects projectile-process-current-project-buffers and
projectile-process-current-project-buffers-current to have similar behaviour"
    (projectile-test-with-sandbox
     (projectile-test-with-files
      ("projectA/"
       "projectA/.projectile"
       "projectA/bufferA"
       "projectA/fileA"
       "projectA/dirA/"
       "projectA/dirA/fileC")
      (let ((list-a '())
            (list-b '()))
        (projectile-process-current-project-buffers (lambda (b) (push b list-a)))
        (projectile-process-current-project-buffers-current (lambda () (push (current-buffer) list-b)))
        (expect list-a :to-equal list-b))))))

(describe "projectile-project-buffers"
          (it "return project buffers"
              (projectile-test-with-sandbox
               (projectile-test-with-files
                ("project1/"
                 "project1/.projectile"
                 "project1/foo")
                (cd "project1")
                (with-current-buffer (find-file-noselect "foo" t))
                (expect (length (projectile-project-buffers)) :to-equal 1)))))

(describe "projectile-buffer-killed-p"
  (it "kills every buffer with the kill-all filter"
    (let ((projectile-kill-buffers-filter 'kill-all))
      (with-temp-buffer
        (expect (projectile-buffer-killed-p (current-buffer)) :to-be-truthy))))

  (it "kills only file-visiting buffers with the kill-only-files filter"
    (let ((projectile-kill-buffers-filter 'kill-only-files))
      (with-temp-buffer
        (expect (projectile-buffer-killed-p (current-buffer)) :not :to-be-truthy)
        (setq buffer-file-name "/tmp/projectile-killtest")
        (expect (projectile-buffer-killed-p (current-buffer)) :to-be-truthy))))

  (it "honors a predicate function filter"
    (let ((projectile-kill-buffers-filter
           (lambda (buf) (string-match-p "keep" (buffer-name buf)))))
      (with-current-buffer (get-buffer-create "keep-me")
        (expect (projectile-buffer-killed-p (current-buffer)) :to-be-truthy))
      (with-current-buffer (get-buffer-create "drop-me")
        (expect (projectile-buffer-killed-p (current-buffer)) :not :to-be-truthy))
      (kill-buffer "keep-me")
      (kill-buffer "drop-me")))

  (it "signals a user-error for an invalid filter value"
    (let ((projectile-kill-buffers-filter 42))
      (with-temp-buffer
        (expect (projectile-buffer-killed-p (current-buffer)) :to-throw 'user-error)))))

(describe "projectile--buffer-matches-conditions"
  (it "matches a buffer-name regexp condition"
    (with-current-buffer (get-buffer-create "*scratch-test*")
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '("\\`\\*scratch-test\\*\\'"))
              :to-be-truthy)
      (kill-buffer)))

  (it "matches a predicate-function condition"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/projectile-cond")
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '(buffer-file-name))
              :to-be-truthy)))

  (it "matches major-mode and derived-mode conditions"
    (with-temp-buffer
      (lisp-mode)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((major-mode . lisp-mode)))
              :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((major-mode . text-mode)))
              :not :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((derived-mode . prog-mode)))
              :to-be-truthy)))

  (it "composes conditions with and/or/not"
    (with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/projectile-compose")
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((and buffer-file-name (derived-mode . text-mode))))
              :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((and buffer-file-name (derived-mode . prog-mode))))
              :not :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((or (derived-mode . prog-mode) (derived-mode . text-mode))))
              :to-be-truthy)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((not (derived-mode . prog-mode))))
              :to-be-truthy)))

  (it "returns nil when no condition matches and for an empty list"
    (with-temp-buffer
      (fundamental-mode)
      (expect (projectile--buffer-matches-conditions
               (current-buffer) '((derived-mode . prog-mode)))
              :not :to-be-truthy)
      (expect (projectile--buffer-matches-conditions (current-buffer) nil)
              :not :to-be-truthy))))

(describe "projectile-project-buffer-p"
  (it "uses the truename cache when provided"
    (let* ((project-root "/projects/foo/")
           (cache (make-hash-table :test 'equal)))
      (spy-on 'file-truename :and-call-fake (lambda (f) f))
      (with-temp-buffer
        (setq default-directory "/projects/foo/src/")
        (rename-buffer "test-buffer")
        (projectile-project-buffer-p (current-buffer) project-root cache)
        ;; The truename result should be cached
        (expect (gethash "/projects/foo/src/" cache) :to-equal "/projects/foo/src/")
        ;; A second call should use the cache, not call file-truename again
        (projectile-project-buffer-p (current-buffer) project-root cache)
        (expect 'file-truename :to-have-been-called-times 1))))

  (it "skips file-truename for buffers visiting remote files"
    ;; Each `file-truename' on a TRAMP path is a remote stat;
    ;; iterating `(buffer-list)' for a remote project should not
    ;; trigger any of them.
    (spy-on 'file-truename :and-call-fake (lambda (f) f))
    (with-temp-buffer
      (setq default-directory "/ssh:host:/proj/src/")
      (rename-buffer "remote-test-buffer")
      (expect (projectile-project-buffer-p (current-buffer) "/ssh:host:/proj/" nil)
              :to-be-truthy)
      (expect 'file-truename :not :to-have-been-called)))

  (it "does not match a remote buffer against a local project root"
    (with-temp-buffer
      (setq default-directory "/ssh:host:/proj/src/")
      (rename-buffer "remote-test-buffer-2")
      (expect (projectile-project-buffer-p (current-buffer) "/local/proj/" nil)
              :not :to-be-truthy))))

;; A bunch of tests that make sure Projectile commands handle
;; gracefully the case of being run outside of a project.

;;; The buffer commands themselves

(describe "projectile-kill-buffers"
  (it "kills the project's buffers once confirmed"
    (projectile-test-with-project
        (("a.txt" . "x") ("b.txt" . "y"))
      (let ((ba (find-file-noselect (expand-file-name "a.txt")))
            (bb (find-file-noselect (expand-file-name "b.txt"))))
        (spy-on 'yes-or-no-p :and-return-value t)
        (projectile-kill-buffers)
        (expect (buffer-live-p ba) :to-be nil)
        (expect (buffer-live-p bb) :to-be nil))))

  (it "kills nothing when the confirmation is declined"
    (projectile-test-with-project
        (("a.txt" . "x"))
      (let ((ba (find-file-noselect (expand-file-name "a.txt"))))
        (spy-on 'yes-or-no-p :and-return-value nil)
        (projectile-kill-buffers)
        (expect (buffer-live-p ba) :to-be-truthy))))

  (it "counts the buffers it is about to kill in the prompt"
    (projectile-test-with-project
        (("a.txt" . "x") ("b.txt" . "y"))
      (find-file-noselect (expand-file-name "a.txt"))
      (find-file-noselect (expand-file-name "b.txt"))
      (spy-on 'yes-or-no-p :and-return-value nil)
      (projectile-kill-buffers)
      (expect (car (spy-calls-args-for 'yes-or-no-p 0)) :to-match "kill 2 buffers"))))

(describe "projectile-next-project-buffer"
  (it "keeps calling next-buffer until it lands on another project buffer"
    (projectile-test-with-project
        (("a.txt" . "x") ("b.txt" . "y"))
      (let* ((ba (find-file-noselect (expand-file-name "a.txt")))
             (bb (find-file-noselect (expand-file-name "b.txt")))
             (visited nil))
        (spy-on 'projectile-project-buffers :and-return-value (list ba bb))
        ;; `save-current-buffer' matters: the command works on whatever buffer
        ;; is current, and leaving a foreign one current leaks into every spec
        ;; that runs afterwards.
        (save-current-buffer
          (set-buffer ba)
          ;; a stand-in for `next-buffer': step through a fixed rotation that
          ;; passes an unrelated buffer before reaching the project's other one
          (let ((rotation (list (get-buffer-create "*unrelated*") bb)))
            (cl-letf (((symbol-function 'next-buffer)
                       (lambda (&rest _)
                         (let ((next (or (pop rotation) bb)))
                           (push next visited)
                           (set-buffer next)))))
              (projectile-next-project-buffer)))
          (expect (current-buffer) :to-be bb))
        (kill-buffer "*unrelated*")
        ;; it did not stop at the unrelated buffer on the way
        (expect (length visited) :to-equal 2))))

  (it "falls back to plain next-buffer outside a project"
    (spy-on 'projectile-project-root :and-return-value nil)
    (spy-on 'next-buffer)
    (projectile-next-project-buffer)
    (expect 'next-buffer :to-have-been-called)))

(describe "projectile-previous-project-buffer"
  (it "walks the other way, through the same repeat-until helper"
    (spy-on 'projectile--repeat-until-project-buffer)
    (projectile-previous-project-buffer)
    (expect 'projectile--repeat-until-project-buffer
            :to-have-been-called-with #'previous-buffer))

  (it "falls back to plain previous-buffer outside a project"
    (spy-on 'projectile-project-root :and-return-value nil)
    (spy-on 'previous-buffer)
    (projectile-previous-project-buffer)
    (expect 'previous-buffer :to-have-been-called)))

;;; projectile-buffers-test.el ends here
